library(tidyverse)
library(lubridate)
library(here)

source(here("R", "01_helpers.R"))

cohort <- read_csv(here("data", "raw", "cohort_baseline.csv"), show_col_types = FALSE) |>
  mutate(baseline_date = ymd(baseline_date))

contacts <- read_csv(here("data", "raw", "registry_contacts.csv"), show_col_types = FALSE) |>
  mutate(
    admission_date = parse_mixed_date(admission_date_raw),
    discharge_date = parse_mixed_date(discharge_date_raw),
    contact_date = parse_mixed_date(contact_date_raw)
  )

diagnoses <- read_csv(here("data", "raw", "registry_diagnoses.csv"), show_col_types = FALSE) |>
  mutate(
    diagnosis_date = parse_mixed_date(diagnosis_date_raw),
    code_standardised = clean_code(diagnosis_code)
  )

lookup <- read_csv(
  here("data", "raw", "icd_harmonisation_lookup.csv"),
  show_col_types = FALSE
)

procedures <- read_csv(here("data", "raw", "procedures.csv"), show_col_types = FALSE) |>
  mutate(procedure_date = parse_mixed_date(procedure_date_raw))

followup <- read_csv(
  here("data", "raw", "death_followup.csv"),
  show_col_types = FALSE
) |>
  mutate(
    death_date = parse_mixed_date(death_date_raw),
    administrative_end_followup = ymd(administrative_end_followup_raw)
  )

# Diagnosis-level file enriched with contact and phenotype information
diagnosis_events <- diagnoses |>
  left_join(
    contacts |>
      select(
        contact_id, person_id, hospital, department, setting,
        admission_date, discharge_date, contact_date
      ),
    by = c("contact_id", "person_id")
  ) |>
  left_join(
    lookup |>
      select(
        coding_system, code_standardised,
        clinical_group, include_broad_ihd, include_strict_ihd
      ),
    by = c("coding_system", "code_standardised")
  ) |>
  mutate(
    event_date = coalesce(admission_date, contact_date, diagnosis_date),
    broad_ihd = replace_na(include_broad_ihd, FALSE),
    strict_ihd = case_when(
      broad_ihd &
        setting == "Inpatient" &
        diagnosis_position == "Primary" ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Add repeated outpatient IHD rule
outpatient_counts <- diagnosis_events |>
  filter(broad_ihd, setting == "Outpatient") |>
  distinct(person_id, event_date) |>
  count(person_id, name = "n_outpatient_ihd_dates")

diagnosis_events <- diagnosis_events |>
  left_join(outpatient_counts, by = "person_id") |>
  mutate(
    strict_ihd = strict_ihd |
      (setting == "Outpatient" & replace_na(n_outpatient_ihd_dates, 0) >= 2)
  )

# First broad and strict event after baseline
first_events <- diagnosis_events |>
  inner_join(
    cohort |>
      select(person_id, baseline_date, self_reported_prior_ihd),
    by = "person_id"
  ) |>
  filter(event_date >= baseline_date) |>
  group_by(person_id) |>
  summarise(
    broad_first_ihd_date = min(event_date[broad_ihd], na.rm = TRUE),
    strict_first_ihd_date = min(event_date[strict_ihd], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    broad_first_ihd_date = if_else(
      is.infinite(broad_first_ihd_date),
      as.Date(NA),
      broad_first_ihd_date
    ),
    strict_first_ihd_date = if_else(
      is.infinite(strict_first_ihd_date),
      as.Date(NA),
      strict_first_ihd_date
    )
  )

analysis_ready <- cohort |>
  left_join(first_events, by = "person_id") |>
  left_join(followup, by = "person_id") |>
  mutate(
    censor_date = pmin(
      administrative_end_followup,
      death_date,
      na.rm = TRUE
    ),
    broad_event = !is.na(broad_first_ihd_date) &
      broad_first_ihd_date <= censor_date,
    strict_event = !is.na(strict_first_ihd_date) &
      strict_first_ihd_date <= censor_date,
    followup_days_broad = as.numeric(
      coalesce(broad_first_ihd_date, censor_date) - baseline_date
    ),
    followup_days_strict = as.numeric(
      coalesce(strict_first_ihd_date, censor_date) - baseline_date
    )
  )

write_csv(
  diagnosis_events,
  here("data", "derived", "diagnosis_events_clean.csv")
)

write_csv(
  analysis_ready,
  here("data", "derived", "first_ihd_analysis_ready.csv")
)