library(tidyverse)
library(lubridate)
library(here)

source(here("R", "01_helpers.R"))

cohort <- read_csv(here("data", "raw", "cohort_baseline.csv"), show_col_types = FALSE)
contacts <- read_csv(here("data", "raw", "registry_contacts.csv"), show_col_types = FALSE)
diagnoses <- read_csv(here("data", "raw", "registry_diagnoses.csv"), show_col_types = FALSE)

# Check 1: one row per person in baseline cohort
cohort |>
  count(person_id) |>
  filter(n > 1)

# Check 2: contact IDs should be unique
contacts |>
  count(contact_id) |>
  filter(n > 1)

# Check 3: every diagnosis should link to a contact
diagnoses |>
  anti_join(contacts, by = c("contact_id", "person_id"))

# Check 4: identify raw date values that cannot be parsed
contacts |>
  mutate(
    admission_date = parse_mixed_date(admission_date_raw),
    discharge_date = parse_mixed_date(discharge_date_raw)
  ) |>
  filter(!is.na(admission_date_raw), is.na(admission_date)) |>
  select(contact_id, person_id, admission_date_raw)

# Check 5: discharge date before admission date
contacts |>
  mutate(
    admission_date = parse_mixed_date(admission_date_raw),
    discharge_date = parse_mixed_date(discharge_date_raw)
  ) |>
  filter(!is.na(admission_date), !is.na(discharge_date)) |>
  filter(discharge_date < admission_date)

# Check 6: standardise diagnosis codes
diagnoses |>
  mutate(diagnosis_code_standardised = clean_code(diagnosis_code)) |>
  select(diagnosis_code, diagnosis_code_standardised) |>
  distinct()
