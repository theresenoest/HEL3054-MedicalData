# ============================================================
# 99_solution_pipeline.R
#
# ONE solution pipeline, noe the only one and most likely not the perfect one, but ONE.

# Validating and using ischaemic heart disease data across
# records, registries, and time.
#
# Remember: All data are synthetic and for teaching purposes only.
# ============================================================

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(here)

dir.create(here("data", "derived"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Helper functions
# ------------------------------------------------------------

# Standardise diagnosis codes:
# I25.1 -> I251
# 414.01 -> 41401
clean_code <- function(x) {
  x |>
    str_to_upper() |>
    str_trim() |>
    str_remove_all("\\.")
}

# Parse deliberately mixed date formats in the synthetic data.
# Incomplete dates such as 2014-??-12 become NA.
parse_mixed_date <- function(x) {
  
  # Convert dates or other input types safely to character first
  x <- as.character(x)
  
  # Convert empty strings to missing values
  x <- na_if(x, "")
  
  suppressWarnings(
    coalesce(
      lubridate::ymd(x, quiet = TRUE),
      lubridate::dmy(x, quiet = TRUE),
      lubridate::mdy(x, quiet = TRUE)
    )
  )
}


# Calculate validation measures from logical variables.
calculate_validation_metrics <- function(data, registry_positive,
                                         chart_positive) {
  
  registry_positive <- data[[registry_positive]]
  chart_positive <- data[[chart_positive]]
  
  true_positive <- sum(registry_positive & chart_positive, na.rm = TRUE)
  false_positive <- sum(registry_positive & !chart_positive, na.rm = TRUE)
  false_negative <- sum(!registry_positive & chart_positive, na.rm = TRUE)
  true_negative <- sum(!registry_positive & !chart_positive, na.rm = TRUE)
  
  tibble(
    true_positive = true_positive,
    false_positive = false_positive,
    false_negative = false_negative,
    true_negative = true_negative,
    ppv = if_else(
      true_positive + false_positive > 0,
      true_positive / (true_positive + false_positive),
      NA_real_
    ),
    sensitivity = if_else(
      true_positive + false_negative > 0,
      true_positive / (true_positive + false_negative),
      NA_real_
    ),
    specificity = if_else(
      true_negative + false_positive > 0,
      true_negative / (true_negative + false_positive),
      NA_real_
    ),
    npv = if_else(
      true_negative + false_negative > 0,
      true_negative / (true_negative + false_negative),
      NA_real_
    )
  )
}

# ------------------------------------------------------------
# 2. Read raw data
# ------------------------------------------------------------

cohort <- read_csv(
  here("data", "raw", "cohort_baseline.csv"),
  show_col_types = FALSE
)

contacts <- read_csv(
  here("data", "raw", "registry_contacts.csv"),
  show_col_types = FALSE
)

diagnoses <- read_csv(
  here("data", "raw", "registry_diagnoses.csv"),
  show_col_types = FALSE
)

procedures <- read_csv(
  here("data", "raw", "procedures.csv"),
  show_col_types = FALSE
)

prescriptions <- read_csv(
  here("data", "raw", "prescriptions.csv"),
  show_col_types = FALSE
)

notes <- read_csv(
  here("data", "raw", "emr_notes_excerpt.csv"),
  show_col_types = FALSE
)

review <- read_csv(
  here("data", "raw", "emr_review.csv"),
  show_col_types = FALSE
)

followup <- read_csv(
  here("data", "raw", "death_followup.csv"),
  show_col_types = FALSE
)

icd_lookup <- read_csv(
  here("data", "raw", "icd_harmonisation_lookup.csv"),
  show_col_types = FALSE
)

snomed_lookup <- read_csv(
  here("data", "raw", "snomed_lookup.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 3. Clean cohort and follow-up data
# ------------------------------------------------------------

cohort_clean <- cohort |>
  mutate(
    baseline_date = parse_mixed_date(baseline_date),
    age_at_baseline = year(baseline_date) - birth_year,
    self_reported_prior_ihd =
      self_reported_prior_ihd == "Yes"
  )

followup_clean <- followup |>
  mutate(
    death_date = parse_mixed_date(death_date_raw),
    administrative_end_followup =
      parse_mixed_date(administrative_end_followup_raw)
  )

# ------------------------------------------------------------
# 4. Clean contact-level data
# ------------------------------------------------------------

contacts_clean <- contacts |>
  mutate(
    admission_date = parse_mixed_date(admission_date_raw),
    discharge_date = parse_mixed_date(discharge_date_raw),
    contact_date = parse_mixed_date(contact_date_raw),
    
    # A practical event date:
    # inpatient: admission date where available
    # outpatient: contact date where available
    # fallback: discharge date
    contact_event_date = coalesce(
      admission_date,
      contact_date,
      discharge_date
    ),
    
    # Flag intentionally problematic records.
    admission_date_missing_or_invalid =
      !is.na(admission_date_raw) & is.na(admission_date),
    
    discharge_before_admission =
      !is.na(admission_date) &
      !is.na(discharge_date) &
      discharge_date < admission_date
  )

# ------------------------------------------------------------
# 5. Clean diagnosis-level data and harmonise ICD codes
# ------------------------------------------------------------

icd_lookup_clean <- icd_lookup |>
  mutate(
    code_standardised = clean_code(code_raw)
  )

diagnoses_clean <- diagnoses |>
  mutate(
    diagnosis_date = parse_mixed_date(diagnosis_date_raw),
    code_standardised = clean_code(diagnosis_code)
  ) |>
  left_join(
    icd_lookup_clean |>
      select(
        coding_system,
        code_standardised,
        clinical_group,
        include_broad_ihd,
        include_strict_ihd,
        code_comments = comments
      ),
    by = c("coding_system", "code_standardised")
  ) |>
  mutate(
    include_broad_ihd = replace_na(include_broad_ihd, FALSE),
    include_strict_ihd = replace_na(include_strict_ihd, FALSE)
  )

# ------------------------------------------------------------
# 6. Create an event-review dataset
#
# One row = one diagnosis record linked to its healthcare contact.
# This is useful for reviewing phenotype decisions.
# ------------------------------------------------------------

event_review <- diagnoses_clean |>
  left_join(
    contacts_clean |>
      select(
        contact_id,
        person_id,
        hospital,
        department,
        setting,
        admission_date,
        discharge_date,
        contact_date,
        contact_event_date,
        admission_date_missing_or_invalid,
        discharge_before_admission
      ),
    by = c("contact_id", "person_id")
  ) |>
  mutate(
    # Prefer a contact-based date. If unavailable, use diagnosis date.
    event_date = coalesce(contact_event_date, diagnosis_date),
    
    # Broad IHD: any diagnosis mapped to IHD in lookup.
    broad_ihd = include_broad_ihd,
    
    # Strict rule part 1:
    # IHD code as primary diagnosis during inpatient admission.
    inpatient_primary_ihd =
      broad_ihd &
      setting == "Inpatient" &
      diagnosis_position == "Primary",
    
    # Secondary IHD diagnosis:
    secondary_ihd =
      broad_ihd &
      diagnosis_position == "Secondary"
  ) |>
  arrange(person_id, event_date, diagnosis_id)

write_csv(
  event_review,
  here("data", "derived", "event_review_diagnosis_level.csv")
)

# ------------------------------------------------------------
# 7. Clean procedure and prescription data
# ------------------------------------------------------------

procedures_clean <- procedures |>
  mutate(
    procedure_date = parse_mixed_date(procedure_date_raw),
    coronary_procedure =
      procedure_code %in% c("CAG", "PCI", "CABG")
  )

prescriptions_clean <- prescriptions |>
  mutate(
    dispensing_date = parse_mixed_date(dispensing_date_raw),
    statin = medicine_group == "Statin",
    antiplatelet = medicine_group == "Antiplatelet"
  )

# ------------------------------------------------------------
# 8. Create phenotype components
# ------------------------------------------------------------

# A. Any broad IHD diagnosis.
broad_ihd_people <- event_review |>
  filter(broad_ihd) |>
  group_by(person_id) |>
  summarise(
    any_broad_ihd_code = TRUE,
    n_broad_ihd_diagnoses = n(),
    n_broad_ihd_contacts = n_distinct(contact_id),
    .groups = "drop"
  )

# B. Inpatient primary IHD diagnosis.
inpatient_primary_people <- event_review |>
  filter(inpatient_primary_ihd) |>
  group_by(person_id) |>
  summarise(
    any_inpatient_primary_ihd = TRUE,
    n_inpatient_primary_ihd = n(),
    .groups = "drop"
  )

# C. Repeated outpatient IHD contacts.
# Here, repeated means at least two distinct outpatient contact dates.
outpatient_ihd_people <- event_review |>
  filter(
    broad_ihd,
    setting == "Outpatient"
  ) |>
  distinct(person_id, event_date) |>
  group_by(person_id) |>
  summarise(
    n_outpatient_ihd_dates = n(),
    repeated_outpatient_ihd = n_outpatient_ihd_dates >= 2,
    .groups = "drop"
  )

# D. Coronary procedures.
coronary_procedure_people <- procedures_clean |>
  filter(coronary_procedure) |>
  group_by(person_id) |>
  summarise(
    any_coronary_procedure = TRUE,
    first_coronary_procedure_date = min(procedure_date, na.rm = TRUE),
    procedure_types = paste(sort(unique(procedure_code)), collapse = "; "),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 9. Create broad and strict person-level registry algorithms
# ------------------------------------------------------------

phenotype_people <- cohort_clean |>
  select(person_id, baseline_date, self_reported_prior_ihd) |>
  left_join(broad_ihd_people, by = "person_id") |>
  left_join(inpatient_primary_people, by = "person_id") |>
  left_join(outpatient_ihd_people, by = "person_id") |>
  left_join(coronary_procedure_people, by = "person_id") |>
  mutate(
    any_broad_ihd_code = replace_na(any_broad_ihd_code, FALSE),
    n_broad_ihd_diagnoses = replace_na(n_broad_ihd_diagnoses, 0L),
    n_broad_ihd_contacts = replace_na(n_broad_ihd_contacts, 0L),
    any_inpatient_primary_ihd =
      replace_na(any_inpatient_primary_ihd, FALSE),
    n_inpatient_primary_ihd =
      replace_na(n_inpatient_primary_ihd, 0L),
    n_outpatient_ihd_dates =
      replace_na(n_outpatient_ihd_dates, 0L),
    repeated_outpatient_ihd =
      replace_na(repeated_outpatient_ihd, FALSE),
    any_coronary_procedure =
      replace_na(any_coronary_procedure, FALSE),
    
    # Algorithm 1: broad registry IHD.
    registry_ihd_broad = any_broad_ihd_code,
    
    # Algorithm 2: strict registry IHD.
    # Inpatient primary IHD OR repeated outpatient IHD.
    registry_ihd_strict =
      any_inpatient_primary_ihd |
      repeated_outpatient_ihd,
    
    # Algorithm 3: strict + procedure support.
    # Requires broad diagnosis plus either:
    # inpatient-primary diagnosis, repeated outpatient diagnoses,
    # or a relevant coronary procedure.
    registry_ihd_strict_procedure =
      any_broad_ihd_code &
      (
        any_inpatient_primary_ihd |
          repeated_outpatient_ihd |
          any_coronary_procedure
      )
  )

write_csv(
  phenotype_people,
  here("data", "derived", "person_level_ihd_algorithms.csv")
)

# ------------------------------------------------------------
# 10. Create first registry-recorded IHD dates
# ------------------------------------------------------------

first_broad_registry_event <- event_review |>
  filter(broad_ihd, !is.na(event_date)) |>
  group_by(person_id) |>
  slice_min(event_date, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    person_id,
    first_broad_registry_ihd_date = event_date,
    first_broad_registry_ihd_code = diagnosis_code,
    first_broad_registry_setting = setting,
    first_broad_registry_hospital = hospital,
    first_broad_registry_position = diagnosis_position
  )

first_strict_registry_event <- event_review |>
  filter(inpatient_primary_ihd, !is.na(event_date)) |>
  group_by(person_id) |>
  slice_min(event_date, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    person_id,
    first_strict_inpatient_ihd_date = event_date,
    first_strict_inpatient_ihd_code = diagnosis_code,
    first_strict_inpatient_ihd_hospital = hospital
  )

# Repeated outpatient rule: event date = second qualifying outpatient date.
second_outpatient_event <- event_review |>
  filter(broad_ihd, setting == "Outpatient", !is.na(event_date)) |>
  distinct(person_id, event_date) |>
  arrange(person_id, event_date) |>
  group_by(person_id) |>
  mutate(outpatient_event_number = row_number()) |>
  filter(outpatient_event_number == 2) |>
  ungroup() |>
  transmute(
    person_id,
    second_outpatient_ihd_date = event_date
  )

# First strict event is either:
# - first inpatient primary IHD date
# - second outpatient IHD date
first_strict_algorithm_event <- full_join(
  first_strict_registry_event,
  second_outpatient_event,
  by = "person_id"
) |>
  mutate(
    first_strict_registry_ihd_date = pmin(
      first_strict_inpatient_ihd_date,
      second_outpatient_ihd_date,
      na.rm = TRUE
    ),
    first_strict_registry_ihd_date = if_else(
      is.infinite(first_strict_registry_ihd_date),
      as.Date(NA),
      first_strict_registry_ihd_date
    )
  ) |>
  select(person_id, first_strict_registry_ihd_date)

# ------------------------------------------------------------
# 11. Clean and prepare chart-review information
# ------------------------------------------------------------

review_clean <- review |>
  mutate(
    clinical_event_date = parse_mixed_date(clinical_event_date_raw),
    
    # Main teaching definition:
    # "Yes" and "Probable" are both considered chart-review positive.
    chart_ihd_positive =
      chart_review_ihd %in% c("Yes", "Probable"),
    
    # A stricter sensitivity analysis:
    chart_ihd_definite =
      chart_review_ihd == "Yes"
  )

# ------------------------------------------------------------
# 12. Validation dataset and validation results
# ------------------------------------------------------------

validation_data <- review_clean |>
  select(
    person_id,
    chart_review_ihd,
    reviewer_certainty,
    chart_ihd_positive,
    chart_ihd_definite,
    clinical_event_date,
    review_comment
  ) |>
  left_join(
    phenotype_people |>
      select(
        person_id,
        registry_ihd_broad,
        registry_ihd_strict,
        registry_ihd_strict_procedure,
        any_inpatient_primary_ihd,
        repeated_outpatient_ihd,
        any_coronary_procedure
      ),
    by = "person_id"
  ) |>
  mutate(
    across(
      starts_with("registry_ihd"),
      ~ replace_na(.x, FALSE)
    ),
    any_inpatient_primary_ihd =
      replace_na(any_inpatient_primary_ihd, FALSE),
    repeated_outpatient_ihd =
      replace_na(repeated_outpatient_ihd, FALSE),
    any_coronary_procedure =
      replace_na(any_coronary_procedure, FALSE)
  )

write_csv(
  validation_data,
  here("data", "derived", "validation_dataset.csv")
)

validation_results <- bind_rows(
  calculate_validation_metrics(
    validation_data,
    registry_positive = "registry_ihd_broad",
    chart_positive = "chart_ihd_positive"
  ) |>
    mutate(algorithm = "Broad: any IHD code"),
  
  calculate_validation_metrics(
    validation_data,
    registry_positive = "registry_ihd_strict",
    chart_positive = "chart_ihd_positive"
  ) |>
    mutate(algorithm = "Strict: inpatient primary OR repeated outpatient"),
  
  calculate_validation_metrics(
    validation_data,
    registry_positive = "registry_ihd_strict_procedure",
    chart_positive = "chart_ihd_positive"
  ) |>
    mutate(
      algorithm =
        "Strict + procedure: diagnosis plus inpatient/repeated/procedure evidence"
    )
) |>
  select(
    algorithm,
    true_positive,
    false_positive,
    false_negative,
    true_negative,
    ppv,
    sensitivity,
    specificity,
    npv
  ) |>
  mutate(
    across(
      c(ppv, sensitivity, specificity, npv),
      ~ round(.x, 3)
    )
  )

write_csv(
  validation_results,
  here("outputs", "tables", "validation_results.csv")
)

# ------------------------------------------------------------
# 13. Stratified PPV examples
# ------------------------------------------------------------

# Attach registry setting and position to broad IHD records.
broad_ihd_records_for_validation <- event_review |>
  filter(broad_ihd) |>
  group_by(person_id) |>
  slice_min(event_date, with_ties = FALSE) |>
  ungroup() |>
  select(
    person_id,
    hospital,
    setting,
    diagnosis_position,
    diagnosis_code,
    event_date
  ) |>
  left_join(
    review_clean |>
      select(person_id, chart_ihd_positive),
    by = "person_id"
  )

ppv_by_setting <- broad_ihd_records_for_validation |>
  filter(!is.na(chart_ihd_positive)) |>
  group_by(setting) |>
  summarise(
    registry_positive_records = n(),
    chart_confirmed = sum(chart_ihd_positive),
    ppv = chart_confirmed / registry_positive_records,
    .groups = "drop"
  )

ppv_by_position <- broad_ihd_records_for_validation |>
  filter(!is.na(chart_ihd_positive)) |>
  group_by(diagnosis_position) |>
  summarise(
    registry_positive_records = n(),
    chart_confirmed = sum(chart_ihd_positive),
    ppv = chart_confirmed / registry_positive_records,
    .groups = "drop"
  )

write_csv(
  ppv_by_setting,
  here("outputs", "tables", "ppv_by_setting.csv")
)

write_csv(
  ppv_by_position,
  here("outputs", "tables", "ppv_by_diagnosis_position.csv")
)

# ------------------------------------------------------------
# 14. Identify possible prevalent IHD before baseline
# ------------------------------------------------------------

# Registry evidence before baseline.
prevalent_registry <- event_review |>
  filter(broad_ihd, !is.na(event_date)) |>
  left_join(
    cohort_clean |>
      select(person_id, baseline_date),
    by = "person_id"
  ) |>
  filter(event_date < baseline_date) |>
  group_by(person_id) |>
  summarise(
    prevalent_ihd_registry = TRUE,
    first_prebaseline_registry_ihd_date = min(event_date),
    .groups = "drop"
  )

# Procedure evidence before baseline.
prevalent_procedure <- procedures_clean |>
  filter(coronary_procedure, !is.na(procedure_date)) |>
  left_join(
    cohort_clean |>
      select(person_id, baseline_date),
    by = "person_id"
  ) |>
  filter(procedure_date < baseline_date) |>
  group_by(person_id) |>
  summarise(
    prevalent_ihd_procedure = TRUE,
    first_prebaseline_coronary_procedure_date = min(procedure_date),
    .groups = "drop"
  )

# Chart-review clinical date before baseline.
prevalent_chart_review <- review_clean |>
  filter(chart_ihd_positive, !is.na(clinical_event_date)) |>
  left_join(
    cohort_clean |>
      select(person_id, baseline_date),
    by = "person_id"
  ) |>
  filter(clinical_event_date < baseline_date) |>
  group_by(person_id) |>
  summarise(
    prevalent_ihd_chart_review = TRUE,
    first_prebaseline_clinical_date = min(clinical_event_date),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 15. First-event summary dataset
#
# This is a teaching summary rather than a full epidemiological
# analysis dataset. It makes the first-event decisions visible.
# ------------------------------------------------------------

first_event_summary <- cohort_clean |>
  left_join(followup_clean, by = "person_id") |>
  left_join(phenotype_people, by = c(
    "person_id",
    "baseline_date",
    "self_reported_prior_ihd"
  )) |>
  left_join(first_broad_registry_event, by = "person_id") |>
  left_join(first_strict_algorithm_event, by = "person_id") |>
  left_join(prevalent_registry, by = "person_id") |>
  left_join(prevalent_procedure, by = "person_id") |>
  left_join(prevalent_chart_review, by = "person_id") |>
  left_join(
    review_clean |>
      select(
        person_id,
        chart_review_ihd,
        reviewer_certainty,
        clinical_event_date,
        chart_ihd_positive
      ),
    by = "person_id"
  ) |>
  mutate(
    prevalent_ihd_registry =
      replace_na(prevalent_ihd_registry, FALSE),
    prevalent_ihd_procedure =
      replace_na(prevalent_ihd_procedure, FALSE),
    prevalent_ihd_chart_review =
      replace_na(prevalent_ihd_chart_review, FALSE),
    
    # Teaching definition of possible prevalent IHD:
    # self-report OR prebaseline registry record OR
    # prebaseline coronary procedure OR prebaseline chart evidence.
    possible_prevalent_ihd =
      self_reported_prior_ihd |
      prevalent_ihd_registry |
      prevalent_ihd_procedure |
      prevalent_ihd_chart_review,
    
    censor_date = coalesce(
      death_date,
      administrative_end_followup
    ),
    
    broad_first_event_after_baseline =
      !possible_prevalent_ihd &
      !is.na(first_broad_registry_ihd_date) &
      first_broad_registry_ihd_date >= baseline_date &
      first_broad_registry_ihd_date <= censor_date,
    
    strict_first_event_after_baseline =
      !possible_prevalent_ihd &
      !is.na(first_strict_registry_ihd_date) &
      first_strict_registry_ihd_date >= baseline_date &
      first_strict_registry_ihd_date <= censor_date,
    
    broad_followup_days = as.numeric(
      coalesce(first_broad_registry_ihd_date, censor_date) -
        baseline_date
    ),
    
    strict_followup_days = as.numeric(
      coalesce(first_strict_registry_ihd_date, censor_date) -
        baseline_date
    ),
    
    broad_followup_years = round(broad_followup_days / 365.25, 2),
    strict_followup_years = round(strict_followup_days / 365.25, 2)
  ) |>
  select(
    person_id,
    baseline_date,
    birth_year,
    age_at_baseline,
    sex,
    education,
    smoking_status,
    physical_activity,
    bmi_kg_m2,
    systolic_bp_mmHg,
    total_cholesterol_mmol_l,
    diabetes_baseline,
    self_reported_prior_ihd,
    possible_prevalent_ihd,
    prevalent_ihd_registry,
    prevalent_ihd_procedure,
    prevalent_ihd_chart_review,
    first_broad_registry_ihd_date,
    first_broad_registry_ihd_code,
    first_broad_registry_setting,
    first_broad_registry_hospital,
    first_broad_registry_position,
    first_strict_registry_ihd_date,
    broad_first_event_after_baseline,
    strict_first_event_after_baseline,
    death_date,
    cause_of_death_group,
    administrative_end_followup,
    censor_date,
    broad_followup_days,
    broad_followup_years,
    strict_followup_days,
    strict_followup_years,
    chart_review_ihd,
    reviewer_certainty,
    clinical_event_date
  )

write_csv(
  first_event_summary,
  here("data", "derived", "first_ihd_event_summary.csv")
)

# ------------------------------------------------------------
# 16. Create a teaching-case summary
# ------------------------------------------------------------

teaching_case_ids <- c(
  "P001", "P002", "P003", "P004", "P005",
  "P006", "P007", "P010", "P011", "P013",
  "P014", "P016", "P017", "P018", "P020"
)

teaching_case_summary <- first_event_summary |>
  filter(person_id %in% teaching_case_ids) |>
  left_join(
    phenotype_people |>
      select(
        person_id,
        registry_ihd_broad,
        registry_ihd_strict,
        registry_ihd_strict_procedure,
        n_broad_ihd_diagnoses,
        n_outpatient_ihd_dates,
        any_coronary_procedure
      ),
    by = "person_id"
  ) |>
  arrange(person_id)

write_csv(
  teaching_case_summary,
  here("outputs", "tables", "teaching_case_summary.csv")
)

# ------------------------------------------------------------
# 17. Data-quality summary tables
# ------------------------------------------------------------

contact_quality_summary <- contacts_clean |>
  summarise(
    n_contacts = n(),
    invalid_or_incomplete_admission_dates =
      sum(admission_date_missing_or_invalid),
    discharge_before_admission =
      sum(discharge_before_admission),
    missing_contact_event_date =
      sum(is.na(contact_event_date))
  )

diagnosis_quality_summary <- diagnoses_clean |>
  summarise(
    n_diagnoses = n(),
    missing_or_unparseable_diagnosis_date =
      sum(!is.na(diagnosis_date_raw) & is.na(diagnosis_date)),
    diagnoses_without_lookup_match =
      sum(is.na(clinical_group))
  )

write_csv(
  contact_quality_summary,
  here("outputs", "tables", "contact_data_quality_summary.csv")
)

write_csv(
  diagnosis_quality_summary,
  here("outputs", "tables", "diagnosis_data_quality_summary.csv")
)

# ------------------------------------------------------------
# 18. Simple descriptive outputs for teaching
# ------------------------------------------------------------

phenotype_count_summary <- first_event_summary |>
  summarise(
    cohort_participants = n(),
    possible_prevalent_ihd = sum(possible_prevalent_ihd),
    broad_first_events = sum(broad_first_event_after_baseline),
    strict_first_events = sum(strict_first_event_after_baseline)
  )

write_csv(
  phenotype_count_summary,
  here("outputs", "tables", "phenotype_count_summary.csv")
)

# ------------------------------------------------------------
# 19. Console summary
# ------------------------------------------------------------

cat("\n========================================\n")
cat("Solution pipeline completed successfully.\n")
cat("========================================\n\n")

cat("Created derived datasets:\n")
cat("- data/derived/event_review_diagnosis_level.csv\n")
cat("- data/derived/person_level_ihd_algorithms.csv\n")
cat("- data/derived/validation_dataset.csv\n")
cat("- data/derived/first_ihd_event_summary.csv\n\n")

cat("Created output tables:\n")
cat("- outputs/tables/validation_results.csv\n")
cat("- outputs/tables/ppv_by_setting.csv\n")
cat("- outputs/tables/ppv_by_diagnosis_position.csv\n")
cat("- outputs/tables/teaching_case_summary.csv\n")
cat("- outputs/tables/contact_data_quality_summary.csv\n")
cat("- outputs/tables/diagnosis_data_quality_summary.csv\n")
cat("- outputs/tables/phenotype_count_summary.csv\n\n")

cat("Validation results:\n")
print(validation_results)

cat("\nPhenotype count summary:\n")
print(phenotype_count_summary)