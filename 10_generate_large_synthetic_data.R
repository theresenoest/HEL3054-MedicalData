# ============================================================
# 10_generate_large_synthetic_data.R
#
# Generate a large synthetic cohort for:
# - first IHD event analysis
# - Kaplan-Meier curves
# - Cox regression
#
# All data are fictional and for teaching purposes only.
# ============================================================

library(tidyverse)
library(lubridate)
library(here)

set.seed(3054)

# ------------------------------------------------------------
# 0. Folders
# ------------------------------------------------------------

dir.create(here("data", "large_raw"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Settings
# ------------------------------------------------------------

n_persons <- 5000

study_start <- as.Date("2005-01-01")
study_end <- as.Date("2021-12-31")

# ------------------------------------------------------------
# 2. Cohort baseline data
#
# One row = one cohort participant
# ------------------------------------------------------------

cohort_large <- tibble(
  person_id = sprintf("L%05d", 1:n_persons),
  
  baseline_date = sample(
    seq(as.Date("2005-01-01"), as.Date("2008-12-31"), by = "day"),
    size = n_persons,
    replace = TRUE
  ),
  
  birth_year = sample(
    1935:1975,
    size = n_persons,
    replace = TRUE
  ),
  
  sex = sample(
    c("Female", "Male"),
    size = n_persons,
    replace = TRUE,
    prob = c(0.52, 0.48)
  ),
  
  education = sample(
    c("Primary", "Secondary", "University"),
    size = n_persons,
    replace = TRUE,
    prob = c(0.25, 0.48, 0.27)
  ),
  
  smoking_status = sample(
    c("Never", "Former", "Current"),
    size = n_persons,
    replace = TRUE,
    prob = c(0.43, 0.34, 0.23)
  ),
  
  physical_activity = sample(
    c("Low", "Moderate", "High"),
    size = n_persons,
    replace = TRUE,
    prob = c(0.25, 0.50, 0.25)
  ),
  
  bmi_kg_m2 = round(
    pmin(pmax(rnorm(n_persons, mean = 27, sd = 4.5), 16), 52),
    1
  ),
  
  systolic_bp_mmHg = round(
    pmin(pmax(rnorm(n_persons, mean = 137, sd = 18), 85), 230)
  ),
  
  total_cholesterol_mmol_l = round(
    pmin(pmax(rnorm(n_persons, mean = 5.5, sd = 1.0), 2.5), 10),
    1
  ),
  
  diabetes_baseline = rbinom(
    n_persons,
    size = 1,
    prob = 0.12
  ),
  
  self_reported_prior_ihd = rbinom(
    n_persons,
    size = 1,
    prob = 0.06
  )
) |>
  mutate(
    age_at_baseline = year(baseline_date) - birth_year,
    
    diabetes_baseline = if_else(
      diabetes_baseline == 1,
      "Yes",
      "No"
    ),
    
    self_reported_prior_ihd = if_else(
      self_reported_prior_ihd == 1,
      "Yes",
      "No"
    )
  )

# ------------------------------------------------------------
# 3. Simulate latent/prevalent IHD before baseline
#
# This represents disease that occurred before baseline.
# ------------------------------------------------------------

prevalent_probability <- plogis(
  -7 +
    0.07 * (cohort_large$age_at_baseline - 50) +
    0.60 * (cohort_large$sex == "Male") +
    0.70 * (cohort_large$diabetes_baseline == "Yes") +
    0.45 * (cohort_large$smoking_status == "Current") +
    0.02 * (cohort_large$systolic_bp_mmHg - 130)
)

cohort_large <- cohort_large |>
  mutate(
    latent_prevalent_ihd = rbinom(
      n_persons,
      size = 1,
      prob = pmin(prevalent_probability, 0.70)
    ) == 1,
    
    # Some people with prevalent disease do not self-report it.
    self_reported_prior_ihd = if_else(
      latent_prevalent_ihd & runif(n_persons) < 0.65,
      "Yes",
      self_reported_prior_ihd
    )
  )

# ------------------------------------------------------------
# 4. Simulate time to first true IHD event
#
# This is the "latent clinical truth" in the synthetic dataset.
# Hazard is based on baseline risk factors.
# ------------------------------------------------------------

linear_predictor <- with(
  cohort_large,
  0.055 * (age_at_baseline - 50) +
    0.45 * (sex == "Male") +
    0.75 * (smoking_status == "Current") +
    0.30 * (smoking_status == "Former") +
    0.55 * (diabetes_baseline == "Yes") +
    0.020 * (systolic_bp_mmHg - 130) +
    0.22 * (total_cholesterol_mmol_l - 5.0) +
    0.20 * (bmi_kg_m2 - 27) / 5 +
    0.35 * (physical_activity == "Low") -
    0.20 * (physical_activity == "High")
)

# Baseline annual hazard. Adjust if you want more/fewer events.
baseline_hazard_per_year <- 0.009

event_rate_per_year <- baseline_hazard_per_year * exp(linear_predictor)

time_to_ihd_years <- rexp(
  n_persons,
  rate = event_rate_per_year
)

# People with pre-existing IHD do not enter the incident-event risk set.
time_to_ihd_years[cohort_large$latent_prevalent_ihd] <- NA_real_

# ------------------------------------------------------------
# 5. Simulate non-IHD death / censoring
# ------------------------------------------------------------

death_linear_predictor <- with(
  cohort_large,
  0.07 * (age_at_baseline - 50) +
    0.35 * (sex == "Male") +
    0.45 * (diabetes_baseline == "Yes") +
    0.015 * (systolic_bp_mmHg - 130)
)

death_rate_per_year <- 0.004 * exp(death_linear_predictor)

time_to_death_years <- rexp(
  n_persons,
  rate = death_rate_per_year
)

administrative_followup_years <- as.numeric(
  study_end - cohort_large$baseline_date
) / 365.25

# ------------------------------------------------------------
# 6. Define observed IHD event and follow-up
# ------------------------------------------------------------

cohort_large <- cohort_large |>
  mutate(
    time_to_ihd_years = time_to_ihd_years,
    time_to_death_years = time_to_death_years,
    
    observed_ihd_event = !is.na(time_to_ihd_years) &
      time_to_ihd_years <= time_to_death_years &
      time_to_ihd_years <= administrative_followup_years,
    
    observed_death_before_ihd = time_to_death_years <
      coalesce(time_to_ihd_years, Inf) &
      time_to_death_years <= administrative_followup_years,
    
    true_ihd_event_date = case_when(
      observed_ihd_event ~ baseline_date + round(time_to_ihd_years * 365.25),
      TRUE ~ as.Date(NA)
    ),
    
    death_date = case_when(
      observed_death_before_ihd ~ baseline_date + round(time_to_death_years * 365.25),
      TRUE ~ as.Date(NA)
    ),
    
    censor_date = coalesce(death_date, study_end)
  )

# ------------------------------------------------------------
# 7. Create registry contacts for true incident IHD events
#
# True cases have a high probability of appearing in the registry,
# but some are deliberately missing to create imperfect sensitivity.
# ------------------------------------------------------------

incident_cases <- cohort_large |>
  filter(observed_ihd_event) |>
  mutate(
    captured_in_registry = rbinom(
      n(),
      size = 1,
      prob = 0.92
    ) == 1,
    
    event_type = sample(
      c("Acute_MI", "Chronic_IHD", "Angina"),
      size = n(),
      replace = TRUE,
      prob = c(0.30, 0.45, 0.25)
    ),
    
    setting = case_when(
      event_type == "Acute_MI" ~ "Inpatient",
      event_type == "Chronic_IHD" ~ sample(
        c("Inpatient", "Outpatient"),
        n(),
        replace = TRUE,
        prob = c(0.60, 0.40)
      ),
      event_type == "Angina" ~ sample(
        c("Inpatient", "Outpatient"),
        n(),
        replace = TRUE,
        prob = c(0.15, 0.85)
      )
    ),
    
    hospital = sample(
      c("Hospital_A", "Hospital_B", "Hospital_C"),
      n(),
      replace = TRUE,
      prob = c(0.40, 0.35, 0.25)
    ),
    
    diagnosis_code = case_when(
      event_type == "Acute_MI" ~ "I21.9",
      event_type == "Chronic_IHD" ~ "I25.1",
      event_type == "Angina" ~ "I20.9"
    ),
    
    diagnosis_position = case_when(
      setting == "Inpatient" ~ "Primary",
      setting == "Outpatient" ~ "Primary"
    )
  ) |>
  filter(captured_in_registry)

# First contact for incident events.
registry_contacts_incident <- incident_cases |>
  transmute(
    contact_id = sprintf("IC%06d", row_number()),
    person_id,
    hospital,
    department = "Cardiology",
    setting,
    
    admission_date_raw = if_else(
      setting == "Inpatient",
      as.character(true_ihd_event_date),
      NA_character_
    ),
    
    discharge_date_raw = if_else(
      setting == "Inpatient",
      as.character(true_ihd_event_date + sample(2:7, n(), replace = TRUE)),
      NA_character_
    ),
    
    contact_date_raw = if_else(
      setting == "Outpatient",
      as.character(true_ihd_event_date),
      NA_character_
    )
  )

registry_diagnoses_incident <- incident_cases |>
  transmute(
    diagnosis_id = sprintf("ID%06d", row_number()),
    contact_id = sprintf("IC%06d", row_number()),
    person_id,
    coding_system = "ICD-10",
    diagnosis_code,
    diagnosis_position,
    diagnosis_date_raw = as.character(true_ihd_event_date)
  )

# ------------------------------------------------------------
# 8. Add repeated outpatient records for some outpatient IHD cases
# ------------------------------------------------------------

repeated_outpatient_cases <- incident_cases |>
  filter(setting == "Outpatient") |>
  slice_sample(prop = 0.55) |>
  mutate(
    repeat_date = true_ihd_event_date + sample(
      30:240,
      n(),
      replace = TRUE
    )
  )

registry_contacts_repeat <- repeated_outpatient_cases |>
  transmute(
    contact_id = sprintf("OC%06d", row_number()),
    person_id,
    hospital,
    department = "Cardiology",
    setting = "Outpatient",
    admission_date_raw = NA_character_,
    discharge_date_raw = NA_character_,
    contact_date_raw = as.character(repeat_date)
  )

registry_diagnoses_repeat <- repeated_outpatient_cases |>
  transmute(
    diagnosis_id = sprintf("OD%06d", row_number()),
    contact_id = sprintf("OC%06d", row_number()),
    person_id,
    coding_system = "ICD-10",
    diagnosis_code,
    diagnosis_position = "Primary",
    diagnosis_date_raw = as.character(repeat_date)
  )

# ------------------------------------------------------------
# 9. Add false-positive registry IHD records
#
# People without true observed IHD can still receive a registry
# IHD code due to uncertain angina, coding variation, etc.
# ------------------------------------------------------------

non_cases <- cohort_large |>
  filter(!observed_ihd_event, !latent_prevalent_ihd)

false_positive_people <- non_cases |>
  slice_sample(prop = 0.025) |>
  mutate(
    false_positive_date = baseline_date + sample(
      100:3000,
      n(),
      replace = TRUE
    ),
    
    setting = sample(
      c("Outpatient", "Inpatient"),
      n(),
      replace = TRUE,
      prob = c(0.85, 0.15)
    ),
    
    hospital = sample(
      c("Hospital_A", "Hospital_B", "Hospital_C"),
      n(),
      replace = TRUE
    ),
    
    diagnosis_position = sample(
      c("Primary", "Secondary"),
      n(),
      replace = TRUE,
      prob = c(0.75, 0.25)
    )
  ) |>
  filter(false_positive_date <= study_end)

registry_contacts_false_positive <- false_positive_people |>
  transmute(
    contact_id = sprintf("FP%06d", row_number()),
    person_id,
    hospital,
    department = "Cardiology",
    setting,
    
    admission_date_raw = if_else(
      setting == "Inpatient",
      as.character(false_positive_date),
      NA_character_
    ),
    
    discharge_date_raw = if_else(
      setting == "Inpatient",
      as.character(false_positive_date + sample(1:4, n(), replace = TRUE)),
      NA_character_
    ),
    
    contact_date_raw = if_else(
      setting == "Outpatient",
      as.character(false_positive_date),
      NA_character_
    )
  )

registry_diagnoses_false_positive <- false_positive_people |>
  transmute(
    diagnosis_id = sprintf("FD%06d", row_number()),
    contact_id = sprintf("FP%06d", row_number()),
    person_id,
    coding_system = "ICD-10",
    diagnosis_code = "I20.9",
    diagnosis_position,
    diagnosis_date_raw = as.character(false_positive_date)
  )

# ------------------------------------------------------------
# 10. Add registry records for some prevalent IHD cases
#
# These records occur after baseline but represent old disease.
# ------------------------------------------------------------

prevalent_people <- cohort_large |>
  filter(latent_prevalent_ihd) |>
  mutate(
    followup_code_date = baseline_date + sample(
      50:2000,
      n(),
      replace = TRUE
    ),
    
    setting = sample(
      c("Outpatient", "Inpatient"),
      n(),
      replace = TRUE,
      prob = c(0.70, 0.30)
    ),
    
    hospital = sample(
      c("Hospital_A", "Hospital_B", "Hospital_C"),
      n(),
      replace = TRUE
    )
  ) |>
  filter(followup_code_date <= study_end)

registry_contacts_prevalent <- prevalent_people |>
  transmute(
    contact_id = sprintf("PC%06d", row_number()),
    person_id,
    hospital,
    department = "Cardiology",
    setting,
    
    admission_date_raw = if_else(
      setting == "Inpatient",
      as.character(followup_code_date),
      NA_character_
    ),
    
    discharge_date_raw = if_else(
      setting == "Inpatient",
      as.character(followup_code_date + sample(1:5, n(), replace = TRUE)),
      NA_character_
    ),
    
    contact_date_raw = if_else(
      setting == "Outpatient",
      as.character(followup_code_date),
      NA_character_
    )
  )

registry_diagnoses_prevalent <- prevalent_people |>
  transmute(
    diagnosis_id = sprintf("PD%06d", row_number()),
    contact_id = sprintf("PC%06d", row_number()),
    person_id,
    coding_system = "ICD-10",
    diagnosis_code = "I25.1",
    diagnosis_position = sample(
      c("Primary", "Secondary"),
      n(),
      replace = TRUE,
      prob = c(0.70, 0.30)
    ),
    diagnosis_date_raw = as.character(followup_code_date)
  )

# ------------------------------------------------------------
# 11. Combine registry contacts and diagnoses
# ------------------------------------------------------------

registry_contacts_large <- bind_rows(
  registry_contacts_incident,
  registry_contacts_repeat,
  registry_contacts_false_positive,
  registry_contacts_prevalent
) |>
  arrange(person_id, contact_id)

registry_diagnoses_large <- bind_rows(
  registry_diagnoses_incident,
  registry_diagnoses_repeat,
  registry_diagnoses_false_positive,
  registry_diagnoses_prevalent
) |>
  arrange(person_id, diagnosis_id)

# ------------------------------------------------------------
# 12. Create procedure data
# ------------------------------------------------------------

procedures_large <- incident_cases |>
  filter(event_type %in% c("Acute_MI", "Chronic_IHD")) |>
  slice_sample(prop = 0.60) |>
  transmute(
    procedure_id = sprintf("PR%06d", row_number()),
    person_id,
    procedure_date_raw = as.character(
      true_ihd_event_date + sample(0:3, n(), replace = TRUE)
    ),
    procedure_code = sample(
      c("CAG", "PCI", "CABG"),
      n(),
      replace = TRUE,
      prob = c(0.55, 0.35, 0.10)
    ),
    procedure_name = case_when(
      procedure_code == "CAG" ~ "Coronary angiography",
      procedure_code == "PCI" ~ "Percutaneous coronary intervention",
      procedure_code == "CABG" ~ "Coronary artery bypass graft"
    )
  )

# ------------------------------------------------------------
# 13. Create prescription data
# ------------------------------------------------------------

prescription_cases <- incident_cases |>
  filter(captured_in_registry) |>
  transmute(
    person_id,
    first_dispensing_date = true_ihd_event_date + sample(
      1:30,
      n(),
      replace = TRUE
    )
  )

prescriptions_large <- bind_rows(
  prescription_cases |>
    transmute(
      dispensing_id = sprintf("RX%06d", row_number()),
      person_id,
      dispensing_date_raw = as.character(first_dispensing_date),
      atc_code = "C10AA05",
      medicine_group = "Statin"
    ),
  
  prescription_cases |>
    transmute(
      dispensing_id = sprintf("RA%06d", row_number()),
      person_id,
      dispensing_date_raw = as.character(first_dispensing_date),
      atc_code = "B01AC06",
      medicine_group = "Antiplatelet"
    )
)

# ------------------------------------------------------------
# 14. Create EMR notes and chart review subsample
#
# Review all registry-positive persons would be possible, but we
# sample a subset to mimic a validation study.
# ------------------------------------------------------------

registry_positive_ids <- registry_diagnoses_large |>
  distinct(person_id) |>
  pull(person_id)

registry_negative_ids <- setdiff(
  cohort_large$person_id,
  registry_positive_ids
)

review_positive_ids <- sample(
  registry_positive_ids,
  size = min(350, length(registry_positive_ids))
)

review_negative_ids <- sample(
  registry_negative_ids,
  size = min(150, length(registry_negative_ids))
)

review_ids <- c(review_positive_ids, review_negative_ids)

emr_review_large <- cohort_large |>
  filter(person_id %in% review_ids) |>
  transmute(
    person_id,
    
    chart_review_ihd = case_when(
      observed_ihd_event | latent_prevalent_ihd ~ "Yes",
      TRUE ~ "No"
    ),
    
    reviewer_certainty = case_when(
      observed_ihd_event ~ sample(
        c("Definite", "Probable"),
        n(),
        replace = TRUE,
        prob = c(0.85, 0.15)
      ),
      latent_prevalent_ihd ~ "Definite",
      TRUE ~ "Definite"
    ),
    
    clinical_event_date_raw = case_when(
      observed_ihd_event ~ as.character(true_ihd_event_date),
      latent_prevalent_ihd ~ as.character(
        baseline_date - sample(365:4000, n(), replace = TRUE)
      ),
      TRUE ~ NA_character_
    ),
    
    review_comment = case_when(
      observed_ihd_event ~ "Clinical information supports IHD.",
      latent_prevalent_ihd ~ "Historical IHD documented before baseline.",
      TRUE ~ "No clinical evidence of IHD in reviewed record."
    )
  )

emr_notes_large <- cohort_large |>
  filter(person_id %in% review_ids) |>
  transmute(
    person_id,
    
    note_date_raw = case_when(
      observed_ihd_event ~ as.character(true_ihd_event_date),
      latent_prevalent_ihd ~ as.character(baseline_date),
      TRUE ~ as.character(baseline_date + sample(100:2000, n(), replace = TRUE))
    ),
    
    snomed_code = case_when(
      observed_ihd_event ~ "53741008",
      latent_prevalent_ihd ~ "53741008",
      TRUE ~ NA_character_
    ),
    
    snomed_term = case_when(
      observed_ihd_event | latent_prevalent_ihd ~ "Coronary arteriosclerosis",
      TRUE ~ NA_character_
    ),
    
    note_excerpt = case_when(
      observed_ihd_event ~ "Clinical documentation supports coronary artery disease.",
      latent_prevalent_ihd ~ "Historical coronary artery disease documented.",
      TRUE ~ "No confirmed evidence of ischaemic heart disease."
    )
  )

# ------------------------------------------------------------
# 15. Death/follow-up data
# ------------------------------------------------------------

death_followup_large <- cohort_large |>
  transmute(
    person_id,
    death_date_raw = as.character(death_date),
    cause_of_death_group = case_when(
      !is.na(death_date) ~ sample(
        c("Cardiovascular", "Cancer", "Other"),
        n(),
        replace = TRUE,
        prob = c(0.35, 0.30, 0.35)
      ),
      TRUE ~ NA_character_
    ),
    administrative_end_followup_raw = as.character(study_end)
  )

# ------------------------------------------------------------
# 16. Lookup tables
# ------------------------------------------------------------

icd_harmonisation_lookup_large <- tribble(
  ~coding_system, ~code_raw, ~code_standardised, ~clinical_group, ~include_broad_ihd, ~include_strict_ihd, ~comments,
  "ICD-10", "I20.9", "I209", "Angina pectoris", TRUE, FALSE, "May require repeat records or supporting evidence.",
  "ICD-10", "I21.9", "I219", "Acute myocardial infarction", TRUE, TRUE, "Acute IHD event.",
  "ICD-10", "I25.1", "I251", "Chronic ischaemic heart disease", TRUE, TRUE, "May represent chronic or prevalent disease."
)

snomed_lookup_large <- tribble(
  ~snomed_code, ~snomed_term, ~clinical_group, ~comments,
  "53741008", "Coronary arteriosclerosis", "IHD", "Clinical concept; date and context still required."
)

# ------------------------------------------------------------
# 17. Save files
# ------------------------------------------------------------

write_csv(cohort_large, here("data", "large_raw", "cohort_baseline_large.csv"))
write_csv(registry_contacts_large, here("data", "large_raw", "registry_contacts_large.csv"))
write_csv(registry_diagnoses_large, here("data", "large_raw", "registry_diagnoses_large.csv"))
write_csv(procedures_large, here("data", "large_raw", "procedures_large.csv"))
write_csv(prescriptions_large, here("data", "large_raw", "prescriptions_large.csv"))
write_csv(emr_notes_large, here("data", "large_raw", "emr_notes_excerpt_large.csv"))
write_csv(emr_review_large, here("data", "large_raw", "emr_review_large.csv"))
write_csv(death_followup_large, here("data", "large_raw", "death_followup_large.csv"))
write_csv(icd_harmonisation_lookup_large, here("data", "large_raw", "icd_harmonisation_lookup_large.csv"))
write_csv(snomed_lookup_large, here("data", "large_raw", "snomed_lookup_large.csv"))

cat("\nLarge synthetic data created.\n")
cat("Cohort size:", nrow(cohort_large), "\n")
cat("Observed true IHD events:", sum(cohort_large$observed_ihd_event), "\n")
cat("Latent prevalent IHD:", sum(cohort_large$latent_prevalent_ihd), "\n")
cat("Registry diagnosis records:", nrow(registry_diagnoses_large), "\n")
cat("Chart-reviewed people:", nrow(emr_review_large), "\n")