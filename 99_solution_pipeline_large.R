# ============================================================
# 99_solution_pipeline_large.R
#
# Solution pipeline for large synthetic IHD dataset.
#
# Creates:
# - event-level registry dataset
# - broad and strict IHD phenotypes
# - validation dataset and metrics
# - first-event analysis-ready dataset
# - Kaplan-Meier curves
# - Cox regression models
#
# All data are fictional and for teaching purposes only.
# ============================================================

library(tidyverse)
library(lubridate)
library(here)
library(survival)

# ------------------------------------------------------------
# 0. Folders
# ------------------------------------------------------------

dir.create(here("data", "large_derived"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "large_tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "large_figures"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. Helper functions
# ------------------------------------------------------------

clean_code <- function(x) {
  x |>
    str_to_upper() |>
    str_trim() |>
    str_remove_all("\\.")
}

parse_mixed_date <- function(x) {
  x <- as.character(x)
  x <- na_if(x, "")
  
  suppressWarnings(
    coalesce(
      ymd(x, quiet = TRUE),
      dmy(x, quiet = TRUE),
      mdy(x, quiet = TRUE)
    )
  )
}

validation_metrics <- function(data, registry_variable, chart_variable) {
  
  registry_positive <- data[[registry_variable]]
  chart_positive <- data[[chart_variable]]
  
  tp <- sum(registry_positive & chart_positive, na.rm = TRUE)
  fp <- sum(registry_positive & !chart_positive, na.rm = TRUE)
  fn <- sum(!registry_positive & chart_positive, na.rm = TRUE)
  tn <- sum(!registry_positive & !chart_positive, na.rm = TRUE)
  
  tibble(
    true_positive = tp,
    false_positive = fp,
    false_negative = fn,
    true_negative = tn,
    ppv = tp / (tp + fp),
    sensitivity = tp / (tp + fn),
    specificity = tn / (tn + fp),
    npv = tn / (tn + fn)
  )
}

# ------------------------------------------------------------
# 2. Read raw data
# ------------------------------------------------------------

cohort <- read_csv(
  here("data", "large_raw", "cohort_baseline_large.csv"),
  show_col_types = FALSE
)

contacts <- read_csv(
  here("data", "large_raw", "registry_contacts_large.csv"),
  show_col_types = FALSE
)

diagnoses <- read_csv(
  here("data", "large_raw", "registry_diagnoses_large.csv"),
  show_col_types = FALSE
)

procedures <- read_csv(
  here("data", "large_raw", "procedures_large.csv"),
  show_col_types = FALSE
)

review <- read_csv(
  here("data", "large_raw", "emr_review_large.csv"),
  show_col_types = FALSE
)

followup <- read_csv(
  here("data", "large_raw", "death_followup_large.csv"),
  show_col_types = FALSE
)

lookup <- read_csv(
  here("data", "large_raw", "icd_harmonisation_lookup_large.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------
# 3. Clean person-level data
# ------------------------------------------------------------

cohort_clean <- cohort |>
  mutate(
    baseline_date = parse_mixed_date(baseline_date),
    
    age_at_baseline = year(baseline_date) - birth_year,
    
    diabetes_baseline = factor(
      diabetes_baseline,
      levels = c("No", "Yes")
    ),
    
    smoking_status = factor(
      smoking_status,
      levels = c("Never", "Former", "Current")
    ),
    
    physical_activity = factor(
      physical_activity,
      levels = c("High", "Moderate", "Low")
    ),
    
    sex = factor(sex),
    
    education = factor(
      education,
      levels = c("University", "Secondary", "Primary")
    ),
    
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
# 4. Clean contact and diagnosis data
# ------------------------------------------------------------

contacts_clean <- contacts |>
  mutate(
    admission_date = parse_mixed_date(admission_date_raw),
    discharge_date = parse_mixed_date(discharge_date_raw),
    contact_date = parse_mixed_date(contact_date_raw),
    
    # Prefer admission date for inpatient contacts,
    # contact date for outpatient contacts.
    contact_event_date = coalesce(
      admission_date,
      contact_date,
      discharge_date
    )
  )

lookup_clean <- lookup |>
  mutate(
    code_standardised = clean_code(code_raw)
  )

diagnoses_clean <- diagnoses |>
  mutate(
    diagnosis_date = parse_mixed_date(diagnosis_date_raw),
    code_standardised = clean_code(diagnosis_code)
  ) |>
  left_join(
    lookup_clean |>
      select(
        coding_system,
        code_standardised,
        clinical_group,
        include_broad_ihd,
        include_strict_ihd
      ),
    by = c("coding_system", "code_standardised")
  ) |>
  mutate(
    include_broad_ihd = replace_na(include_broad_ihd, FALSE),
    include_strict_ihd = replace_na(include_strict_ihd, FALSE)
  )

# ------------------------------------------------------------
# 5. Create diagnosis/contact-level event dataset
#
# One row = one diagnosis record within one contact.
# ------------------------------------------------------------

event_data <- diagnoses_clean |>
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
        contact_event_date
      ),
    by = c("contact_id", "person_id")
  ) |>
  mutate(
    event_date = coalesce(
      contact_event_date,
      diagnosis_date
    ),
    
    broad_ihd = include_broad_ihd,
    
    strict_ihd_inpatient =
      broad_ihd &
      setting == "Inpatient" &
      diagnosis_position == "Primary"
  )

# Count qualifying outpatient IHD contacts per person.
outpatient_counts <- event_data |>
  filter(
    broad_ihd,
    setting == "Outpatient",
    !is.na(event_date)
  ) |>
  distinct(person_id, event_date) |>
  count(person_id, name = "n_outpatient_ihd_dates")

event_data <- event_data |>
  left_join(outpatient_counts, by = "person_id") |>
  mutate(
    n_outpatient_ihd_dates =
      replace_na(n_outpatient_ihd_dates, 0L),
    
    strict_ihd_outpatient =
      setting == "Outpatient" &
      n_outpatient_ihd_dates >= 2,
    
    strict_ihd =
      strict_ihd_inpatient |
      strict_ihd_outpatient
  ) |>
  arrange(person_id, event_date)

write_csv(
  event_data,
  here("data", "large_derived", "event_data_large.csv")
)

# ------------------------------------------------------------
# 6. Create person-level phenotype indicators
# ------------------------------------------------------------

phenotype_person <- cohort_clean |>
  select(person_id, baseline_date) |>
  left_join(
    event_data |>
      group_by(person_id) |>
      summarise(
        registry_ihd_broad = any(broad_ihd),
        registry_ihd_strict = any(strict_ihd),
        any_inpatient_primary_ihd = any(strict_ihd_inpatient),
        n_outpatient_ihd_dates = max(n_outpatient_ihd_dates),
        .groups = "drop"
      ),
    by = "person_id"
  ) |>
  mutate(
    registry_ihd_broad =
      replace_na(registry_ihd_broad, FALSE),
    
    registry_ihd_strict =
      replace_na(registry_ihd_strict, FALSE),
    
    any_inpatient_primary_ihd =
      replace_na(any_inpatient_primary_ihd, FALSE),
    
    n_outpatient_ihd_dates =
      replace_na(n_outpatient_ihd_dates, 0L)
  )

# ------------------------------------------------------------
# 7. Create first broad and strict registry event dates
# ------------------------------------------------------------

first_broad_event <- event_data |>
  filter(broad_ihd, !is.na(event_date)) |>
  group_by(person_id) |>
  slice_min(event_date, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    person_id,
    first_broad_ihd_date = event_date,
    first_broad_ihd_code = diagnosis_code,
    first_broad_ihd_setting = setting,
    first_broad_ihd_hospital = hospital
  )

# First inpatient-primary strict event.
first_inpatient_strict_event <- event_data |>
  filter(strict_ihd_inpatient, !is.na(event_date)) |>
  group_by(person_id) |>
  slice_min(event_date, with_ties = FALSE) |>
  ungroup() |>
  transmute(
    person_id,
    first_inpatient_strict_date = event_date
  )

# Date of second outpatient record for strict outpatient phenotype.
second_outpatient_strict_event <- event_data |>
  filter(
    broad_ihd,
    setting == "Outpatient",
    !is.na(event_date)
  ) |>
  distinct(person_id, event_date) |>
  arrange(person_id, event_date) |>
  group_by(person_id) |>
  mutate(
    outpatient_number = row_number()
  ) |>
  filter(outpatient_number == 2) |>
  ungroup() |>
  transmute(
    person_id,
    second_outpatient_strict_date = event_date
  )

first_strict_event <- full_join(
  first_inpatient_strict_event,
  second_outpatient_strict_event,
  by = "person_id"
) |>
  rowwise() |>
  mutate(
    first_strict_ihd_date = {
      possible_dates <- c(
        first_inpatient_strict_date,
        second_outpatient_strict_date
      )
      
      possible_dates <- possible_dates[!is.na(possible_dates)]
      
      if (length(possible_dates) == 0) {
        as.Date(NA)
      } else {
        min(possible_dates)
      }
    }
  ) |>
  ungroup() |>
  select(person_id, first_strict_ihd_date)

# ------------------------------------------------------------
# 8. Identify possible prevalent IHD before baseline
#
# For this large synthetic teaching dataset, use self-report
# plus any registry IHD event before baseline.
# ------------------------------------------------------------

prevalent_registry <- event_data |>
  filter(broad_ihd, !is.na(event_date)) |>
  left_join(
    cohort_clean |>
      select(person_id, baseline_date),
    by = "person_id"
  ) |>
  filter(event_date < baseline_date) |>
  distinct(person_id) |>
  mutate(prevalent_registry_ihd = TRUE)

# ------------------------------------------------------------
# 9. Create analysis-ready first-event dataset
#
# One row = one participant.
# ------------------------------------------------------------

analysis_ready <- cohort_clean |>
  left_join(followup_clean, by = "person_id") |>
  left_join(prevalent_registry, by = "person_id") |>
  left_join(first_broad_event, by = "person_id") |>
  left_join(first_strict_event, by = "person_id") |>
  mutate(
    prevalent_registry_ihd =
      replace_na(prevalent_registry_ihd, FALSE),
    
    possible_prevalent_ihd =
      self_reported_prior_ihd |
      prevalent_registry_ihd,
    
    censor_date = pmin(
      coalesce(death_date, administrative_end_followup),
      administrative_end_followup
    ),
    
    # Broad event outcome:
    # first broad registry IHD code after baseline and before censoring.
    broad_event = !possible_prevalent_ihd &
      !is.na(first_broad_ihd_date) &
      first_broad_ihd_date >= baseline_date &
      first_broad_ihd_date <= censor_date,
    
    # Strict event outcome:
    # first strict registry IHD event after baseline and before censoring.
    strict_event = !possible_prevalent_ihd &
      !is.na(first_strict_ihd_date) &
      first_strict_ihd_date >= baseline_date &
      first_strict_ihd_date <= censor_date,
    
    # Use strict event date for strict phenotype model.
    strict_exit_date = if_else(
      strict_event,
      first_strict_ihd_date,
      censor_date
    ),
    
    broad_exit_date = if_else(
      broad_event,
      first_broad_ihd_date,
      censor_date
    ),
    
    strict_followup_days =
      as.numeric(strict_exit_date - baseline_date),
    
    broad_followup_days =
      as.numeric(broad_exit_date - baseline_date),
    
    strict_followup_years =
      strict_followup_days / 365.25,
    
    broad_followup_years =
      broad_followup_days / 365.25,
    
    smoking_status = fct_relevel(
      smoking_status,
      "Never"
    ),
    
    physical_activity = fct_relevel(
      physical_activity,
      "High"
    ),
    
    diabetes_baseline = fct_relevel(
      diabetes_baseline,
      "No"
    )
  ) |>
  filter(
    !possible_prevalent_ihd,
    strict_followup_days > 0,
    broad_followup_days > 0
  )

write_csv(
  analysis_ready,
  here("data", "large_derived", "analysis_ready_first_ihd.csv")
)

# ------------------------------------------------------------
# 10. Validation analysis
# ------------------------------------------------------------

review_clean <- review |>
  mutate(
    chart_ihd_positive = chart_review_ihd == "Yes"
  )

validation_data <- review_clean |>
  select(
    person_id,
    chart_review_ihd,
    reviewer_certainty,
    chart_ihd_positive,
    clinical_event_date_raw
  ) |>
  left_join(
    phenotype_person |>
      select(
        person_id,
        registry_ihd_broad,
        registry_ihd_strict
      ),
    by = "person_id"
  ) |>
  mutate(
    registry_ihd_broad =
      replace_na(registry_ihd_broad, FALSE),
    
    registry_ihd_strict =
      replace_na(registry_ihd_strict, FALSE)
  )

validation_results <- bind_rows(
  validation_metrics(
    validation_data,
    registry_variable = "registry_ihd_broad",
    chart_variable = "chart_ihd_positive"
  ) |>
    mutate(algorithm = "Broad: any IHD registry code"),
  
  validation_metrics(
    validation_data,
    registry_variable = "registry_ihd_strict",
    chart_variable = "chart_ihd_positive"
  ) |>
    mutate(algorithm = "Strict: inpatient primary or repeated outpatient")
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
  validation_data,
  here("data", "large_derived", "validation_dataset_large.csv")
)

write_csv(
  validation_results,
  here("outputs", "large_tables", "validation_results_large.csv")
)

# ------------------------------------------------------------
# 11. Descriptive table for first-event cohort
# ------------------------------------------------------------

descriptive_table <- analysis_ready |>
  summarise(
    n = n(),
    strict_events = sum(strict_event),
    event_percentage = round(mean(strict_event) * 100, 1),
    median_followup_years = round(median(strict_followup_years), 2),
    mean_age = round(mean(age_at_baseline), 1),
    percentage_male = round(mean(sex == "Male") * 100, 1),
    percentage_current_smoking = round(
      mean(smoking_status == "Current") * 100,
      1
    ),
    percentage_diabetes = round(
      mean(diabetes_baseline == "Yes") * 100,
      1
    )
  )

write_csv(
  descriptive_table,
  here("outputs", "large_tables", "descriptive_table_large.csv")
)

# ------------------------------------------------------------
# 12. Kaplan-Meier curve
#
# Compare time to strict first IHD event by smoking status.
# ------------------------------------------------------------

km_fit_smoking <- survfit(
  Surv(strict_followup_years, strict_event) ~ smoking_status,
  data = analysis_ready
)

png(
  filename = here(
    "outputs",
    "large_figures",
    "kaplan_meier_ihd_by_smoking.png"
  ),
  width = 1800,
  height = 1200,
  res = 180
)

plot(
  km_fit_smoking,
  col = c("#1B9E77", "#D95F02", "#7570B3"),
  lwd = 2,
  xlab = "Years since cohort baseline",
  ylab = "Probability of remaining free from strict IHD event",
  mark.time = FALSE,
  conf.int = FALSE,
  xlim = c(0, max(analysis_ready$strict_followup_years))
)

legend(
  "bottomleft",
  legend = levels(analysis_ready$smoking_status),
  col = c("#1B9E77", "#D95F02", "#7570B3"),
  lwd = 2,
  title = "Smoking status at baseline"
)

title(
  main = "Kaplan-Meier curve: time to first strict IHD event"
)

dev.off()

# Log-rank test for difference between smoking groups.
log_rank_smoking <- survdiff(
  Surv(strict_followup_years, strict_event) ~ smoking_status,
  data = analysis_ready
)

log_rank_table <- tibble(
  chisq = log_rank_smoking$chisq,
  df = length(log_rank_smoking$n) - 1,
  p_value = pchisq(
    log_rank_smoking$chisq,
    df = length(log_rank_smoking$n) - 1,
    lower.tail = FALSE
  )
)

write_csv(
  log_rank_table,
  here("outputs", "large_tables", "log_rank_smoking.csv")
)

# ------------------------------------------------------------
# 13. Cox regression
#
# Outcome: time to strict first IHD event.
# ------------------------------------------------------------

cox_model_unadjusted_smoking <- coxph(
  Surv(strict_followup_years, strict_event) ~ smoking_status,
  data = analysis_ready
)

cox_model_adjusted <- coxph(
  Surv(strict_followup_years, strict_event) ~
    age_at_baseline +
    sex +
    smoking_status +
    systolic_bp_mmHg +
    total_cholesterol_mmol_l +
    diabetes_baseline +
    bmi_kg_m2 +
    physical_activity +
    education,
  data = analysis_ready
)

# Save model summaries as text files.
capture.output(
  summary(cox_model_unadjusted_smoking),
  file = here(
    "outputs",
    "large_tables",
    "cox_model_unadjusted_smoking.txt"
  )
)

capture.output(
  summary(cox_model_adjusted),
  file = here(
    "outputs",
    "large_tables",
    "cox_model_adjusted.txt"
  )
)

# Create a tidy Cox results table without requiring extra packages.
cox_results <- summary(cox_model_adjusted)

cox_results_table <- tibble(
  variable = rownames(cox_results$coefficients),
  hazard_ratio = exp(cox_results$coefficients[, "coef"]),
  lower_95_ci = exp(cox_results$conf.int[, "lower .95"]),
  upper_95_ci = exp(cox_results$conf.int[, "upper .95"]),
  p_value = cox_results$coefficients[, "Pr(>|z|)"]
) |>
  mutate(
    across(
      c(hazard_ratio, lower_95_ci, upper_95_ci),
      ~ round(.x, 3)
    ),
    p_value = signif(p_value, 3)
  )

write_csv(
  cox_results_table,
  here("outputs", "large_tables", "cox_results_adjusted.csv")
)

# ------------------------------------------------------------
# 14. Proportional hazards assumption check
# ------------------------------------------------------------

cox_ph_test <- cox.zph(cox_model_adjusted)

capture.output(
  cox_ph_test,
  file = here(
    "outputs",
    "large_tables",
    "cox_proportional_hazards_test.txt"
  )
)

# ------------------------------------------------------------
# 15. Console output
# ------------------------------------------------------------

cat("\n============================================\n")
cat("Large-data pipeline completed successfully.\n")
cat("============================================\n\n")

cat("Analysis-ready cohort size:", nrow(analysis_ready), "\n")
cat("Strict first IHD events:", sum(analysis_ready$strict_event), "\n")
cat(
  "Median follow-up years:",
  round(median(analysis_ready$strict_followup_years), 2),
  "\n\n"
)

cat("Validation results:\n")
print(validation_results)

cat("\nAdjusted Cox regression results:\n")
print(cox_results_table)

cat("\nCreated files:\n")
cat("- data/large_derived/analysis_ready_first_ihd.csv\n")
cat("- data/large_derived/event_data_large.csv\n")
cat("- data/large_derived/validation_dataset_large.csv\n")
cat("- outputs/large_tables/validation_results_large.csv\n")
cat("- outputs/large_tables/cox_results_adjusted.csv\n")
cat("- outputs/large_tables/cox_model_adjusted.txt\n")
cat("- outputs/large_tables/log_rank_smoking.csv\n")
cat("- outputs/large_figures/kaplan_meier_ihd_by_smoking.png\n")
