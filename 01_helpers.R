library(tidyverse)
library(lubridate)

clean_code <- function(x) {
  x |>
    str_to_upper() |>
    str_remove_all("\\.")
}

parse_mixed_date <- function(x) {
  suppressWarnings(
    coalesce(
      ymd(x),
      dmy(x),
      ymd(str_replace_all(x, "/", "-"))
    )
  )
}

contact_event_date <- function(admission_date, contact_date, diagnosis_date) {
  coalesce(admission_date, contact_date, diagnosis_date)
}

calculate_ppv <- function(true_positive, false_positive) {
  true_positive / (true_positive + false_positive)
}

calculate_sensitivity <- function(true_positive, false_negative) {
  true_positive / (true_positive + false_negative)
}

calculate_specificity <- function(true_negative, false_positive) {
  true_negative / (true_negative + false_positive)
}

calculate_npv <- function(true_negative, false_negative) {
  true_negative / (true_negative + false_negative)
}