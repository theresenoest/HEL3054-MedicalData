# HEL3054-Medicaldataday
One day I have responsibility for in HEL-3054

# Validating and using ischaemic heart disease data across records, registries, and time

This repository contains synthetic data and teaching materials for a master's-level
health-data workshop.

## Case

Students work with a population-based cohort linked to:

- a national patient registry;
- hospital electronic medical records;
- a procedure register;
- a prescription database; and
- mortality/follow-up information.

The main question is:

> How accurately do registry-based ICD codes identify people with clinically
> documented ischaemic heart disease (IHD), and how can linked data be prepared
> for later analysis of first IHD events?

The teaching focus is on:

- health-data sources and linkage;
- differences between person-, contact-, diagnosis-, and procedure-level data;
- ICD, SNOMED CT, procedures, notes, and prescriptions;
- wide/long data and one-to-many joins;
- dates, timelines, index dates, and follow-up;
- first recorded versus incident disease;
- registry validation;
- data quality, metadata, harmonisation, and reproducible workflows.

## Important

All data in this repository are fictional and synthetic. They do not describe
real people, institutions, hospitals, or patient records.

## Setup

1. Open `ihd-registry-validation-workshop.Rproj`.
2. Install packages if needed:

```r
install.packages(c("tidyverse", "lubridate", "janitor", "here", "skimr"))
