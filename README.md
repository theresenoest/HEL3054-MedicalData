# Validating and using ischaemic heart disease data across records, registries, and time

How can registry, EMR, procedure, and cohort data be combined to identify a valid and reproducible first-IHD-event outcome?

This repository contains synthetic data and teaching materials for a master's-level
health-data workshop.


# Teaching plan

## 09.15–10.00
### What does an IHD diagnosis mean in different data sources?

Students examine:

- population cohort data;
- national registry records;
- medical-record excerpts;
- SNOMED CT concepts;
- ICD codes;
- procedures;
- medication data;
- death/follow-up data.

Output:

- data-source map;
- draft IHD phenotype definition;
- identification of the unit of observation in every dataset.

## 10.15–11.00
### Time is part of the phenotype

Students:

- inspect data types;
- parse and clean dates;
- identify impossible, missing, and inconsistent dates;
- create patient timelines;
- discuss first recorded versus incident IHD;
- define an index-event date.

Output:

- timeline for selected patients;
- first-event rule and date rule.

## 11.15–12.00
### Registry validation study

Students compare registry algorithms with chart-review status.

Output:

- two-by-two table;
- PPV, sensitivity, specificity, and NPV where estimable;
- interpretation of broad versus strict algorithms.

## 13.15–14.00
### Building an analysis-ready first-IHD-event dataset

Students:

- link person-, contact-, diagnosis-, and procedure-level data;
- recognise one-to-many joins;
- create broad and strict IHD indicators;
- select the first eligible event after baseline;
- create a person-level dataset.

Output:

- one row per cohort participant;
- documented phenotype and index-event algorithm.

## 14.15–15.00
### Phenotype clinic: validation, sensitivity analyses, and reflection

Students compare algorithms and investigate disagreement between records and registry data.

Output:

- phenotype protocol;
- sensitivity-analysis table;
- data-quality and methodological reflection.


--------------
## Our case for the day

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
