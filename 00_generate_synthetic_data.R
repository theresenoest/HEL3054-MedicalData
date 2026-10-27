# ============================================================
# Generate synthetic data for:
# Validating and using IHD data across records, registries,
# and time.
#
# All data are fictional and created solely for teaching.
# ============================================================

library(tidyverse)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
dir.create("data/dictionary", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Population-based cohort: one row per participant
# ------------------------------------------------------------

cohort_baseline <- read_csv(I("
person_id,baseline_date,birth_year,sex,education,smoking_status,physical_activity,bmi_kg_m2,systolic_bp_mmHg,total_cholesterol_mmol_l,diabetes_baseline,self_reported_prior_ihd
P001,2006-05-10,1950,Male,Secondary,Current,Low,29.4,158,6.7,No,No
P002,2007-09-15,1962,Female,University,Never,High,23.1,118,4.9,No,No
P003,2005-03-21,1943,Male,Primary,Former,Moderate,27.8,146,5.8,No,Yes
P004,2006-11-02,1958,Female,University,Former,High,24.6,126,5.2,No,No
P005,2007-04-18,1955,Male,Secondary,Current,Low,31.2,164,7.1,Yes,No
P006,2005-08-30,1948,Male,Primary,Former,Low,28.7,151,6.1,No,No
P007,2006-02-12,1960,Male,Secondary,Current,Moderate,30.1,171,6.8,No,No
P008,2007-12-04,1946,Female,Primary,Former,Low,32.4,168,6.0,Yes,No
P009,2005-06-19,1952,Female,Secondary,Never,Moderate,25.9,139,5.4,No,No
P010,2008-01-25,1964,Male,University,Former,High,26.4,133,5.6,No,No
P011,2006-09-11,1951,Male,Secondary,Current,Low,29.8,156,6.4,Yes,No
P012,2005-01-17,1940,Female,Primary,Former,Low,28.9,149,5.9,No,Yes
P013,2007-07-08,1957,Female,University,Never,High,22.7,121,4.8,No,No
P014,2005-12-20,1947,Male,Primary,Former,Moderate,28.4,154,6.5,No,No
P015,2006-04-04,1961,Female,Secondary,Never,Moderate,26.2,129,5.1,No,No
P016,2007-03-14,1953,Male,Primary,Current,Low,33.5,176,7.3,Yes,No
P017,2008-08-22,1965,Female,University,Former,High,24.1,124,5.0,No,No
P018,2006-06-30,1954,Male,Secondary,Current,Low,30.5,162,6.9,No,No
P019,2007-10-09,1959,Female,Secondary,Never,Moderate,27.0,137,5.7,No,No
P020,2005-05-26,1945,Male,Primary,Former,Low,29.1,148,6.2,Yes,No
"), show_col_types = FALSE)

write_csv(cohort_baseline, "data/raw/cohort_baseline.csv")

# ------------------------------------------------------------
# National registry contacts: one row per contact
# Deliberately inconsistent date formats are included.
# ------------------------------------------------------------

registry_contacts <- read_csv(I("
contact_id,person_id,hospital,department,setting,admission_date_raw,discharge_date_raw,contact_date_raw
C001,P001,Hospital_A,Cardiology,Inpatient,2012-05-14,2012-05-18,
C002,P002,Hospital_B,Outpatient,,,15/06/2015
C003,P003,Hospital_A,Cardiology,Outpatient,,,2012/09/10
C004,P005,Hospital_B,Cardiology,Outpatient,,,2013-03-01
C005,P005,Hospital_B,Cardiology,Outpatient,,,2013-09-10
C006,P006,Hospital_C,Respiratory,Inpatient,2014-11-03,2014-11-09,
C007,P007,Hospital_A,Cardiology,Inpatient,2011-02-08,2011-02-14,
C008,P008,Hospital_A,Internal_medicine,Inpatient,2015-01-09,2015-01-14,
C009,P009,Hospital_C,Neurology,Inpatient,2016-04-19,2016-04-26,
C010,P010,Hospital_A,Cardiology,Inpatient,2016-07-02,2016-07-05,
C011,P010,Hospital_B,Cardiology,Inpatient,2016-07-05,2016-07-07,
C012,P011,Hospital_B,Cardiology,Inpatient,2017-10-12,2017-10-10,
C013,P013,Hospital_C,Cardiology,Outpatient,,,2018-04-23
C014,P014,Hospital_A,Cardiology,Inpatient,2010-12-10,2010-12-13,
C015,P015,Hospital_B,Emergency,Inpatient,2013-02-01,2013-02-04,
C016,P016,Hospital_B,Cardiology,Inpatient,2014-??-12,2014-12-16,
C017,P017,Hospital_C,Cardiology,Outpatient,,,2019-08-03
C018,P018,Hospital_A,Cardiology,Inpatient,2020-01-12,2020-01-17,
C019,P020,Hospital_C,Cardiology,Inpatient,2004-04-02,2004-04-06,
"), show_col_types = FALSE)

write_csv(registry_contacts, "data/raw/registry_contacts.csv")

# ------------------------------------------------------------
# Registry diagnoses: one row per diagnosis
# ------------------------------------------------------------

registry_diagnoses <- read_csv(I("
diagnosis_id,contact_id,person_id,coding_system,diagnosis_code,diagnosis_position,diagnosis_date_raw
D001,C001,P001,ICD-10,I25.1,Primary,2012-05-18
D002,C002,P002,ICD-10,I20.9,Primary,15/06/2015
D003,C003,P003,ICD-10,I25.1,Secondary,2012/09/10
D004,C004,P005,ICD-10,I20.9,Primary,2013-03-01
D005,C005,P005,ICD-10,I20.9,Primary,2013-09-10
D006,C006,P006,ICD-10,I25.1,Secondary,2014-11-09
D007,C006,P006,ICD-10,J18.9,Primary,2014-11-09
D008,C007,P007,ICD-10,I21.9,Primary,2011-02-14
D009,C008,P008,ICD-10,I50.9,Primary,2015-01-14
D010,C009,P009,ICD-10,I63.9,Primary,2016-04-26
D011,C010,P010,ICD-10,I25.1,Primary,2016-07-05
D012,C011,P010,ICD-10,I25.1,Primary,2016-07-07
D013,C012,P011,ICD-10,I25.1,Primary,2017-10-10
D014,C013,P013,ICD-10,I20.9,Primary,2018-04-23
D015,C014,P014,ICD-9,414.01,Primary,2010-12-13
D016,C015,P015,ICD-10,R07.4,Primary,2013-02-04
D017,C016,P016,ICD-10,I25.1,Primary,2014-12-16
D018,C017,P017,ICD-10,I20.9,Secondary,2019-08-03
D019,C018,P018,ICD-10,I25.1,Primary,2020-01-17
D020,C019,P020,ICD-9,414.01,Primary,2004-04-06
"), show_col_types = FALSE)

write_csv(registry_diagnoses, "data/raw/registry_diagnoses.csv")

# ------------------------------------------------------------
# Procedures
# ------------------------------------------------------------

procedures <- read_csv(I("
procedure_id,person_id,procedure_date_raw,procedure_code,procedure_name
PR001,P001,2012-05-15,CAG,Coronary angiography
PR002,P001,2012-05-16,PCI,Percutaneous coronary intervention
PR003,P007,2011-02-10,CAG,Coronary angiography
PR004,P007,2011-02-11,PCI,Percutaneous coronary intervention
PR005,P010,2016-07-03,CAG,Coronary angiography
PR006,P014,2010-12-11,CABG,Coronary artery bypass graft
PR007,P018,2020-01-10,CAG,Coronary angiography
PR008,P020,2004-04-04,CABG,Coronary artery bypass graft
"), show_col_types = FALSE)

write_csv(procedures, "data/raw/procedures.csv")

# ------------------------------------------------------------
# Prescription database
# ------------------------------------------------------------

prescriptions <- read_csv(I("
dispensing_id,person_id,dispensing_date_raw,atc_code,medicine_group
RX001,P001,2012-05-20,C10AA05,Statin
RX002,P001,2012-05-20,B01AC06,Antiplatelet
RX003,P005,2013-09-15,C10AA01,Statin
RX004,P007,2011-02-16,C10AA05,Statin
RX005,P007,2011-02-16,B01AC06,Antiplatelet
RX006,P010,2016-07-09,C10AA05,Statin
RX007,P014,2010-12-15,C10AA05,Statin
RX008,P018,2020-01-20,C10AA05,Statin
RX009,P020,2004-04-10,C10AA05,Statin
RX010,P016,2014-12-20,C10AA01,Statin
"), show_col_types = FALSE)

write_csv(prescriptions, "data/raw/prescriptions.csv")

# ------------------------------------------------------------
# EMR excerpts and SNOMED concepts
# ------------------------------------------------------------

emr_notes_excerpt <- read_csv(I("
person_id,note_date_raw,snomed_code,snomed_term,note_excerpt
P001,2012-05-16,53741008,Coronary arteriosclerosis,\"Angiography confirms significant coronary artery disease. PCI performed.\"
P002,2015-06-15,,,\"Intermittent chest pain. Cardiac work-up negative. IHD not confirmed.\"
P003,2012-09-10,22298006,Myocardial infarction,\"History of myocardial infarction in 1999. No evidence of acute event today.\"
P004,2014-08-01,53741008,Coronary arteriosclerosis,\"Known coronary artery disease documented in problem list; no registry IHD record found.\"
P005,2013-09-10,53741008,Coronary arteriosclerosis,\"Recurrent exertional angina. Clinical assessment supports stable IHD.\"
P006,2014-11-07,53741008,Coronary arteriosclerosis,\"Known stable coronary artery disease. Admitted because of pneumonia.\"
P007,2011-02-10,57054005,Acute myocardial infarction,\"Acute myocardial infarction confirmed. PCI performed.\"
P011,2017-10-11,,,\"Discharge date appears inconsistent in administrative extract. IHD confirmed.\"
P013,2018-04-23,53741008,Coronary arteriosclerosis,\"Stable coronary artery disease. Managed in outpatient clinic.\"
P014,2010-12-11,53741008,Coronary arteriosclerosis,\"Severe coronary artery disease. CABG performed.\"
P015,2013-02-02,,,\"Chest pain considered musculoskeletal. No evidence of IHD.\"
P016,2014-12-14,53741008,Coronary arteriosclerosis,\"Coronary artery disease suspected, but original admission date is incomplete.\"
P017,2019-08-03,,,\"IHD remains uncertain; further investigation planned.\"
P018,2020-01-13,53741008,Coronary arteriosclerosis,\"Coronary artery disease confirmed after angiography.\"
P020,2004-04-03,53741008,Coronary arteriosclerosis,\"Known severe coronary artery disease. CABG performed before cohort baseline.\"
"), show_col_types = FALSE)

write_csv(emr_notes_excerpt, "data/raw/emr_notes_excerpt.csv")

# ------------------------------------------------------------
# Chart-review outcome
# ------------------------------------------------------------

emr_review <- read_csv(I("
person_id,chart_review_ihd,reviewer_certainty,clinical_event_date_raw,review_comment
P001,Yes,Definite,2012-05-14,\"Angiography and PCI support IHD.\"
P002,No,Definite,,\"Chest pain investigated; IHD ruled out.\"
P003,Yes,Definite,1999-06-01,\"Historical MI; 2012 registry record is not incident disease.\"
P004,Yes,Probable,2014-08-01,\"IHD in problem list and clinical note, but no registry diagnosis found.\"
P005,Yes,Probable,2013-03-01,\"Repeated outpatient angina records support stable IHD.\"
P006,Yes,Definite,2008-04-12,\"Established IHD; I25.1 is secondary during pneumonia admission.\"
P007,Yes,Definite,2011-02-08,\"Acute MI confirmed clinically and by procedure.\"
P010,Yes,Definite,2016-07-02,\"IHD documented during admission and transfer.\"
P011,Yes,Definite,2017-10-12,\"Clinical IHD confirmed; administrative dates are inconsistent.\"
P013,Yes,Definite,2018-04-23,\"Outpatient clinical IHD confirmed.\"
P014,Yes,Definite,2010-12-10,\"CABG and clinical documentation support IHD.\"
P015,No,Definite,,\"Non-cardiac chest pain.\"
P016,Probable,Probable,2014-12-12,\"Likely IHD, but original admission date cannot be established.\"
P017,No,Probable,,\"IHD uncertain; evidence insufficient.\"
P018,Yes,Definite,2020-01-12,\"Angiography and discharge summary confirm IHD.\"
P019,No,Definite,,\"No IHD identified in available record review.\"
P020,Yes,Definite,2004-04-02,\"IHD and CABG before cohort baseline.\"
"), show_col_types = FALSE)

write_csv(emr_review, "data/raw/emr_review.csv")

# ------------------------------------------------------------
# Follow-up and death
# ------------------------------------------------------------

death_followup <- read_csv(I("
person_id,death_date_raw,cause_of_death_group,administrative_end_followup_raw
P001,,,2021-12-31
P002,,,2021-12-31
P003,,,2021-12-31
P004,,,2021-12-31
P005,,,2021-12-31
P006,,,2021-12-31
P007,,,2021-12-31
P008,2017-03-22,Cardiovascular,2021-12-31
P009,,,2021-12-31
P010,,,2021-12-31
P011,,,2021-12-31
P012,,,2021-12-31
P013,,,2021-12-31
P014,,,2021-12-31
P015,,,2021-12-31
P016,2015-01-03,Unknown,2021-12-31
P017,,,2021-12-31
P018,,,2021-12-31
P019,,,2021-12-31
P020,,,2021-12-31
"), show_col_types = FALSE)

write_csv(death_followup, "data/raw/death_followup.csv")

# ------------------------------------------------------------
# ICD harmonisation lookup
# ------------------------------------------------------------

icd_harmonisation_lookup <- read_csv(I("
coding_system,code_raw,code_standardised,clinical_group,include_broad_ihd,include_strict_ihd,comments
ICD-10,I20.9,I209,Angina pectoris,TRUE,FALSE,\"May require repeated outpatient contacts or supporting evidence.\"
ICD-10,I21.9,I219,Acute myocardial infarction,TRUE,TRUE,\"Usually a specific acute IHD event.\"
ICD-10,I22.0,I220,Subsequent myocardial infarction,TRUE,TRUE,\"Included in IHD group; interpretation depends on prior event history.\"
ICD-10,I25.1,I251,Chronic ischaemic heart disease,TRUE,TRUE,\"Can represent prevalent disease rather than incident IHD.\"
ICD-9,414.01,41401,Chronic ischaemic heart disease,TRUE,TRUE,\"Historical ICD-9 code mapped to IHD for teaching harmonisation.\"
ICD-10,I50.9,I509,Heart failure,FALSE,FALSE,\"Not IHD; included as competing cardiovascular diagnosis.\"
ICD-10,I63.9,I639,Cerebral infarction,FALSE,FALSE,\"Not IHD; included as competing cardiovascular diagnosis.\"
ICD-10,R07.4,R074,Chest pain unspecified,FALSE,FALSE,\"Symptom code; does not establish IHD.\"
ICD-10,J18.9,J189,Pneumonia unspecified,FALSE,FALSE,\"Non-cardiovascular diagnosis.\"
"), show_col_types = FALSE)

write_csv(
  icd_harmonisation_lookup,
  "data/raw/icd_harmonisation_lookup.csv"
)

# ------------------------------------------------------------
# SNOMED lookup
# ------------------------------------------------------------

snomed_lookup <- read_csv(I("
snomed_code,snomed_term,clinical_group,comments
53741008,Coronary arteriosclerosis,IHD,\"Clinical concept that may not map one-to-one to a particular ICD code.\"
22298006,Myocardial infarction,IHD,\"May describe prior or acute MI; date and clinical context are required.\"
57054005,Acute myocardial infarction,IHD,\"Acute clinical concept; still requires confirmation of timing and episode.\"
"), show_col_types = FALSE)

write_csv(snomed_lookup, "data/raw/snomed_lookup.csv")

# ------------------------------------------------------------
# Data dictionary
# ------------------------------------------------------------

data_dictionary <- read_csv(I("
dataset,variable,definition,unit_or_format,allowed_values_or_example,data_quality_note
cohort_baseline,person_id,Pseudonymous cohort participant identifier,Character,P001,Must be unique
cohort_baseline,baseline_date,Date of cohort examination,Date,2006-09-11,Predictors should precede outcome
cohort_baseline,systolic_bp_mmHg,Baseline systolic blood pressure,mmHg,120,Check plausible values
registry_contacts,contact_id,Registry contact identifier,Character,C001,Must identify one contact
registry_contacts,setting,Type of healthcare contact,Character,Inpatient or Outpatient,Missingness may occur
registry_diagnoses,diagnosis_position,Position of diagnosis within contact,Character,Primary or Secondary,Meaning may vary by registry
registry_diagnoses,diagnosis_date_raw,Raw diagnosis date string,Character,2012-05-18,Deliberately inconsistent formats
procedures,procedure_code,Simplified procedure code,Character,PCI or CAG,Supporting evidence rather than diagnosis alone
prescriptions,atc_code,ATC medicine classification code,Character,C10AA05,Dispensing is not adherence
emr_review,chart_review_ihd,IHD status from structured record review,Character,Yes or No,Provisional reference standard
emr_notes_excerpt,snomed_code,SNOMED CT concept identifier,Character,53741008,Not always complete or available
death_followup,administrative_end_followup_raw,Administrative end of observation,Date,2021-12-31,Used for censoring
"), show_col_types = FALSE)

write_csv(data_dictionary, "data/dictionary/data_dictionary.csv")

message("Synthetic data files generated successfully in data/raw/.")