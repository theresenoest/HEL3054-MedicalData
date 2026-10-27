# Synthetic raw data

All files in this directory are fictional and created only for teaching.

## Deliberately included data challenges

- Multiple levels of data: person, contact, diagnosis, procedure, note, review.
- One-to-many joins.
- Outpatient and inpatient contacts.
- Primary and secondary diagnoses.
- ICD-9 and ICD-10 codes.
- SNOMED concepts in EMR excerpts.
- A historical myocardial infarction recorded after baseline.
- IHD before cohort baseline.
- One outpatient angina diagnosis where IHD is ruled out.
- Repeated outpatient diagnoses supporting IHD.
- A hospital transfer that can generate duplicate event records.
- An incomplete date: `2014-??-12`.
- A discharge date before admission date.
- Heart failure and stroke codes that should not automatically be classified as IHD.
- Clinically documented IHD with no registry IHD record.