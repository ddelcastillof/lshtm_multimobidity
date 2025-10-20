########################################
# Title: Data Cleaning and Exploration #
# Author: Darwin Del Castillo          #
########################################

# Importing datasets
afib <- fread("data_raw/afib.csv")
cases_drugs <- fread("data_raw/cases_drugs.csv")
cases_medical <- fread("data_raw/cases_medical.csv")
cases_personal <- fread("data_raw/cases_personal.csv")
pneumo <- fread("data_raw/pneumo.csv")


## Data Cleaning ##
### Exploring datasets
glimpse(afib)
glimpse(cases_drugs)
glimpse(cases_medical)
glimpse(cases_personal)
glimpse(pneumo)

skim(afib)
skim(pneumo)
skim(cases_drugs)
skim(cases_medical)
skim(cases_personal)

### medcode in pneumo is numeric, so is better to convert it to char for joining operations
pneumo <- pneumo |>
  mutate(medcode = as.character(medcode))

### same with medcode in cases_medical and cases_drugs
cases_medical <- cases_medical |>
  mutate(medcode = as.character(medcode))

cases_drugs <- cases_drugs |>
  mutate(medcode = as.character(medcode))

### patid in cases_personal, cases_medical and cases_drugs are numeric
cases_personal <- cases_personal |>
  mutate(patid = as.character(patid))

cases_medical <- cases_medical |>
  mutate(patid = as.character(patid))

cases_drugs <- cases_drugs |>
  mutate(patid = as.character(patid))

### date columns to Date format with lubridate (evntdate and dob)
cases_medical <- cases_medical |>
  mutate(evntdate_m = dmy(evntdate))

cases_drugs <- cases_drugs |>
  mutate(evntdate_m = dmy(evntdate))

cases_personal <- cases_personal |>
  mutate(dob_m = dmy(dob))

## checking for duplicated patients in cases_personal
dup_personal <- get_dupes(cases_personal)

### there is a duplicate obs in cases_personal, so we will keep unique patid/dob entries

cases_personal <- cases_personal |>
  distinct(patid, dob_m, .keep_all = TRUE)