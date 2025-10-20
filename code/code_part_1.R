########################################
# Title: Data Cleaning and Exploration #
# Author: Darwin Del Castillo          #
########################################

# Load necessary packages
pacman::p_load(tidyverse,
               skimr)


# Importing datasets
afib <- read_csv("data_raw/afib.csv")
cases_drugs <- read_csv("data_raw/cases_drugs.csv")
cases_medical <- read_csv("data_raw/cases_medical.csv")
cases_personal <- read_csv("data_raw/cases_personal.csv")
pneumo <- read_csv("data_raw/pneumo.csv")


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


