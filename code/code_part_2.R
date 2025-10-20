###################################
# Title: Enhanced medical records #
# Author: Darwin Del Castillo     #
###################################

# Generating enhanced medical records dataset by leveraging independent datasets

## Identifying from therapy records those who received pneumococcal vaccines
## There are multiple events per patients that correspond to different doses
## Selecting distinct patid-date combinations (taking first dose date)
therapy_vaccine <- cases_drugs |>
  inner_join(pneumo, by = "medcode") |>
  group_by(patid) |>
  summarise(evntdate_pneumo = min(evntdate_m), .groups = "drop")

### Adding personal information for every individual and pneumococcal vaccine where available
enhanced_medical_records <- cases_medical |>
  left_join(cases_personal, by = "patid") |>
  left_join(therapy_vaccine, by = "patid")

### Identifying first atrial fibrillation diagnosis per patient
afib_first <- cases_medical |>
  filter(medcode %in% afib$medcode) |>
  group_by(patid) |>
  summarise(evntdate_afib = min(evntdate_m), .groups = "drop")

### Creating atrial fibrillation indicators and derived age at first diagnosis
enhanced_medical_records <- enhanced_medical_records |>
  left_join(afib_first, by = "patid") |>
  mutate(
    atrial_fib = if_else(!is.na(evntdate_afib), 1L, 0L),
    age_afib = if_else(
      atrial_fib == 1 & !is.na(dob_m) & evntdate_afib >= dob_m,
      floor(time_length(dob_m %--% evntdate_afib, "years")),
      NA_integer_
    )
  )

### Detecting patients with inconsistencies in dob and atrial fibrillation date
### Showing the patid and the dates involved (one row per patient)
inconsistent_afib_dates <- enhanced_medical_records |>
  filter(
    atrial_fib == 1,
    is.na(dob_m) | (!is.na(evntdate_afib) & evntdate_afib < dob_m)
  ) |>
  distinct(patid, dob_m, evntdate_afib)

#### Two patients with inconsistent afib dates detected
#### I will transform into NA the inconsistent evntdate_afib values
enhanced_medical_records <- enhanced_medical_records |>
  mutate(
    afib_date_valid = atrial_fib == 1 &
      !is.na(evntdate_afib) &
      !is.na(dob_m) &
      evntdate_afib >= dob_m,
    evntdate_afib = if_else(afib_date_valid, evntdate_afib, as.Date(NA)),
    age_afib = if_else(
      afib_date_valid,
      floor(time_length(dob_m %--% evntdate_afib, "years")),
      NA_integer_
    )
  ) |>
  select(-afib_date_valid)

inconsistent_pneumo_dates <- enhanced_medical_records |>
  filter(
    atrial_fib == 1,
    is.na(dob_m) | (!is.na(evntdate_pneumo) & evntdate_pneumo < dob_m)
  ) |>
  distinct(patid, dob_m, evntdate_pneumo)

#### No patients with inconsistent pneumo dates detected

#### In a real scenario, I would solve these inconsistencies with death codes
#### Or creating a function to detect the most proximal event to the cutoff date
#### So dates prior to death or cutoff would be valid event dates
#### Another option is multiple imputation for missing time to event
#### For the sake of this exercise, I won't correct these inconsistencies

### Final step: generating variables for the total number of consultation days
#### Days for pneumococcal vaccine
pneumo_consults <- cases_drugs |>
  inner_join(pneumo, by = "medcode") |>
  summarise(n_pneumo_days = dplyr::n_distinct(evntdate_m), .by = patid)

### Days for atrial fibrillation attentions
afib_consults <- cases_medical |>
  inner_join(afib, by = "medcode") |>
  summarise(n_afib_days = dplyr::n_distinct(evntdate_m), .by = patid)

### Merging these new variables to the enhanced medical records dataset
enhanced_medical_records <- enhanced_medical_records |>
  left_join(pneumo_consults, by = "patid") |>
  left_join(afib_consults, by = "patid")

#### Saving the enhanced medical records dataset
write_csv(
  enhanced_medical_records,
  "output/enhanced_case_medical.csv"
)