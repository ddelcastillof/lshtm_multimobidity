##########################################
# Title: Summary info of new variables   #
# Author: Darwin Del Castillo            #
##########################################

## This code will generate summary tables for the final dataset
## These will be printed with flextable for direct inclusion in Word documents
## This is the commented code chunk in output.qmd

### Creating summary table for personal information
total_participants <- n_distinct(cases_personal$patid)

summary_personal <- cases_personal |>
  select(sex, dob_m) |>
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))) |>
  tbl_summary(
    by = NULL,
    type = list(sex ~ "categorical",
                dob_m ~ "continuous2"),
    statistic = list(
      sex ~ "{n} ({p}%)",
      dob_m ~ c("{min}", "{max}")
    ),
    digits = list(
      sex ~ c(0, 1)
    ),
    label = list(
      sex = "Sex",
      dob_m = "Date of Birth"
    ),
    missing = "ifany"
  ) |>
  modify_header(stat_0 ~ "") |>
  modify_table_body(
    ~ bind_rows(
      tibble(
        row_type = "label",
        variable = "total_participants",
        label = "Number of participants",
        stat_0 = format(total_participants, big.mark = ",")
      ),
      .x
    )
  ) |>
  modify_table_styling(footnote_abbrev = FALSE) |>
  modify_table_styling(footnote = list(NULL)) |>
  modify_footnote(update = everything() ~ NA_character_)


### Creating summary table for characteristics of atrial fibrillation patients
afib_count <- n_distinct(enhanced_medical_records$patid[enhanced_medical_records$atrial_fib == 1])

afib_patients <- enhanced_medical_records |>
  filter(atrial_fib == 1) |>
  distinct(patid, .keep_all = TRUE) |>
  select(age_afib, evntdate_afib, sex) |>
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))) |>
  tbl_summary(
    type = list(
      age_afib ~ "continuous2",
      evntdate_afib ~ "continuous2",
      sex ~ "categorical"
    ),
    statistic = list(
      age_afib ~ c("{mean} ({sd})", "{min}", "{max}"),
      evntdate_afib ~ c("{min}", "{max}"),
      sex ~ "{n} ({p}%)"
    ),
    digits = list(
      age_afib ~ c(1, 1, 0, 0),
      sex ~ c(0, 1)
    ),
    label = list(
      age_afib = "Age at first atrial fibrillation diagnosis",
      evntdate_afib = "Date of first atrial fibrillation diagnosis",
      sex = "Sex"
    ),
    missing = "ifany"
  ) |>
  modify_header(stat_0 ~ "") |>
  modify_table_body(
    ~ bind_rows(
      tibble(
        row_type = "label",
        variable = "afib_count",
        label = "Number of patients with atrial fibrillation",
        stat_0 = scales::comma(afib_count)
      ),
      .x
    )
  ) |>
  modify_table_styling(footnote_abbrev = FALSE) |>
  modify_table_styling(footnote = list(NULL)) |>
  modify_footnote(update = everything() ~ NA_character_)

### Creating summary table for characteristics of pneumococcal vaccinated patients

pneumo_count <- n_distinct(enhanced_medical_records$patid[!is.na(enhanced_medical_records$evntdate_pneumo)])

pneumo_patients <- enhanced_medical_records |>
  filter(!is.na(evntdate_pneumo)) |>
  distinct(patid, .keep_all = TRUE) |>
  select(evntdate_pneumo, n_pneumo_days, sex) |>
  mutate(sex = factor(sex, levels = c(1, 2), labels = c("Male", "Female"))) |>
  tbl_summary(
    type = list(
      evntdate_pneumo ~ "continuous2",
      n_pneumo_days ~ "continuous2",
      sex ~ "categorical"
    ),
    statistic = list(
      evntdate_pneumo ~ c("{min}", "{max}"),
      n_pneumo_days ~ c("{min}", "{max}"),
      sex ~ "{n} ({p}%)"
    ),
    digits = list(
      n_pneumo_days ~ c(0, 0),
      sex ~ c(0, 1)
    ),
    label = list(
      evntdate_pneumo = "Date of pneumococcal vaccination",
      n_pneumo_days = "Number of pneumococcal consultation days",
      sex = "Sex"
    ),
    missing = "ifany"
  ) |>
  modify_header(stat_0 ~ "") |>
  modify_table_body(
    ~ bind_rows(
      tibble(
        row_type = "label",
        variable = "pneumo_count",
        label = "Number of patients with pneumococcal vaccination",
        stat_0 = scales::comma(pneumo_count)
      ),
      .x
    )
  ) |>
  modify_footnote(update = everything() ~ NA_character_) |>
  modify_table_styling(footnote_abbrev = FALSE)
