library(tidyverse)
library(lubridate)
library(stringr)
library(stringi)

fix_txt <- function(x) {
  x <- iconv(as.character(x), from = "", to = "UTF-8", sub = "")
  x <- trimws(x)
  x[x == ""] <- NA_character_
  x
}

state_to_region <- function(state_abbrev) {
  northeast <- c("CT","ME","MA","NH","RI","VT","NJ","NY","PA")
  midwest   <- c("IL","IN","MI","OH","WI","IA","KS","MN","MO","NE","ND","SD")
  south     <- c("DE","FL","GA","MD","NC","SC","VA","DC","WV","AL","KY","MS","TN","AR","LA","OK","TX")
  west      <- c("AZ","CO","ID","MT","NV","NM","UT","WY","AK","CA","HI","OR","WA")
  
  dplyr::case_when(
    state_abbrev %in% northeast ~ "Northeast",
    state_abbrev %in% midwest   ~ "Midwest",
    state_abbrev %in% south     ~ "South",
    state_abbrev %in% west      ~ "West",
    TRUE                        ~ NA_character_
  )
}

consolidate_manu <- function(x) {
  x2 <- tolower(fix_txt(x))
  case_when(
    is.na(x2) ~ NA_character_,
    stri_detect_regex(x2, "pfizer|biontech") ~ "PFIZER",
    stri_detect_regex(x2, "moderna") ~ "MODERNA",
    stri_detect_regex(x2, "janssen|johnson") ~ "JANSSEN",
    stri_detect_regex(x2, "novavax") ~ "NOVAVAX",
    TRUE ~ "OTHER"
  )
}

clean_yesno_na_as_no <- function(x) {
  x2 <- toupper(fix_txt(x))
  out <- case_when(
    x2 %in% c("Y","YES") ~ "Yes",
    x2 %in% c("N","NO")  ~ "No",
    TRUE ~ NA_character_
  )
  replace_na(out, "No")
}

categorize_health_text <- function(x) {
  x2 <- tolower(fix_txt(x))
  case_when(
    is.na(x2) ~ NA_character_,
    stri_detect_regex(x2, "^(n/a|na|none|no|nil|not applicable)$") ~ "NONE",
    stri_detect_regex(x2, "autoimmune|lupus|ms\\b|rheumatoid") ~ "AUTOIMMUNE",
    stri_detect_regex(x2, "cardio|heart|hypertension|stroke") ~ "CARDIOVASCULAR",
    stri_detect_regex(x2, "diabet|metabolic") ~ "METABOLIC",
    stri_detect_regex(x2, "asthma|copd|respir") ~ "RESPIRATORY",
    stri_detect_regex(x2, "infection|flu|viral|bacterial|covid") ~ "INFECTIOUS",
    stri_detect_regex(x2, "depress|anxiety|bipolar|psych|neuro") ~ "NEURO_PSYCH",
    stri_detect_regex(x2, "chronic") ~ "OTHER_CHRONIC",
    TRUE ~ "OTHER"
  )
}

categorize_allergies <- function(x) {
  x2 <- tolower(fix_txt(x))
  case_when(
    is.na(x2) ~ NA_character_,
    stri_detect_regex(x2, "^(n/a|na|none|no|nil|not applicable)$") ~ "NONE",
    stri_detect_regex(x2, "antibiotic|penicillin") ~ "ANTIBIOTICS",
    stri_detect_regex(x2, "nsaid|ibuprofen|aspirin|pain") ~ "PAIN_MEDS_NSAIDS",
    stri_detect_regex(x2, "latex|material|environment") ~ "ENV_MATERIAL",
    stri_detect_regex(x2, "food|peanut|milk|egg|shellfish") ~ "FOOD",
    TRUE ~ "OTHER"
  )
}

clean_vaers_year <- function(data_file, vax_file) {
  
  year <- as.integer(str_extract(data_file, "\\d{4}"))
  
  data <- read_csv(data_file, show_col_types = FALSE)
  vax  <- read_csv(vax_file,  show_col_types = FALSE)
  
  data <- data %>% mutate(VAERS_ID = as.character(VAERS_ID), YEAR = year)
  vax  <- vax  %>% mutate(VAERS_ID = as.character(VAERS_ID))
  
  data <- data %>% mutate(across(where(is.character), fix_txt))
  vax  <- vax  %>% mutate(across(where(is.character), fix_txt))
  
  vax <- vax %>%
    mutate(
      VAX_TYPE = toupper(VAX_TYPE),
      VAX_MANU_CONSOL = consolidate_manu(VAX_MANU)
    )
  
  vax_summary <- vax %>%
    group_by(VAERS_ID) %>%
    summarise(
      NUM_VAX = n(),
      VAX_TYPE = paste(sort(unique(na.omit(VAX_TYPE))), collapse = ", "),
      VAX_NAME = paste(sort(unique(na.omit(VAX_NAME))), collapse = ", "),
      VAX_COVID = as.integer(any(VAX_TYPE == "COVID19", na.rm = TRUE)),
      COVID_MANU = if (any(VAX_TYPE == "COVID19", na.rm = TRUE)) {
        paste(sort(unique(na.omit(VAX_MANU_CONSOL[VAX_TYPE == "COVID19"]))), collapse = ", ")
      } else { NA_character_ },
      OTHER_MANU = if (any(VAX_TYPE != "COVID19", na.rm = TRUE)) {
        paste(sort(unique(na.omit(VAX_MANU_CONSOL[VAX_TYPE != "COVID19"]))), collapse = ", ")
      } else { NA_character_ },
      .groups = "drop"
    )
  
  vaers <- data %>%
    left_join(vax_summary, by = "VAERS_ID") %>%
    filter(!is.na(NUM_VAX))
  
  if ("SEX" %in% names(vaers)) vaers <- vaers %>% mutate(SEX = toupper(SEX))
  
  if ("STATE" %in% names(vaers)) {
    vaers <- vaers %>%
      mutate(REGION = state_to_region(toupper(STATE)))
  }
  
  date_cols <- intersect(c("RECVDATE","VAX_DATE","ONSET_DATE"), names(vaers))
  if (length(date_cols) > 0) vaers <- vaers %>% mutate(across(all_of(date_cols), ~ mdy(.x)))
  
  num_cols <- intersect(c("AGE_YRS","NUMDAYS"), names(vaers))
  if (length(num_cols) > 0) vaers <- vaers %>% mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))
  
  if ("AGE_YRS" %in% names(vaers)) {
    vaers <- vaers %>% mutate(AGE_YRS = ifelse(AGE_YRS < 0 | AGE_YRS > 120, NA, AGE_YRS))
    if (!all(is.na(vaers$AGE_YRS))) vaers$AGE_YRS[is.na(vaers$AGE_YRS)] <- median(vaers$AGE_YRS, na.rm = TRUE)
  }
  
  if ("NUMDAYS" %in% names(vaers) && !all(is.na(vaers$NUMDAYS))) {
    vaers$NUMDAYS[is.na(vaers$NUMDAYS)] <- median(vaers$NUMDAYS, na.rm = TRUE)
  }
  
  if ("CUR_ILL" %in% names(vaers)) vaers$CUR_ILL_CAT <- categorize_health_text(vaers$CUR_ILL)
  if ("HISTORY" %in% names(vaers)) vaers$HISTORY_CAT <- categorize_health_text(vaers$HISTORY)
  if ("OTHER_MEDS" %in% names(vaers)) vaers$OTHER_MEDS_CAT <- categorize_health_text(vaers$OTHER_MEDS)
  if ("ALLERGIES" %in% names(vaers)) vaers$ALLERGIES_CAT <- categorize_allergies(vaers$ALLERGIES)
  
  if ("PRIOR_VAX" %in% names(vaers)) {
    pv <- tolower(fix_txt(vaers$PRIOR_VAX))
    vaers$PRIOR_VAX <- case_when(
      is.na(pv) ~ "No",
      pv %in% c("n","no","na","n/a","none") ~ "No",
      TRUE ~ "Yes"
    )
  }
  
  outcome_vars <- intersect(c("DIED","L_THREAT","ER_VISIT","ER_ED_VISIT","HOSPITAL","X_STAY","DISABLE"), names(vaers))
  if (length(outcome_vars) > 0) vaers <- vaers %>% mutate(across(all_of(outcome_vars), clean_yesno_na_as_no))
  
  vaers <- vaers %>%
    mutate(
      SEVERE = as.integer(rowSums(across(all_of(outcome_vars), ~ .x == "Yes"), na.rm = TRUE) > 0),
      SEVERE = replace_na(SEVERE, 0)
    )
  
  drop_cols <- intersect(c(
    "CUR_ILL","HISTORY","OTHER_MEDS","ALLERGIES",
    "LAB_DATA","SYMPTOM_TEXT",
    "RPT_DATE","TODAYS_DATE","DATEDIED","RECOVD","V_FUNDBY",
    "SPLTTYPE","ORDER","FORM_TYPE","FORM_VERS", "VAX_TYPE","VAX_NAME",
    "CAGE_YR","CAGE_MO","HOSPDAYS"
  ), names(vaers))
  if (length(drop_cols) > 0) vaers <- vaers %>% select(-all_of(drop_cols))
  
  desired_order <- c(
    "VAERS_ID","SEVERE",
    "YEAR","AGE_YRS","SEX","STATE","REGION",
    "VAX_DATE","RECVDATE", "ONSET_DATE","NUMDAYS","V_ADMINBY",
    "NUM_VAX","VAX_COVID",
    "COVID_MANU","OTHER_MANU",
    "PRIOR_VAX",
    "CUR_ILL_CAT","HISTORY_CAT","OTHER_MEDS_CAT","ALLERGIES_CAT",
    "DIED","L_THREAT","ER_VISIT","HOSPITAL","DISABLE","ER_ED_VISIT"
  )
  
  vaers <- vaers %>% select(any_of(desired_order), everything())
  
  write_csv(vaers, paste0("vaers_cleaned_", year, ".csv"))
  vaers
}

years <- 2021:2024
all_clean <- map_dfr(years, function(y) {
  clean_vaers_year(paste0(y, "VAERSDATA.csv"), paste0(y, "VAERSVAX.csv"))
})

write_csv(all_clean, "vaers_cleaned_2021_2024.csv")
print(table(all_clean$YEAR))
print(table(all_clean$VAX_COVID, useNA = "ifany"))
print(table(all_clean$SEVERE, useNA = "ifany"))
