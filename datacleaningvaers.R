library(tidyverse)
library(lubridate)
library(stringr)

clean_vaers_year <- function(data_file, vax_file) {
  
  year <- as.integer(str_extract(data_file, "\\d{4}"))
  
  data <- read_csv(data_file, show_col_types = FALSE)
  vax  <- read_csv(vax_file,  show_col_types = FALSE)
  
  data <- data %>% mutate(VAERS_ID = as.character(VAERS_ID))
  vax  <- vax  %>% mutate(VAERS_ID = as.character(VAERS_ID))
  
  data <- data %>% mutate(YEAR = year)
  
  vax_summary <- vax %>%
    group_by(VAERS_ID) %>%
    summarise(
      NUM_VAX  = n(),
      VAX_TYPE = paste(unique(VAX_TYPE), collapse = ", "),
      VAX_MANU = paste(unique(VAX_MANU), collapse = ", "),
      VAX_NAME = paste(unique(VAX_NAME), collapse = ", "),
      .groups = "drop"
    ) %>%
    mutate(VAERS_ID = as.character(VAERS_ID))  
  
  vaers <- data %>%
    left_join(vax_summary, by = "VAERS_ID")
  
  vaers <- vaers %>%
    mutate(across(where(is.character), ~na_if(., "")))
  
  if ("SEX" %in% names(vaers))   vaers <- vaers %>% mutate(SEX = str_to_upper(SEX))
  if ("STATE" %in% names(vaers)) vaers <- vaers %>% mutate(STATE = str_to_upper(STATE))
  
  date_cols <- intersect(c("RECVDATE", "RPT_DATE", "VAX_DATE", "ONSET_DATE", "DATEDIED"),
                         names(vaers))
  if (length(date_cols) > 0) {
    vaers <- vaers %>% mutate(across(all_of(date_cols), ~ mdy(.x)))
  }
  
  num_cols <- intersect(c("AGE_YRS", "NUMDAYS", "HOSPDAYS"), names(vaers))
  if (length(num_cols) > 0) {
    vaers <- vaers %>% mutate(across(all_of(num_cols), ~ suppressWarnings(as.numeric(.x))))
  }
  
  if ("AGE_YRS" %in% names(vaers)) {
    vaers <- vaers %>%
      mutate(AGE_YRS = ifelse(AGE_YRS < 0 | AGE_YRS > 120, NA, AGE_YRS))
  }
  
  yesno_vars <- intersect(
    c("DIED", "L_THREAT", "HOSPITAL", "X_STAY", "DISABLE",
      "ER_VISIT", "ER_ED_VISIT", "OFC_VISIT", "RECOVD", "BIRTH_DEFECT"),
    names(vaers)
  )
  
  if (length(yesno_vars) > 0) {
    vaers <- vaers %>%
      mutate(across(all_of(yesno_vars), ~ case_when(
        str_to_upper(str_trim(.x)) %in% c("Y", "YES") ~ "Yes",
        str_to_upper(str_trim(.x)) %in% c("N", "NO")  ~ "No",
        TRUE ~ NA_character_
      )))
  }
  
  drop_cols <- intersect(c("CAGE_YR", "CAGE_MO", "SPLTTYPE", "ORDER", "FORM_VERS"),
                         names(vaers))
  if (length(drop_cols) > 0) {
    vaers <- vaers %>% select(-all_of(drop_cols))
  }
  
  sev_cols <- intersect(c("DIED", "L_THREAT", "HOSPITAL", "ER_VISIT", "ER_ED_VISIT", "X_STAY", "DISABLE"),
                        names(vaers))
  
  vaers <- vaers %>%
    mutate(
      SEVERE = as.integer(
        rowSums(across(all_of(sev_cols), ~ .x == "Yes"), na.rm = TRUE) > 0 |
          (("HOSPDAYS" %in% names(vaers)) & !is.na(HOSPDAYS) & HOSPDAYS > 0)
      ),
      SEVERE = replace_na(SEVERE, 0)
    )
  
  out_file <- paste0("vaers_cleaned_", year, ".csv")
  write_csv(vaers, out_file)
  
  message("Finished ", year,
          " | saved: ", out_file,
          " | rows: ", nrow(vaers),
          " | severe=1: ", sum(vaers$SEVERE == 1, na.rm = TRUE))
  
  vaers
}

years <- 2018:2023

all_clean <- map_dfr(years, function(y) {
  data_file <- paste0(y, "VAERSDATA.csv")
  vax_file  <- paste0(y, "VAERSVAX.csv")
  clean_vaers_year(data_file, vax_file)
})

write_csv(all_clean, "vaers_cleaned_2018_2023.csv")

print(table(all_clean$YEAR))
print(table(all_clean$SEVERE, useNA = "ifany"))
