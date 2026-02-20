library(tidyverse)

# Using the cleaned csv files
df2018 = read_csv("data/cleaned_data/vaers_cleaned_2018.csv")
df2019 = read_csv("data/cleaned_data/vaers_cleaned_2019.csv")
df2020 = read_csv("data/cleaned_data/vaers_cleaned_2020.csv")
df2021 = read_csv("data/cleaned_data/vaers_cleaned_2021.csv")
df2022 = read_csv("data/cleaned_data/vaers_cleaned_2022.csv")
df2023 = read_csv("data/cleaned_data/vaers_cleaned_2023.csv")

# combining all the files together and getting rid of the narrative columns
df = rbind(df2018, df2019, df2020, df2021, df2022, df2023) |>
  select(-c("LAB_DATA", "SYMPTOM_TEXT"))

# Fining the percent missing
miss_per = colMeans(is.na(df))
round(miss_per, 2)


# Finding all the columns with more than 50% NA
cols_to_take_out = names(miss_per[miss_per > .5])
cols_to_take_out

# Seeing if HOSPDAYS is needed (are there any entries where hospday is filled, but hospital is not)
is_HOSPDAYS_needed = df |>
  select(HOSPITAL, HOSPDAYS) |>
  filter(!is.na(HOSPITAL) | !is.na(HOSPDAYS)) |>
  filter(is.na(HOSPITAL) & !is.na(HOSPDAYS))
is_HOSPDAYS_needed ## NO. We do not need it for a response indicator

# Creating names of all variables that contribute to the response 
response_indicators = c("DIED", "L_THREAT", "ER_VISIT", "HOSPITAL", "DISABLE", "ER_ED_VISIT")

# Making sure that the columns that contribute to the response stay in the data (even though we will not use them)
cols_to_take_out  = setdiff(cols_to_take_out, response_indicators)
cols_to_take_out = cols_to_take_out[-which(cols_to_take_out == "ALLERGIES")]

# Remove columns with more that 50% missing 
df = df |> select(-cols_to_take_out)

df_temp1 = df |> select("STATE", "OTHER_MEDS", "HISTORY", "ALLERGIES")
df_temp2 = df |> select("VAX_TYPE", "VAX_NAME", "VAX_MANU")

#write_csv(df_temp1, "df_temp1.csv")
#rite_csv(df_temp2, "df_temp2.csv")

# Using AI (Google Gemini) to find factors for the character/factor values (including an NA factor)
#=====================================================================================


# Apply to your dataframe
df_factored[c("HISTORY", "ALLERGIES")] <- df[c("HISTORY", "ALLERGIES")]

# --- 2. FILE 1 CATEGORIZATION ---
# Note: Using case_when, which uses the FIRST True value. The following are arranged by importance to investigators

df_factored <- df_factored |>
  mutate(
    # State Regions
    REGION = case_when(
      STATE %in% c('CT', 'ME', 'MA', 'NH', 'RI', 'VT', 'NJ', 'NY', 'PA') ~ "Northeast",
      STATE %in% c('IL', 'IN', 'IA', 'KS', 'MI', 'MN', 'MO', 'NE', 'ND', 'OH', 'SD', 'WI') ~ "Midwest",
      STATE %in% c('AL', 'AR', 'DE', 'FL', 'GA', 'KY', 'LA', 'MD', 'MS', 'NC', 'OK', 'SC', 'TN', 'TX', 'VA', 'WV', 'DC') ~ "South",
      STATE %in% c('AK', 'AZ', 'CA', 'CO', 'HI', 'ID', 'MT', 'NV', 'NM', 'OR', 'UT', 'WA', 'WY') ~ "West",
      is.na(STATE) ~ NA_character_,
      TRUE ~ "Other"
    ),
    
    # Medication Classes
    OTHER_MEDS = case_when(
      is.na(OTHER_MEDS) ~ NA_character_, 
      str_detect(str_to_lower(OTHER_MEDS), "lisinopril|amlodipine|atorvastatin|warfarin|statin|blood pressure|aspirin") ~ "Cardiovascular",
      str_detect(str_to_lower(OTHER_MEDS), "fluoxetine|carbamazepine|levetiracetam|ambien|sertraline|xanax") ~ "Neuro/Psych",
      str_detect(str_to_lower(OTHER_MEDS), "vitamin|calcium|magnesium|d3|b12|iron|supplement") ~ "Supplements",
      str_detect(str_to_lower(OTHER_MEDS), "albuterol|prednisone") ~ "Resp",
      str_detect(str_to_lower(OTHER_MEDS), "advil|motrin|ibuprofen|tylenol") ~ "Anti-Inflam",
      str_detect(str_to_lower(OTHER_MEDS), "no|none|none known|nka|none stated|nkda|non") ~ "None",
      str_detect(str_to_lower(OTHER_MEDS), "unk|unknown|unk|n/a|na") ~ "UNK",
      TRUE ~ "Other"
    )
  ) 

# --- 3. FILE 2 CATEGORIZATION ---
df_factored <- df_factored %>%
  mutate(
    # Vaccine Types
    VAX_TYPE = case_when(
      str_detect(VAX_TYPE, "COVID") ~ "COVID",
      str_detect(VAX_TYPE, "FLU") ~ "Flu",
      str_detect(VAX_TYPE, "PPV|PCV|PNEUMO") ~ "Pneumo",
      str_detect(VAX_TYPE, "VARZOS|SHINGRIX") ~ "Zoster",
      str_detect(VAX_TYPE, "DTAP|MMR|RV1|HIB|IPV|HEP|MEN") ~ "Pediatric",
      str_detect(str_to_lower(OTHER_MEDS), "no|none|none known|nka|none stated|nkda") ~ "None",
      str_detect(str_to_lower(OTHER_MEDS), "unk|unknown|unk|n/a|na") ~ "UNK",
      is.na(ALLERGIES) ~ NA_character_,
      TRUE ~ "Other"
    ),
    
    # VAX_NAME is too broad (and Vax type gives similar information)
    # # Vaccine Mechanisms
    # VAX_NAME = case_when(
    #   str_detect(VAX_NAME, "PFIZER|MODERNA|MRNA") ~ "mRNA",
    #   str_detect(VAX_NAME, "INFLUENZA|POLIO|HEPATITIS|INACTIVATED") ~ "Inactivated",
    #   str_detect(VAX_NAME, "MMR|ROTAVIRUS|VARICELLA|LIVE") ~ "Live Attenuated",
    #   str_detect(VAX_NAME, "ZOSTER|TDAP|MENINGOCOCCAL|RECOMBINANT") ~ "Recombinant/Protein",
    #   is.na(VAX_NAME) ~ NA_character_,
    #   TRUE ~ "Other/Not Specified"
    # ),
    
    # Manufacturers
    VAX_MANU = case_when(
      str_detect(VAX_MANU, "PFIZER|BIONTECH") ~ "Pfizer/BioNTech",
      str_detect(VAX_MANU, "MODERNA") ~ "Moderna",
      str_detect(VAX_MANU, "JANSSEN") ~ "JANSSEN",
      str_detect(VAX_MANU, "SANOFI") ~ "Sanofi Pasteur",
      str_detect(VAX_MANU, "GSK|GLAXO") ~ "GSK",
      str_detect(VAX_MANU, "MERCK|NOVARTIS|SEQIRUS|MEDIMMUNE") ~ "Other Major",
      str_detect(str_to_lower(OTHER_MEDS), "no|none|none known|nka|none stated|nkda") ~ "None",
      str_detect(str_to_lower(OTHER_MEDS), "unk|unknown|unk|n/a|na") ~ "UNK",
      is.na(ALLERGIES) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

df_factored <- df_factored |>
  mutate(
    # Medical History Systems
    HISTORY = case_when(
      str_detect(str_to_lower(HISTORY), "ms|multiple sclerosis|autoimmune|lupus|arthritis") ~ "Autoimmune",
      str_detect(str_to_lower(HISTORY), "cancer") ~ "Cancer",
      str_detect(str_to_lower(HISTORY), "diabetes|hypothyroidism|thyroid|metabolic") ~ "Metabolic",
      str_detect(str_to_lower(HISTORY), "hypertension|afib|clotting|cholesterol|heart|cardiac") ~ "Cardiovascular",
      str_detect(str_to_lower(HISTORY), "asthma|copd|respiratory|lung") ~ "Respiratory",
      str_detect(str_to_lower(HISTORY), "no|none|none known|nka|none stated|nkda") ~ "None",
      str_detect(str_to_lower(HISTORY), "unk|unknown|unk|n/a|na") ~ "UNK",
      is.na(HISTORY) ~ NA_character_, 
      TRUE ~ "Other"
    ),
    
    # Allergy Triggers
    ALLERGIES = case_when(
      str_detect(str_to_lower(ALLERGIES), "amoxicillin|penicillin|sulfa|antibiotic") ~ "Antibiotics",
      str_detect(str_to_lower(ALLERGIES), "tylenol|codeine|aspirin|ibuprofen|nsaid|morphine") ~ "Pain Meds/NSAIDs",
      str_detect(str_to_lower(ALLERGIES), "latex|nickel|betadine|tape|pollen|environmental") ~ "Env/Material",
      str_detect(str_to_lower(ALLERGIES), "peanut|shellfish|egg|dairy") ~ "Food",
      str_detect(str_to_lower(OTHER_MEDS), "no|none|none known|nka|none stated|nkda|non") ~ "None",
      str_detect(str_to_lower(OTHER_MEDS), "unk|unknown|unk|n/a|na") ~ "UNK",
      is.na(ALLERGIES) ~ NA_character_,
      TRUE ~ "Other"
    )
  )
#================================================

# Losing state, region is easier to have as a predictor
df_factored = df_factored |> select(-"STATE")

# Cleaning up column types
df_factored = df_factored |>
  mutate(across(c(SEX, RECOVD, V_ADMINBY, OTHER_MEDS, HISTORY, ALLERGIES, VAX_TYPE, VAX_MANU, REGION), as.factor)) |>
  mutate(across(c(RECVDATE, VAX_DATE, ONSET_DATE), lubridate::ymd)) |>
  mutate(across(c(AGE_YRS, NUMDAYS, YEAR, NUM_VAX, SEVERE), as.numeric)) |>
  select(-c(TODAYS_DATE, VAX_NAME, RECVDATE))

# Re-arranging column order: ID, Response, Predictors, Response indicators
df_factored <- df_factored[, c("VAERS_ID", "SEVERE", "YEAR", "AGE_YRS", "SEX", "REGION", "VAX_DATE", "ONSET_DATE", "NUMDAYS", "V_ADMINBY", "NUM_VAX", "VAX_TYPE", "VAX_MANU", "HISTORY", "ALLERGIES", "OTHER_MEDS", "RECOVD", "DIED", "L_THREAT", "ER_VISIT", "HOSPITAL", "DISABLE", "ER_ED_VISIT")]
df_factored

#===============================================================================
# Imputing values with missing <20%
round(colMeans(is.na(df_factored)), 2)

library(missRanger)

# 1. Select only the columns we WANT to use/impute
# We add NUMDAYS here instead of the raw dates
cols_to_impute <- c("AGE_YRS", "REGION", "VAX_TYPE", "VAX_MANU", "SEX", "NUM_VAX", "NUMDAYS")

# 2. Run the imputation
# We use pmm.k = 3 to ensure NUMDAYS doesn't become a negative number or a decimal
df_imputed_sub <- missRanger(
  df_factored[, cols_to_impute], 
  formula = . ~ ., 
  num.trees = 50,     # reduced from 500 from a typical missRanger imputation due to amount of data  
  pmm.k = 3,          # Keeps values (somewhat) realistic by iterating 3 times
  seed = 42,
  verbose = 1
)

# 3. Transfer the clean data back to your main dataframe
df_factored$AGE_YRS  <- df_imputed_sub$AGE_YRS
df_factored$REGION   <- df_imputed_sub$REGION
df_factored$VAX_TYPE <- df_imputed_sub$VAX_TYPE
df_factored$VAX_MANU <- df_imputed_sub$VAX_MANU
df_factored$NUM_VAX  <- df_imputed_sub$NUM_VAX
df_factored$NUMDAYS  <- df_imputed_sub$NUMDAYS


# Saving csv file
write_csv(df_factored, "imputed_df.csv")


