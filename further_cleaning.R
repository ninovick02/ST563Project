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
  select(-c("LAB_DATA", "SYMPTOM_TEXT")) |> # Assumption 2
  select(-c("RPT_DATE", "TODAYS_DATE")) |> # Assumption 3
  select(-c("DATEDIED", "RECOVD")) |> #  Assumption 4
  select(-V_FUNDBY) # Assumption 7

# Assumption 6
# Seeing if HOSPDAYS is needed (are there any entries where hospday is filled, but hospital is not)
is_HOSPDAYS_needed = df |>
  select(HOSPITAL, HOSPDAYS) |>
  filter(!is.na(HOSPITAL) | !is.na(HOSPDAYS)) |>
  filter(is.na(HOSPITAL) & !is.na(HOSPDAYS))
is_HOSPDAYS_needed ## NO. We do not need it for a response indicator

df = df |> select(-HOSPDAYS) 


# Assumption 8: Replacing NA with NO for binary Y/NA variables
# Switching to a new df in case I mess this up
df1 = df
binary_na = c("DIED", "L_THREAT", "ER_VISIT", "ER_ED_VISIT", "HOSPITAL", "X_STAY", "DISABLE", "BIRTH_DEFECT", "OFC_VISIT")

df1 <- df |>
  mutate(across(all_of(binary_na), ~replace_na(., "NO")))

df1$PRIOR_VAX <- if_else(is.na(df1$PRIOR_VAX), "No", "Yes")

# Fining the percent missing
miss_per = colMeans(is.na(df1))
round(miss_per[miss_per > 0], 2)

#write_csv(df1["CUR_ILL"], "current_illness.csv")

# Using AI (Google Gemini) to find factors for the character/factor values (including an NA factor)
#=====================================================================================


# creating checkpoint
df_factored = df1

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
    ))

df2 = df_factored # Checkpoint

df_factored <- df_factored |>
  mutate(
    # Medication Classes
    OTHER_MEDS = case_when(
      is.na(OTHER_MEDS) ~ NA_character_, 
      str_detect(str_to_lower(OTHER_MEDS), "lisinopril|amlodipine|atorvastatin|warfarin|statin|blood pressure|aspirin") ~ "Cardiovascular",
      str_detect(str_to_lower(OTHER_MEDS), "fluoxetine|carbamazepine|levetiracetam|ambien|sertraline|xanax") ~ "Neuro/Psych",
      str_detect(str_to_lower(OTHER_MEDS), "albuterol|prednisone") ~ "Resp",
      str_detect(str_to_lower(OTHER_MEDS), "advil|motrin|ibuprofen|tylenol") ~ "Anti-Inflam",
      str_detect(str_to_lower(OTHER_MEDS), "vitamin|calcium|magnesium|d3|b12|iron|supplement") ~ "Supplements",
      str_detect(str_to_lower(OTHER_MEDS), "no|none|none known|nka|none stated|nkda|non|n/a|na") ~ "None",
      str_detect(str_to_lower(OTHER_MEDS), "unk|unknown|unk") ~ "UNK",
      TRUE ~ "Other"
    )
  ) 

df3 = df_factored # Checkpoint

df_factored <- df_factored %>%
  mutate(
    # Vaccine Types
    VAX_TYPE = case_when(
      str_detect(VAX_TYPE, "COVID") ~ "COVID",
      str_detect(VAX_TYPE, "FLU") ~ "Flu",
      str_detect(VAX_TYPE, "PPV|PCV|PNEUMO") ~ "Pneumo",
      str_detect(VAX_TYPE, "VARZOS|SHINGRIX") ~ "Zoster",
      str_detect(VAX_TYPE, "DTAP|MMR|RV1|HIB|IPV|HEP|MEN") ~ "Pediatric",
      str_detect(str_to_lower(OTHER_MEDS), "no|none|none known|nka|none stated|nkda|n/a|na") ~ "None",
      str_detect(str_to_lower(OTHER_MEDS), "unk|unknown|unk") ~ "UNK",
      is.na(ALLERGIES) ~ NA_character_,
      TRUE ~ "Other"
    ),
    
    # Manufacturers
    VAX_MANU = case_when(
      str_detect(VAX_MANU, "PFIZER|BIONTECH") ~ "Pfizer/BioNTech",
      str_detect(VAX_MANU, "MODERNA") ~ "Moderna",
      str_detect(VAX_MANU, "JANSSEN") ~ "JANSSEN",
      str_detect(VAX_MANU, "SANOFI") ~ "Sanofi Pasteur",
      str_detect(VAX_MANU, "GSK|GLAXO") ~ "GSK",
      str_detect(VAX_MANU, "MERCK|NOVARTIS|SEQIRUS|MEDIMMUNE") ~ "Other Major",
      str_detect(str_to_lower(OTHER_MEDS), "no|none|none known|nka|none stated|nkda|n\a|na") ~ "None",
      str_detect(str_to_lower(OTHER_MEDS), "unk|unknown|unk") ~ "UNK",
      is.na(ALLERGIES) ~ NA_character_,
      TRUE ~ "Other"
    )
  )
df4 = df_factored # Checkpoint

df_factored <- df_factored |>
  mutate(
    # Medical History Systems
    HISTORY = case_when(
      str_detect(str_to_lower(HISTORY), "ms|multiple sclerosis|autoimmune|lupus|arthritis") ~ "Autoimmune",
      str_detect(str_to_lower(HISTORY), "cancer") ~ "Cancer",
      str_detect(str_to_lower(HISTORY), "diabetes|hypothyroidism|thyroid|metabolic") ~ "Metabolic",
      str_detect(str_to_lower(HISTORY), "hypertension|afib|clotting|cholesterol|heart|cardiac") ~ "Cardiovascular",
      str_detect(str_to_lower(HISTORY), "asthma|copd|respiratory|lung") ~ "Respiratory",
      str_detect(str_to_lower(HISTORY), "no|none|none known|nka|none stated|nkda|n/a|na") ~ "None",
      str_detect(str_to_lower(HISTORY), "unk|unknown|unk") ~ "UNK",
      is.na(HISTORY) ~ NA_character_, 
      TRUE ~ "Other"
    ),
    
    # Allergy Triggers
    ALLERGIES = case_when(
      str_detect(str_to_lower(ALLERGIES), "amoxicillin|penicillin|sulfa|antibiotic") ~ "Antibiotics",
      str_detect(str_to_lower(ALLERGIES), "tylenol|codeine|aspirin|ibuprofen|nsaid|morphine") ~ "Pain Meds/NSAIDs",
      str_detect(str_to_lower(ALLERGIES), "latex|nickel|betadine|tape|pollen|environmental") ~ "Env/Material",
      str_detect(str_to_lower(ALLERGIES), "peanut|shellfish|egg|dairy") ~ "Food",
      str_detect(str_to_lower(OTHER_MEDS), "no|none|none known|nka|none stated|nkda|non|n/a|na") ~ "None",
      str_detect(str_to_lower(OTHER_MEDS), "unk|unknown|unk") ~ "UNK",
      is.na(ALLERGIES) ~ NA_character_,
      TRUE ~ "Other"
    )
  )

df5 = df_factored # Checkpoint

df_factored <- df_factored %>%
  mutate(ILLNESS_TYPE = case_when(
    # 1. Truly NA (The data was never entered)
    is.na(CUR_ILL) ~ NA,
    
    # 2. Explicit UNKNOWN 
    str_detect(CUR_ILL, regex("unknown", ignore_case = TRUE)) ~ "UNK",
    
    # 3. Explicit NONE (not sick)
    str_detect(CUR_ILL, regex("^(none|no|nil|0|nothing|healthy|no illnesses|no active problems|n/a|none reported|none known|none stated|not applicable)$", ignore_case = TRUE)) ~ "NONE",
    
    # 8. Autoimmune/MSK
    str_detect(CUR_ILL, regex("arthritis|lupus|psoriasis|fibromyalgia|osteoporosis|joint|back pain|gout", ignore_case = TRUE)) ~ "AUTOIMMUNE_MSK",
    
    # 4. Cardiovascular
    str_detect(CUR_ILL, regex("hypertension|blood pressure|heart|cardiac|atrial|a-fib|infarction|stroke|cad|cholesterol|lipids", ignore_case = TRUE)) ~ "CARDIOVASCULAR",
    
    # 5. Metabolic/Endocrine
    str_detect(CUR_ILL, regex("diabetes|diabetic|thyroid|obesity|metabolic|pcos", ignore_case = TRUE)) ~ "METABOLIC_ENDO",
    
    # 6. Respiratory
    str_detect(CUR_ILL, regex("asthma|copd|bronchitis|respiratory|emphysema|sleep apnea|seasonal allergies|allergic rhinitis|sinus", ignore_case = TRUE)) ~ "RESPIRATORY",
    
    # 7. Infectious
    str_detect(CUR_ILL, regex("uti|covid|herpes|influenza|flu|cold|infection|shingles|pneumonia|hiv|hep", ignore_case = TRUE)) ~ "INFECTIOUS",
    
    # 9. Neuro/Psych
    str_detect(CUR_ILL, regex("anxiety|depression|migraine|headache|seizure|epilepsy|dementia|alzheimer|adhd|bipolar|neuropathy", ignore_case = TRUE)) ~ "NEURO_PSYCH",
    
    # 10. Other Chronic
    str_detect(CUR_ILL, regex("cancer|tumor|lymphoma|leukemia|kidney|renal|gerd|acid reflux|crohn|colitis|liver|gastritis|eczema", ignore_case = TRUE)) ~ "OTHER_CHRONIC",
    
    # Default for anything else that contains text but didn't match the above
    TRUE ~ "OTHER"
  )) %>%
  mutate(ILLNESS_TYPE = as.factor(ILLNESS_TYPE))
#================================================

# Cleaning up column types
df_factored = df_factored |>
  mutate(across(c(SEX, V_ADMINBY, OTHER_MEDS, HISTORY, ALLERGIES, VAX_TYPE, VAX_MANU, REGION, CUR_ILL), as.factor)) |>
  mutate(across(c(RECVDATE, VAX_DATE, ONSET_DATE), lubridate::ymd)) |>
  mutate(across(c(AGE_YRS, NUMDAYS, YEAR, NUM_VAX, SEVERE), as.numeric))

# Re-arranging column order: ID, Response, Predictors, Response indicators, additional info
df_factored <- df_factored[, c("VAERS_ID", "SEVERE", "YEAR", "AGE_YRS", "SEX", "REGION", "VAX_DATE", "ONSET_DATE", "NUMDAYS", "V_ADMINBY", "NUM_VAX", "VAX_TYPE", "VAX_MANU", "HISTORY", "ALLERGIES", "OTHER_MEDS", "DIED", "L_THREAT", "ER_VISIT", "HOSPITAL", "DISABLE", "ER_ED_VISIT", "STATE", "VAX_NAME")]
df_factored

#===============================================================================
# Imputing values with missing <20%
round(colMeans(is.na(df_factored)), 2)

library(missRanger)

# 1. Select only the columns we WANT to use/impute
# We add NUMDAYS here instead of the raw dates (cannot impute dates)
cols_to_impute <- c("AGE_YRS", "REGION", "VAX_TYPE", "VAX_MANU", "NUM_VAX", "NUMDAYS")

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


