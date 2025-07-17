library(readr)
library(dplyr)
library(survival)
library(ggplot2)

# reading the file
breast_cancer_data_new <- read_csv("Desktop/2025-osri-breast-cancer/breast_cancer_data_new.csv")

# remove male instances
breast_cancer_data_clean = subset(breast_cancer_data_new, breast_cancer_data_new$Sex != "Male")

# only include women under 40 years of age
breast_cancer_data_clean <- subset(
  breast_cancer_data_clean,
  `Age recode with <1 year olds and 90+` %in% c(
    "01-04 years",
    "05-09 years",
    "10-14 years",
    "15-19 years",
    "20-24 years",
    "25-29 years",
    "30-34 years",
    "35-39 years"
  )
)

# Removing unnecessary variables
breast_cancer_data_clean = breast_cancer_data_clean[,-c(2, 30, 32, 36)]

# Removing unknown income entries
breast_cancer_data_clean = subset(breast_cancer_data_clean, breast_cancer_data_clean$`Median household income inflation adj to 2023` != "Unknown/missing/no match/Not 1990-2023")

# Recoding income based on guidelines found in paper
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(
    `Median household income inflation adj to 2023` = case_when(
      `Median household income inflation adj to 2023` %in% c("< $40,000", "$40,000 - $44,999", "$45,000 - $49,999") ~ "<$50,000",
      `Median household income inflation adj to 2023` %in% c("$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999") ~ "$50,000-$74,999",
      `Median household income inflation adj to 2023` %in% c("$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999") ~ "$75,000-$99,999",
      `Median household income inflation adj to 2023` %in% c("$90,000 - $94,999", "$95,000 - $99,999") ~ "$75,000-$99,999",
      `Median household income inflation adj to 2023` %in% c("$100,000 - $109,999", "$110,000 - $119,999", "$120,000+") ~ ">= $100,000",
      TRUE ~ NA_character_
    )
  )

breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(
    `Age recode with <1 year olds and 90+` = case_when(
      `Age recode with <1 year olds and 90+` %in% c("01-04 years", "05-09 years", "15-19 years", "20-24 years", "25-29 years") ~ ">30 years",
      `Age recode with <1 year olds and 90+` %in% c("30-34 years", "35-39 years") ~ "30-39 years",
      TRUE ~ NA_character_
    )
  )

breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(
    `Total number of in situ/malignant tumors for patient` = case_when(
      `Total number of in situ/malignant tumors for patient` %in% c("1") ~ "1",
      `Total number of in situ/malignant tumors for patient` %in% c("2") ~ "2",
      `Total number of in situ/malignant tumors for patient` %in% c("3", "4", "5") ~ "3+",
      TRUE ~ NA_character_
    )
  )

# Change Age to factor
breast_cancer_data_clean$'Age recode with <1 year olds and 90+'= as.factor(breast_cancer_data_clean$`Age recode with <1 year olds and 90+`)

# Change Race variable to a factor 
breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`= as.factor(breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`)

# Change Grade Recode variable to a factor
breast_cancer_data_clean$'Grade Recode (thru 2017)'= as.factor(breast_cancer_data_clean$`Grade Recode (thru 2017)`)

# Changing stage summary variable to a factor
breast_cancer_data_clean$'Combined Summary Stage with Expanded Regional Codes (2004+)'= as.factor(breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`)

# Change Radiation variable to a factor 
breast_cancer_data_clean$`Radiation recode (2003+)`= as.factor(breast_cancer_data_clean$`Radiation recode (2003+)`)

# Change Chemotherapy variable to a factor 
breast_cancer_data_clean$`Chemotherapy recode (yes, no/unk) (2004+)`= as.factor(breast_cancer_data_clean$`Chemotherapy recode (yes, no/unk) (2004+)`)

# Change Subtype variable to a factor 
breast_cancer_data_clean$`Breast Subtype (2010+)`= as.factor(breast_cancer_data_clean$`Breast Subtype (2010+)`)

# Change HER2 status variable to a factor 
breast_cancer_data_clean$`Derived HER2 Recode (2010+)`= as.factor(breast_cancer_data_clean$`Derived HER2 Recode (2010+)`)

# Change Estrogen status variable to a factor 
breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)`= as.factor(breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)`)

# Change Progesterone status variable to a factor 
breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)`= as.factor(breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)`)

# Change PRCDA to factor
breast_cancer_data_clean$'PRCDA 2020'= as.factor(breast_cancer_data_clean$`PRCDA 2020`)

# Change Primary site to a factor 
breast_cancer_data_clean$'Primary Site - labeled'= as.factor(breast_cancer_data_clean$`Primary Site - labeled`)

# Change the Income variable to factor
breast_cancer_data_clean$`Median household income inflation adj to 2023`= as.factor(breast_cancer_data_clean$`Median household income inflation adj to 2023`)

# Changing number of tumors to a factor
breast_cancer_data_clean$`Total number of in situ/malignant tumors for patient` = as.factor(breast_cancer_data_clean$`Total number of in situ/malignant tumors for patient`)

# Recoding cause of death to censor for survival analysis
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(
    `SEER cause-specific death classification` = case_when(
      `SEER cause-specific death classification` %in% c("Alive or dead of other cause", "Dead (missing/unknown COD)") ~ "0",
      `SEER cause-specific death classification` %in% c("Dead (attributable to this cancer dx)") ~ "1",
      TRUE ~ NA_character_
    )
  )

breast_cancer_data_clean$`Survival months` = as.numeric(breast_cancer_data_clean$`Survival months`)

breast_cancer_data_clean$event = as.numeric(breast_cancer_data_clean$`SEER cause-specific death classification`)

# Defining a colorblind-friendly color palette
new_palette <- c(
  "#70C1A6", # mint
  "#F79068", # peach
  "#4E79A7", # strong blue
  "#E28DC1", # soft pink
  "#ABD761", # lime
  "#F7DC70", # yellow
  "#D37295", # deep rose
  "#59A14F", # deeper teal green
  "#AF7AA1"  # purple plum
)

breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` <- factor(
  breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`,
  levels = c("Non-Hispanic White", "Non-Hispanic Black", 
             "Non-Hispanic Asian or Pacific Islander", 
             "Non-Hispanic American Indian/Alaska Native",
             "Non-Hispanic Unknown Race", "Hispanic (All Races)")
)

breast_cancer_data_clean$`Grade Recode (thru 2017)` <- factor(
  breast_cancer_data_clean$`Grade Recode (thru 2017)`,
  levels = c("Well differentiated; Grade I", "Moderately differentiated; Grade II", 
             "Poorly differentiated; Grade III", 
             "Undifferentiated; anaplastic; Grade IV",
             "Unknown")
)

breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)` <- factor(
  breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`,
  levels = c("Localized only", "Regional lymph nodes involved only", 
             "Regional by direct extension only", 
             "Regional by both direct extension and lymph node involvement",
             "Distant site(s)/node(s) involved", "Unknown/unstaged/unspecified/DCO")
)

breast_cancer_data_clean$`Radiation recode (2003+)` <- factor(
  breast_cancer_data_clean$`Radiation recode (2003+)`,
  levels = c("None/Unknown", "Beam radiation", 
             "Combination of beam with implants or isotopes", 
             "Radioactive implants (includes brachytherapy) (1988+)",
             "Not available (IL or TX)", "Radiation, NOS  method or source not specified", 
             "Radioisotopes (1988+)", "Recommended, unknown if administered", "Refused (1988+)")
)

breast_cancer_data_clean$`Breast Subtype (2010+)` <- factor(
  breast_cancer_data_clean$`Breast Subtype (2010+)`,
  levels = c("HR+/HER2-", "HR+/HER2+", 
             "Recode not available", 
             "HR-/HER2+", "HR-/HER2-",
             "Unknown")
)

breast_cancer_data_clean$`Derived HER2 Recode (2010+)` <- factor(
  breast_cancer_data_clean$`Derived HER2 Recode (2010+)`,
  levels = c("Negative", "Positive", 
             "Recode not available", 
             "Borderline/Unknown")
)

breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)` <- factor(
  breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)`,
  levels = c("Negative", "Positive", 
             "Recode not available", 
             "Borderline/Unknown")
)

breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)` <- factor(
  breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)`,
  levels = c("Negative", "Positive", 
             "Recode not available", 
             "Borderline/Unknown")
)

breast_cancer_data_clean$`Median household income inflation adj to 2023` <- factor(
  breast_cancer_data_clean$`Median household income inflation adj to 2023`,
  levels = c(">= $100,000", "$75,000-$99,999", 
             "$50,000-$74,999", 
             "<$50,000")
)


# Subseting states
california = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Utah')


# Changing the variables to numeric 

# Recode mapping
recoding_stage <- c(
  "Localized only" = "1",
  "Regional lymph nodes involved only" = "2",
  "Regional by direct extension only" = "3",
  "Regional by both direct extension and lymph node involvement" = "4",
  "Distant site(s)/node(s) involved" = "0",
  "Unknown/unstaged/unspecified/DCO" = "5"
)

# Convert using the mapping
breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)` <- 
  recoding_stage[as.character(breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`)]

# Convert from character to numeric
breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)` <- 
  as.numeric(breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`)



# recoding map for ER
recoding_er <- c(
  "Negative" = "0",
  "Positive" = "1",
  "Borderline/Unknown" = "2",
  "Recode not available" = "3"
)

# Apply the mapping
breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)` <- 
  recoding_er[as.character(breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)`)]

# Convert to numeric
breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)` <- 
  as.numeric(breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)`)



# Create recoding map for PR
recoding_pr <- c(
  "Negative" = "0",
  "Positive" = "1",
  "Borderline/Unknown" = "2",
  "Recode not available" = "2"
)

# Apply the mapping
breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)` <- 
  recoding_pr[as.character(breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)`)]

# Convert to numeric
breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)` <- 
  as.numeric(breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)`)


# Create recoding map for HER2
recoding_her2 <- c(
  "Negative" = "1",
  "Positive" = "2",
  "Borderline/Unknown" = "0",
  "Recode not available" = "0"
)

# Apply the mapping
breast_cancer_data_clean$`Derived HER2 Recode (2010+)` <- 
  recoding_her2[as.character(breast_cancer_data_clean$`Derived HER2 Recode (2010+)`)]

# Convert to numeric
breast_cancer_data_clean$`Derived HER2 Recode (2010+)` <- 
  as.numeric(breast_cancer_data_clean$`Derived HER2 Recode (2010+)`)

# Recode for grade
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(`Grade Recode (thru 2017)` = case_when(
    `Grade Recode (thru 2017)` == "Well differentiated; Grade I" ~ 0,
    `Grade Recode (thru 2017)` == "Moderately differentiated; Grade II" ~ 1,
    `Grade Recode (thru 2017)` == "Poorly differentiated; Grade III" ~ 2,
    `Grade Recode (thru 2017)` == "Undifferentiated; anaplastic; Grade IV" ~ 3,
    `Grade Recode (thru 2017)` == "Unknown" ~ 4,
    TRUE ~ NA_real_
  ))

# Recoding for Race
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(race_numeric = case_when(
    grepl("Non-Hispanic White", `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`) ~ 0,
    grepl("Non-Hispanic Black", `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`) ~ 1,
    grepl("Non-Hispanic Asian or Pacific Islander", `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`) ~ 2,
    grepl("Non-Hispanic American Indian/Alaska Native", `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`) ~ 3,
    grepl("Hispanic", `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`) ~ 4,
    TRUE ~ 5  # Optional: fallback for unknowns or unmatched
  ))

# Recoding for Radiation
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(radiation_numeric = case_when(
    `Radiation recode (2003+)` == "None/Unknown" ~ 0,
    `Radiation recode (2003+)` == "Beam radiation" ~ 1,
    `Radiation recode (2003+)` == "Combination of beam with implants or isotopes" ~ 2,
    `Radiation recode (2003+)` == "Radioactive implants (includes brachytherapy) (1988+)" ~ 3,
    `Radiation recode (2003+)` == "Not available (IL or TX)" ~ 4,
    `Radiation recode (2003+)` == "Radiation, NOS  method or source not specified" ~ 5,
    `Radiation recode (2003+)` == "Radioisotopes (1988+)" ~ 6,
    `Radiation recode (2003+)` == "Recommended, unknown if administered" ~ 7,
    `Radiation recode (2003+)` == "Refused (1988+)" ~ 8,
    TRUE ~ NA_real_
  ))

# Recoding for Chemotherapy
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(chemo_numeric = case_when(
    `Chemotherapy recode (yes, no/unk) (2004+)` == "Yes" ~ 1,
    `Chemotherapy recode (yes, no/unk) (2004+)` == "No/Unknown" ~ 0,
    TRUE ~ NA_real_
  ))

# Recoding for Subtypes
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(subtype_numeric = case_when(
    `Breast Subtype (2010+)` == "HR+/HER2-" ~ 1,
    `Breast Subtype (2010+)` == "HR+/HER2+" ~ 2,
    `Breast Subtype (2010+)` == "HR-/HER2+" ~ 3,
    `Breast Subtype (2010+)` == "HR-/HER2-" ~ 4,
    `Breast Subtype (2010+)` == "HR+/HER2+ Recode not available" ~ 5,
    `Breast Subtype (2010+)` == "Unknown" ~ 6,
    TRUE ~ 0  # fallback in case there's a typo or unmatched value
  ))

# Recoding for Age
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(age_group_numeric = case_when(
    `Age recode with <1 year olds and 90+` == "30-39 years" ~ 1,
    `Age recode with <1 year olds and 90+` == ">30 years" ~ 0,
    TRUE ~ NA_real_  # fallback for any unmatched value
  ))



