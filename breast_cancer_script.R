library(readr)

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

# Recoding income variable
breast_cancer_data_clean$`Median household income inflation adj to 2023` = factor(breast_cancer_data_clean$`Median household income inflation adj to 2023`)
recoding_income <- c(
  "< $40,000" = "<$50,000",
  "$40,000 - $44,999" = "<$50,000",
  "$45,000 - $49,999" = "<$50,000",
  "$50,000 - $54,999" = "$50,000-$74,999",
  "$55,000 - $59,999" = "$50,000-$74,999",
  "$60,000 - $64,999" = "$50,000-$74,999",
  "$65,000 - $69,999" = "$50,000-$74,999",
  "$70,000 - $74,999" = "$50,000-$74,999",
  "$75,000 - $79,999" = "$75,000-$99,999",
  "$80,000 - $84,999" = "$75,000-$99,999",
  "$85,000 - $89,999" = "$75,000-$99,999",
  "$90,000 - $94,999" = "$75,000-$99,999",
  "$95,000 - $99,999" = "$75,000-$99,999",
  "$100,000 - $109,999" = ">=$100,000",
  "$110,000 - $119,999" = ">=$100,000",
  "$120,000+" = ">=$100,000"
)

breast_cancer_data_clean$`Median household income inflation adj to 2023` = as.factor(recoding_income[breast_cancer_data_clean$`Median household income inflation adj to 2023`])
