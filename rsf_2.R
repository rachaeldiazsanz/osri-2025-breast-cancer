# Install if needed
install.packages("randomForestSRC")
install.packages("ggplot2")

# Load
library(randomForestSRC)
library(ggplot2)

# Make sure variables are factors (and numeric stays numeric!)
breast_cancer_data_clean$`Derived HER2 Recode (2010+)` <- factor(breast_cancer_data_clean$`Derived HER2 Recode (2010+)`)
breast_cancer_data_clean$`Breast Subtype (2010+)` <- factor(breast_cancer_data_clean$`Breast Subtype (2010+)`)
breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` <- factor(breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`)
breast_cancer_data_clean$`Age recode with <1 year olds and 90+` <- factor(breast_cancer_data_clean$`Age recode with <1 year olds and 90+`)
breast_cancer_data_clean$`Grade Recode (thru 2017)` <- factor(breast_cancer_data_clean$`Grade Recode (thru 2017)`)
breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)` <- factor(breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`)
breast_cancer_data_clean$`Radiation recode (2003+)` <- factor(breast_cancer_data_clean$`Radiation recode (2003+)`)
breast_cancer_data_clean$`Chemotherapy recode (yes, no/unk) (2004+)` <- factor(breast_cancer_data_clean$`Chemotherapy recode (yes, no/unk) (2004+)`)
breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)` <- factor(breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)`)
breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)` <- factor(breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)`)
breast_cancer_data_clean$`Median household income inflation adj to 2023` <- factor(breast_cancer_data_clean$`Median household income inflation adj to 2023`)

breast_cancer_data_clean$`SEER cause-specific death classification` <- 
  as.numeric(breast_cancer_data_clean$`SEER cause-specific death classification`)


# Run Random Survival Forest
california_rsf_model <- rfsrc(
  Surv(`Survival months`, `SEER cause-specific death classification`) ~ 
    `Age recode with <1 year olds and 90+` +
    `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` +
    `Grade Recode (thru 2017)` +
    `Combined Summary Stage with Expanded Regional Codes (2004+)` +
    `Radiation recode (2003+)` +
    `Chemotherapy recode (yes, no/unk) (2004+)` +
    `Breast Subtype (2010+)` +
    `Derived HER2 Recode (2010+)` +
    `ER Status Recode Breast Cancer (2010+)` +
    `PR Status Recode Breast Cancer (2010+)` +
    `Median household income inflation adj to 2023` +
    `Total number of in situ/malignant tumors for patient`,
  data = california,
  ntree = 50,
  nodesize = 15,
  importance = TRUE
)

# Check output
print(rsf_model)



