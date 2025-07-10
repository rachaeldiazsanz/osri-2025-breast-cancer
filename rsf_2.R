# Install if needed
install.packages("randomForestSRC")
install.packages("ggplot2")

# Load
library(randomForestSRC)
library(ggplot2)

# Make sure variables are factors (and numeric stays numeric!)
california$`Derived HER2 Recode (2010+)` <- factor(california$`Derived HER2 Recode (2010+)`)
california$`Breast Subtype (2010+)` <- factor(california$`Breast Subtype (2010+)`)
california$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` <- factor(california$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`)
california$`Age recode with <1 year olds and 90+` <- factor(california$`Age recode with <1 year olds and 90+`)
california$`Grade Recode (thru 2017)` <- factor(california$`Grade Recode (thru 2017)`)
california$`Combined Summary Stage with Expanded Regional Codes (2004+)` <- factor(california$`Combined Summary Stage with Expanded Regional Codes (2004+)`)
california$`Radiation recode (2003+)` <- factor(california$`Radiation recode (2003+)`)
california$`Chemotherapy recode (yes, no/unk) (2004+)` <- factor(california$`Chemotherapy recode (yes, no/unk) (2004+)`)
california$`ER Status Recode Breast Cancer (2010+)` <- factor(california$`ER Status Recode Breast Cancer (2010+)`)
california$`PR Status Recode Breast Cancer (2010+)` <- factor(california$`PR Status Recode Breast Cancer (2010+)`)
california$`Median household income inflation adj to 2023` <- factor(california$`Median household income inflation adj to 2023`)

california$`SEER cause-specific death classification` <- 
  as.numeric(california$`SEER cause-specific death classification`)


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



