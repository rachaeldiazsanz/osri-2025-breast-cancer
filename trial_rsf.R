# ===============================
# 📦 Install and load packages
# ===============================
# install.packages("randomForestSRC")
# install.packages("ggplot2")
# install.packages("survival")

library(randomForestSRC)
library(ggplot2)
library(survival)

# ===============================
# 🧹 Convert variables
# ===============================
california$`Derived HER2 Recode (2010+)` <- factor(california$`Derived HER2 Recode (2010+)`)
california$`Breast Subtype (2010+)` <- factor(california$`Breast Subtype (2010+)`)
california$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` <- factor(california$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`)
california$`Age recode with <1 year olds and 90+` <- factor(california$`Age recode with <1 year olds and 90+`)
california$`Grade Recode (thru 2017)` <- factor(california$`Grade Recode (thru 2017)`)
california$`Combined Summary Stage with Expanded Regional Codes (2004+)` <- factor(california$`Combined Summary Stage with Expanded Regional Codes (2004+)`)
california$`Radiation recode (2003+)` <- factor(california$`Radiation recode (2003+)`)
california$`Chemotherapy recode (yes, no/unk) (2004+)` <- factor(california$`Chemotherapy recode (yes, no/unk) (2004+)`)
california$`ER Status Recoded Breast Cancer (2010+)` <- factor(california$`ER Status Recoded Breast Cancer (2010+)`)
california$`PR Status Recoded Breast Cancer (2010+)` <- factor(california$`PR Status Recoded Breast Cancer (2010+)`)
california$`Median household income inflation adj to 2023` <- factor(california$`Median household income inflation adj to 2023`)

# ===============================
# 🔢 Make sure status is 0/1
# ===============================
california$`SEER cause-specific death classification` <- as.numeric(california$`SEER cause-specific death classification`)

# ===============================
# 🧽 Clean empty strings
# ===============================
california[california == ""] <- NA

# ===============================
# ✂️ Split into train/test
# ===============================
set.seed(42)
train_indices <- sample(1:nrow(california), 0.8 * nrow(california))
train_data <- california[train_indices, ]
test_data <- california[-train_indices, ]

# Optional: Check for remaining NAs
print("NAs per column (train):")
print(sapply(train_data, function(x) sum(is.na(x))))

# ===============================
# 🌲 Fit Random Survival Forest
# ===============================
rsf_model <- rfsrc(
  Surv(`Survival months`, `SEER cause-specific death classification`) ~
    `Age recode with <1 year olds and 90+` +
    `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` +
    `Grade Recode (thru 2017)` +
    `Combined Summary Stage with Expanded Regional Codes (2004+)` +
    `Radiation recode (2003+)` +
    `Chemotherapy recode (yes, no/unk) (2004+)` +
    `Breast Subtype (2010+)` +
    `Derived HER2 Recode (2010+)` +
    `ER Status Recoded Breast Cancer (2010+)` +ce
    `PR Status Recoded Breast Cancer (2010+)` +
    `Median household income inflation adj to 2023` +
    `Total number of in situ/malignant tumors for patient`,
  data = train_data,
  ntree = 500,
  nodesize = 15,
  importance = TRUE,
  na.action = "na.impute"
)

print(rsf_model)

# ===============================
# 🔮 Predict on Test Data
# ===============================
rsf_pred <- predict(rsf_model, newdata = test_data)

# ===============================
# 📈 Get C-index on Training Data
# ===============================
train_cindex <- rsf_model$concordance
print(paste("Training C-index:", round(train_cindex, 3)))

# ===============================
# 📈 Get C-index on Test Data
# ===============================
final_time <- ncol(rsf_pred$survival)
predicted_surv <- rsf_pred$survival[, final_time]
predicted_risk <- 1 - predicted_surv

true_Surv <- Surv(
  time = test_data$`Survival months`,
  event = test_data$`SEER cause-specific death classification`
)

cindex_result <- concordance(true_Surv ~ predicted_risk)
print(paste("Test C-index:", round(cindex_result$concordance, 3)))


