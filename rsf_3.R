# =======================================================
# 📦 1️⃣ Install and load packages (only run install once)
# =======================================================
install.packages("randomForestSRC")
install.packages("ggplot2")
install.packages("survival")

library(randomForestSRC)
library(ggplot2)
library(survival)

# =======================================================
# ✅ 2️⃣ Convert variables to factors/numeric as needed
# =======================================================

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

# Make sure event is numeric 0/1
california$`SEER cause-specific death classification` <- as.numeric(california$`SEER cause-specific death classification` == "1")

# =======================================================
# ✂️ 3️⃣ Split data into 80% training and 20% test
# =======================================================

set.seed(42)
train_indices <- sample(1:nrow(california), 0.8 * nrow(california))
train_data <- california[train_indices, ]
test_data  <- california[-train_indices, ]

# =======================================================
# 🌳 4️⃣ Fit Random Survival Forest on training data
# =======================================================

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
    `ER Status Recode Breast Cancer (2010+)` +
    `PR Status Recode Breast Cancer (2010+)` +
   # `Median household income inflation adj to 2023` +
    `Total number of in situ/malignant tumors for patient`,
  data = train_data,
  ntree = 500,
  nodesize = 15,
  importance = TRUE
)

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
    `ER Status Recode Breast Cancer (2010+)` +
    `PR Status Recode Breast Cancer (2010+)` +
    `Median household income inflation adj to 2023` +
    `Total number of in situ/malignant tumors for patient`,
  data = train_data,
  ntrees = 500,
  nodesize = 15,
  importance = TRUE,
  na.action = "na.impute"
)


print(rsf_model)

# =======================================================
# 🧩 5️⃣ Predict survival for test data
# =======================================================

rsf_pred <- predict(rsf_model, newdata = test_data)

# =======================================================
# 🏅 6️⃣ Get C-index for training data
# =======================================================
train_cindex <- rsf_model$concordance
print(paste("Training C-index:", round(train_cindex, 3)))

# =======================================================
# 🏅 7️⃣ Get C-index for test data
# =======================================================

# Get final time point
final_time <- ncol(rsf_pred$survival)
predicted_surv <- rsf_pred$survival[, final_time]
predicted_risk <- 1 - predicted_surv

# True Surv object
true_Surv <- Surv(
  time = test_data$`Survival months`,
  event = test_data$`SEER cause-specific death classification`
)

# C-index
cindex_result <- concordance(true_Surv ~ predicted_risk)
print(paste("Test C-index:", round(cindex_result$concordance, 3)))
