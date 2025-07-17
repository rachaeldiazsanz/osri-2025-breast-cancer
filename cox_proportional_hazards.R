# Cox Proportional Hazards Model

# Load libraries

library(broom)
library(ggplot2)
library(dplyr)


# ------------------------------
# 1️⃣ Run the Cox PH model
# ------------------------------


cox_model <- coxph(
  Surv(`Survival months`, `SEER cause-specific death classification` == "1") ~ 
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
  data = breast_cancer_data_clean
)

print(cox_model)

cox_table <- tidy(cox_model)

new_york_cox_model <- coxph(
  Surv(`Survival months`, `SEER cause-specific death classification` == "1") ~ 
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
  data = new_york
)
# Hazard Ratio Code
summary_model <- summary(new_york_cox_model)

HR <- exp(coef(new_york_cox_model))
CI <- exp(confint(new_york_cox_model))
pvals <- summary_model$coefficients[, "Pr(>|z|)"]

result_table <- cbind(HR, CI, pvals)
colnames(result_table) <- c("Hazard Ratio", "2.5 % CI", "97.5 % CI", "p-value")

print(round(result_table, 3))  


print(new_york_cox_model)

california_cox_model <- coxph(
  Surv(`Survival months`, `SEER cause-specific death classification` == "1") ~ 
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
  data = california
)

# Hazard Ratio Code
summary_model <- summary(california_cox_model)

HR <- exp(coef(california_cox_model))
CI <- exp(confint(california_cox_model))
pvals <- summary_model$coefficients[, "Pr(>|z|)"]

result_table <- cbind(HR, CI, pvals)
colnames(result_table) <- c("Hazard Ratio", "2.5 % CI", "97.5 % CI", "p-value")

print(round(result_table, 3))  


print(california_cox_model)


texas_cox_model <- coxph(
  Surv(`Survival months`, `SEER cause-specific death classification` == "1") ~ 
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
  data = texas
)
# Hazard Ratio Code
summary_model <- summary(texas_cox_model)

HR <- exp(coef(texas_cox_model))
CI <- exp(confint(texas_cox_model))
pvals <- summary_model$coefficients[, "Pr(>|z|)"]

result_table <- cbind(HR, CI, pvals)
colnames(result_table) <- c("Hazard Ratio", "2.5 % CI", "97.5 % CI", "p-value")

print(round(result_table, 3))  


print(texas_cox_model)

# ------------------------------
# 2️⃣ Tidy the model output
# ------------------------------

tidy_cox <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# ------------------------------
# 3️⃣ Clean it up for easy reading
# ------------------------------

tidy_pretty <- tidy_cox %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    p.value = signif(p.value, 3)
  )

# View it in the console
print(tidy_pretty, n = Inf)

# ------------------------------
# 4️⃣ Save it to CSV (optional)
# ------------------------------

california_cox_table <- tidy(california_cox_model)  # raw log HRs
write.csv(california_cox_table, "new_california_cox_model.csv", row.names = FALSE)

new_york_cox_table <- tidy(new_york_cox_model)
write.csv(new_york_cox_table, "new_new_york_cox_model.csv", row.names = FALSE)

texas_cox_table <- tidy(texas_cox_model)
write.csv(texas_cox_table, "new_texas_cox_model.csv", row.names = FALSE)

# ------------------------------
# 5️⃣ Forest plot (optional)
# ------------------------------

ggplot(tidy_pretty, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "#4E79A7", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  xlab("Hazard Ratio (HR)") +
  ylab("Variable") +
  theme_minimal(base_size = 12) +
  ggtitle("Cox Proportional Hazards: Hazard Ratios with 95% CI")
