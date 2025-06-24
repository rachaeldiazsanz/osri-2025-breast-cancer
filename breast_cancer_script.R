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

breast_cancer_data_clean$`Median household income inflation adj to 2023`= as.factor(breast_cancer_data_clean$`Median household income inflation adj to 2023`)

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

# Survival Curve for Stages (Kaplan-Meier)
fitIncome <- survfit(
  Surv(
    time = breast_cancer_data_clean$`Survival months`,
    event = breast_cancer_data_clean$`SEER cause-specific death classification` == "1"
  ) ~ breast_cancer_data_clean$`Median household income inflation adj to 2023`
)
plot(fitIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.6, 1),
     main = "Survival Curve by Income Group"
)

legend("bottomleft",
       legend = levels(breast_cancer_data_clean$`Median household income inflation adj to 2023`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
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

# Kaplan-Meier Income by States

# California by Income
fitCAIncome <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$`Median household income inflation adj to 2023`
)

plot(fitCAIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for California"
)

legend("bottomleft",
       legend = levels(california$`Median household income inflation adj to 2023`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Drop unused factor levels first

# Connecticut by Income
fitCTIncome <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$`Median household income inflation adj to 2023`
)

plot(fitCTIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Connecticut"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCTIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitCTIncome$strata)],
       lty = 1)

# Georgia by Income
fitGAIncome <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$`Median household income inflation adj to 2023`
)

plot(fitGAIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`Median household income inflation adj to 2023`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Drop unused factor levels first
hawaii <- droplevels(hawaii[hawaii$`Median household income inflation adj to 2023` %in%
                              c("$50,000-$74,999", "$75,000-$99,999"), ])


# Hawaii by Income
fitHIIncome <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$`Median household income inflation adj to 2023`
)
plot(fitHIIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Hawaii"
)

legend("bottomleft",
       legend = levels(hawaii$`Median household income inflation adj to 2023`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Drop unused factor levels first
idaho <- droplevels(idaho[idaho$`Median household income inflation adj to 2023` %in%
                              c( ">=$100,000"), ])


# Idaho by Income
fitIDIncome <- survfit(
  Surv(
    time = idaho$`Survival months`,
    event = idaho$`SEER cause-specific death classification` == "1"
  ) ~ idaho$`Median household income inflation adj to 2023`
)
plot(fitIDIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Idaho"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIDIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitIDIncome$strata)],
       lty = 1
)



# Drop unused factor levels first
#iowa <- droplevels(iowa[iowa$`Median household income inflation adj to 2023` %in%
#                            c( "<$50,000", ">=$100,000"), ])


# Iowa by Income
fitIAIncome <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$`Median household income inflation adj to 2023`
)
plot(fitIAIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Iowa"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIAIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitIAIncome$strata)],
       lty = 1
)

# Kentuky by Income
fitKYIncome <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$`Median household income inflation adj to 2023`
)

plot(fitKYIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Kentucky"
)

legend("bottomleft",
       legend = levels(kentucky$`Median household income inflation adj to 2023`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)



# Louisiana by Income
fitLAIncome <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$`Median household income inflation adj to 2023`
)
plot(fitLAIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Louisiana"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitLAIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitLAIncome$strata)],
       lty = 1
)

# New Jersey by Income 
fitNJIncome <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$`Median household income inflation adj to 2023`
)
plot(fitNJIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for New Jersey"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNJIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitNJIncome$strata)],
       lty = 1
)

# New Mexico by Income
fitNMIncome <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$`Median household income inflation adj to 2023`
)
plot(fitNMIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for New Mexico"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNMIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitNMIncome$strata)],
       lty = 1
)

# New York by Income
fitNYIncome <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$`Median household income inflation adj to 2023`
)
plot(fitNYIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for New York"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNYIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitNYIncome$strata)],
       lty = 1
)

# Drop unused factor levels first
#seattle <- droplevels(seattle[seattle$`Median household income inflation adj to 2023` %in%
#                              c("<$50,000",">= $100,000 "), ])


# Seattle by Income
fitSEAIncome <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$`Median household income inflation adj to 2023`
)

plot(fitSEAIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Seattle"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitSEAIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitSEAIncome$strata)],
       lty = 1
)

# Texas by Income
fitTXIncome <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$`Median household income inflation adj to 2023`
)
plot(fitTXIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Texas"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTXIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitTXIncome$strata)],
       lty = 1
)

#Utah by Income
fitUTIncome <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$`Median household income inflation adj to 2023`
)
plot(fitUTIncome,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Income Group for Utah"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitUTIncome$strata)),
       col = c("red", "blue", "green", "purple")[1:length(fitUTIncome$strata)],
       lty = 1
)



