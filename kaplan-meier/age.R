# California by Age

breast_cancer_data_clean_age <- droplevels(breast_cancer_data_clean[breast_cancer_data_clean$`Age recode with <1 year olds and 90+` %in%
                                                                          c("15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years"), ])

# All by Age
fitAge <- survfit(
  Surv(
    time = breast_cancer_data_clean_age$`Survival months`,
    event = breast_cancer_data_clean_age$`SEER cause-specific death classification` == "1"
  ) ~ breast_cancer_data_clean_age$`Age recode with <1 year olds and 90+`
)

plot(fitAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitAge$strata)),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1
)

# Subseting states
california = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean_age, breast_cancer_data_clean_age$`SEER registry (with CA and GA as whole states)`=='Utah')


fitCAAge <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$'Age recode with <1 year olds and 90+'
)

plot(fitCAAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for California"
)

legend("bottomleft",
       legend = levels(california$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Connecticut by Age 
fitCTAge <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$'Age recode with <1 year olds and 90+'
)

plot(fitCTAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Connecticut"
)

legend("bottomleft",
       legend = levels(connecticut$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Alaska by Age 
fitAKAge <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$'Age recode with <1 year olds and 90+'
)

plot(fitAKAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Alaska"
)

legend("bottomleft",
       legend = levels(alaska$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Georgia by Age
fitGAAge <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$'Age recode with <1 year olds and 90+'
)

plot(fitGAAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Hawaii by Age 
fitHIAge <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$'Age recode with <1 year olds and 90+'
)

plot(fitHIAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Hawaii"
)

legend("bottomleft",
       legend = levels(hawaii$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Iowa by Age
fitIAAge <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$'Age recode with <1 year olds and 90+'
)

plot(fitIAAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Iowa"
)

legend("bottomleft",
       legend = levels(iowa$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Kentucky by Age
fitKTAge <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$'Age recode with <1 year olds and 90+'
)

plot(fitKTAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Kentucky"
)

legend("bottomleft",
       legend = levels(kentucky$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Louisiana by Age 
fitLAAge <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$'Age recode with <1 year olds and 90+'
)

plot(fitLAAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Louisiana"
)

legend("bottomleft",
       legend = levels(louisiana$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# New Jersey by Age 
fitNJAge <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$'Age recode with <1 year olds and 90+'
)

plot(fitNJAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New Jersey"
)

legend("bottomleft",
       legend = levels(new_jersey$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# New Mexico by Age
fitNMAge <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$'Age recode with <1 year olds and 90+'
)

plot(fitNMAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New Mexico"
)

legend("bottomleft",
       legend = levels(new_mexico$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# New York by Age 
fitNYAge <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$'Age recode with <1 year olds and 90+'
)

plot(fitNYAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New York"
)

legend("bottomleft",
       legend = levels(new_york$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Seattle by Age
fitSEAAge <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$'Age recode with <1 year olds and 90+'
)

plot(fitSEAAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Seattle"
)

legend("bottomleft",
       legend = levels(seattle$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Texas by Age 
fitTXAge <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$'Age recode with <1 year olds and 90+'
)

plot(fitTXAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Texas"
)

legend("bottomleft",
       legend = levels(texas$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)

# Utah by Age 
fitUTAge <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$'Age recode with <1 year olds and 90+'
)

plot(fitUTAge,
     col = my_palette[1:length(fitIncome$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Utah"
)

legend("bottomleft",
       legend = levels(utah$`Age recode with <1 year olds and 90+`),
       col = my_palette[1:length(fitIncome$strata)], 
       lwd = 2,
       lty = 1:1
)
