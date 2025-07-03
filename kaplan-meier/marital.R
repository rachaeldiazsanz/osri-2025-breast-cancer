# Survival Curve by Marital Status (Kaplan-Meier)

breast_cancer_data_clean_marital <- droplevels(breast_cancer_data_clean[breast_cancer_data_clean$`Marital status at diagnosis` %in%
                                                                       c("Divorced", "Married (including common law)", "Single (never married)"), ])
# California by Marital Status
fitMarital <- survfit(
  Surv(
    time = breast_cancer_data_clean_marital$`Survival months`,
    event = breast_cancer_data_clean_marital$`SEER cause-specific death classification` == "1"
  ) ~ breast_cancer_data_clean_marital$`Marital status at diagnosis`
)

plot(fitMarital,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitMarital$strata)],
       lty = 1
)

# Subseting states
california = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean_marital, breast_cancer_data_clean_marital$`SEER registry (with CA and GA as whole states)`=='Utah')

# California by Marital Status
fitCAMarital <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$`Marital status at diagnosis`
)

plot(fitCAMarital,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for California"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCAMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitCAMarital$strata)],
       lty = 1
)

# Connecticut by Marital Status
fitCTMarital <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$`Marital status at diagnosis`
)

plot(fitCTMarital,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Connecticut"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCTMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitCTMarital$strata)],
       lty = 1
)

# Alaska by Marital Status
fitAKMarital <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$`Marital status at diagnosis`
)

plot(fitAKMarital,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Alaska"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitAKMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitAKMarital$strata)],
       lty = 1
)

# Georgia by Marital Status
fitGAMarital <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$`Marital status at diagnosis`
)

plot(fitGAMarital,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Georgia"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitGAMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitGAMarital$strata)],
       lty = 1
)

# Hawaii by Marital Status
fitHIMarital <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$`Marital status at diagnosis`
)

plot(fitHIMarital,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Hawaii"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitHIMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitHIMarital$strata)],
       lty = 1
)


# Iowa by Marital Status
fitIAMarital <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$`Marital status at diagnosis`
)

plot(fitIAMarital,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Iowa"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIAMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitIAMarital$strata)],
       lty = 1
)

# Kentucky by Marital Status
fitKYMarital <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$`Marital status at diagnosis`
)

plot(fitKYMarital,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Kentucky"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitKYMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitKYMarital$strata)],
       lty = 1
)

# Louisiana by Marital Status
fitLAMarital <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$`Marital status at diagnosis`
)
plot(fitLAMarital,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Louisiana"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitLAMarital$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitLAMarital$strata)],
       lty = 1
)

# New Jersey by Marital Status 
fitNJMarital <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$`Marital status at diagnosis`
)
plot(fitNJMarital,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for New Jersey"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNJMarital$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitNJMarital$strata)],
       lty = 1
)

# New Mexico by Marital Status
fitNMMarital <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$`Marital status at diagnosis`
)
plot(fitNMMarital,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for New Mexico"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNMMarital$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitNMMarital$strata)],
       lty = 1
)

# New York by Marital Status
fitNYMarital <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$`Marital status at diagnosis`
)
plot(fitNYMarital,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for New York"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNYMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitNYMarital$strata)],
       lty = 1
)


# Seattle by Marital Status
fitSEAMarital <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$`Marital status at diagnosis`
)

plot(fitSEAMarital,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Seattle"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitSEAMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitSEAMarital$strata)],
       lty = 1
)

# Texas by Marital Status
fitTXMarital <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$`Marital status at diagnosis`
)
plot(fitTXMarital,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Texas"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTXMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitTXMarital$strata)],
       lty = 1
)

# Utah by Marital Status
fitUTMarital <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$`Marital status at diagnosis`
)
plot(fitUTMarital,
     col = c("red", "blue", "green", "purple", "orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Marital Status for Utah"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitUTMarital$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitUTMarital$strata)],
       lty = 1
)

