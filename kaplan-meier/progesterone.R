# Survival Curve by Progesterone Receptor Status (Kaplan-Meier)

breast_cancer_data_clean_progesterone <- droplevels(breast_cancer_data_clean[breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)` %in%
                                                                           c("Borderline/Unknown", "Negative", "Positive"), ])

# Subseting states
california = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean_progesterone, breast_cancer_data_clean_progesterone$`SEER registry (with CA and GA as whole states)`=='Utah')

# California by Progesterone Receptor
fitCAProg <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$`PR Status Recode Breast Cancer (2010+)`
)

plot(fitCAProg,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for California"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCAProg$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitCAProg$strata)],
       lty = 1
)

# Connecticut by Progesterone Receptor
fitCTProg <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$`PR Status Recode Breast Cancer (2010+)`
)

plot(fitCTProg,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Connecticut"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCTProg$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitCTProg$strata)],
       lty = 1
)

# Alaska by Progesterone Receptor
fitAKProg <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$`PR Status Recode Breast Cancer (2010+)`
)

plot(fitAKProg,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Alaska"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitAKProg$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitAKProg$strata)],
       lty = 1
)

# Georgia by Progesterone Receptor
fitGAProg <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$`PR Status Recode Breast Cancer (2010+)`
)

plot(fitGAProg,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Georgia"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitGAProg$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitGAProg$strata)],
       lty = 1
)

# Hawaii by Progesterone Receptor
fitHIProg <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$`PR Status Recode Breast Cancer (2010+)`
)

plot(fitHIProg,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Hawaii"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitHIProg$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitHIProg$strata)],
       lty = 1
)


# Iowa by Progesterone Receptor
fitIAProg <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$`PR Status Recode Breast Cancer (2010+)`
)

plot(fitIAProg,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Iowa"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIAProg$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitIAProg$strata)],
       lty = 1
)

# Kentucky by Progesterone Receptor
fitKYProg <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$`PR Status Recode Breast Cancer (2010+)`
)

plot(fitKYProg,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Kentucky"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitKYProg$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitKYProg$strata)],
       lty = 1
)

# Louisiana by Progesterone Receptor
fitLAProg <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$`PR Status Recode Breast Cancer (2010+)`
)
plot(fitLAProg,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Louisiana"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitLAProg$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitLAProg$strata)],
       lty = 1
)

# New Jersey by Progesterone Receptor 
fitNJProg <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$`PR Status Recode Breast Cancer (2010+)`
)
plot(fitNJProg,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for New Jersey"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNJProg$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitNJProg$strata)],
       lty = 1
)

# New Mexico by Progesterone Receptor
fitNMProg <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$`PR Status Recode Breast Cancer (2010+)`
)
plot(fitNMProg,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for New Mexico"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNMProg$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitNMProg$strata)],
       lty = 1
)

# New York by Progesterone Receptor
fitNYProg <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$`PR Status Recode Breast Cancer (2010+)`
)
plot(fitNYProg,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for New York"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNYProg$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitNYProg$strata)],
       lty = 1
)


# Seattle by Progesterone Receptor
fitSEAProg <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$`PR Status Recode Breast Cancer (2010+)`
)

plot(fitSEAProg,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Seattle"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitSEAProg$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitSEAProg$strata)],
       lty = 1
)

# Texas by Progesterone Receptor
fitTXProg <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$`PR Status Recode Breast Cancer (2010+)`
)
plot(fitTXProg,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Texas"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTXProg$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitTXProg$strata)],
       lty = 1
)

# Utah by Progesterone Receptor
fitUTProg <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$`PR Status Recode Breast Cancer (2010+)`
)
plot(fitUTProg,
     col = c("red", "blue", "green", "purple", "orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Progesterone Receptor for Utah"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitUTProg$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitUTProg$strata)],
       lty = 1
)

