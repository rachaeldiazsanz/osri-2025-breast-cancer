# Survival Curve by HER2 Receptor Status (Kaplan-Meier)

breast_cancer_data_clean_her2 <- droplevels(breast_cancer_data_clean[breast_cancer_data_clean$`Derived HER2 Recode (2010+)` %in%
                                                                               c("Borderline/Unknown", "Negative", "Positive"), ])

# Subseting states
california = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean_her2, breast_cancer_data_clean_her2$`SEER registry (with CA and GA as whole states)`=='Utah')

# California by HER2 Receptor
fitCAHER2 <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$`Derived HER2 Recode (2010+)`
)

plot(fitCAHER2,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for California"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCAHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitCAHER2$strata)],
       lty = 1
)

# Connecticut by HER2 Receptor
fitCTHER2 <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$`Derived HER2 Recode (2010+)`
)

plot(fitCTHER2,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Connecticut"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCTHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitCTHER2$strata)],
       lty = 1
)

# Alaska by HER2 Receptor
fitAKHER2 <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$`Derived HER2 Recode (2010+)`
)

plot(fitAKHER2,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Alaska"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitAKHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitAKHER2$strata)],
       lty = 1
)

# Georgia by HER2 Receptor
fitGAHER2 <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$`Derived HER2 Recode (2010+)`
)

plot(fitGAHER2,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Georgia"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitGAHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitGAHER2$strata)],
       lty = 1
)

# Hawaii by HER2 Receptor
fitHIHER2 <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$`Derived HER2 Recode (2010+)`
)

plot(fitHIHER2,
     col = c("red", "blue", "green", "purple","orange","brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Hawaii"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitHIHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange","brown")[1:length(fitHIHER2$strata)],
       lty = 1
)


# Iowa by HER2 Receptor
fitIAHER2 <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$`Derived HER2 Recode (2010+)`
)

plot(fitIAHER2,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Iowa"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIAHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitIAHER2$strata)],
       lty = 1
)

# Kentucky by HER2 Receptor
fitKYHER2 <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$`Derived HER2 Recode (2010+)`
)

plot(fitKYHER2,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Kentucky"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitKYHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitKYHER2$strata)],
       lty = 1
)

# Louisiana by HER2 Receptor
fitLAHER2 <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$`Derived HER2 Recode (2010+)`
)
plot(fitLAHER2,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Louisiana"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitLAHER2$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitLAHER2$strata)],
       lty = 1
)

# New Jersey by HER2 Receptor 
fitNJHER2 <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$`Derived HER2 Recode (2010+)`
)
plot(fitNJHER2,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for New Jersey"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNJHER2$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitNJHER2$strata)],
       lty = 1
)

# New Mexico by HER2 Receptor
fitNMHER2 <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$`Derived HER2 Recode (2010+)`
)
plot(fitNMHER2,
     col = c("red", "blue", "green", "purple","orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for New Mexico"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNMHER2$strata)),
       col = c("red", "blue", "green", "purple","orange")[1:length(fitNMHER2$strata)],
       lty = 1
)

# New York by HER2 Receptor
fitNYHER2 <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$`Derived HER2 Recode (2010+)`
)
plot(fitNYHER2,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for New York"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNYHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitNYHER2$strata)],
       lty = 1
)


# Seattle by HER2 Receptor
fitSEAHER2 <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$`Derived HER2 Recode (2010+)`
)

plot(fitSEAHER2,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Seattle"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitSEAHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitSEAHER2$strata)],
       lty = 1
)

# Texas by HER2 Receptor
fitTXHER2 <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$`Derived HER2 Recode (2010+)`
)
plot(fitTXHER2,
     col = c("red", "blue", "green", "purple", "orange", "brown"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Texas"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTXHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange", "brown")[1:length(fitTXHER2$strata)],
       lty = 1
)

# Utah by HER2 Receptor
fitUTHER2 <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$`Derived HER2 Recode (2010+)`
)
plot(fitUTHER2,
     col = c("red", "blue", "green", "purple", "orange"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by HER2 Receptor for Utah"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitUTHER2$strata)),
       col = c("red", "blue", "green", "purple", "orange")[1:length(fitUTHER2$strata)],
       lty = 1
)

