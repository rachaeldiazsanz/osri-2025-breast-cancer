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
