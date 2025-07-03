# California by Primary site
fitCAPrimary <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$'Primary Site - labeled'
)

plot(fitCAPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for California"
)

legend("bottomleft",
       legend = levels(california$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Connecticut by Primary site
fitCTPrimary <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$'Primary Site - labeled'
)

plot(fitCTPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for Connecticut"
)

legend("bottomleft",
       legend = levels(connecticut$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Georgia by Primary site
fitGAPrimary <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$'Primary Site - labeled'
)

plot(fitGAPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Iowa by Primary site
fitIAPrimary <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$'Primary Site - labeled'
)

plot(fitIAPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for Iowa"
)

legend("bottomleft",
       legend = levels(iowa$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Kentucky by Primary site
fitKTPrimary <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$'Primary Site - labeled'
)

plot(fitKTPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for Kentucky"
)

legend("bottomleft",
       legend = levels(kentucky$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Louisiana by Primary site
fitLAPrimary <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$'Primary Site - labeled'
)

plot(fitLAPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for Louisiana"
)

legend("bottomleft",
       legend = levels(louisiana$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# New Jersey by Primary site
fitNJPrimary <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$'Primary Site - labeled'
)

plot(fitNJPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for New Jersey"
)

legend("bottomleft",
       legend = levels(new_jersey$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# New Mexico by Primary site 
fitNMPrimary <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$'Primary Site - labeled'
)

plot(fitNMPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for New Mexico"
)

legend("bottomleft",
       legend = levels(new_mexico$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# New York by Primary site
fitNYPrimary <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$'Primary Site - labeled'
)

plot(fitNYPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for New York"
)

legend("bottomleft",
       legend = levels(new_york$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Seattle by Primary site
fitSEAPrimary <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$'Primary Site - labeled'
)

plot(fitSEAPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for Seattle"
)

legend("bottomleft",
       legend = levels(seattle$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Texas by Primary site
fitTXPrimary <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$'Primary Site - labeled'
)

plot(fitTXPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for Texas"
)

legend("bottomleft",
       legend = levels(texas$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Utah by Primary site
fitUTPrimary <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$'Primary Site - labeled'
)

plot(fitUTPrimary,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Primary Site for Utah"
)

legend("bottomleft",
       legend = levels(utah$`Primary Site - labeled`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)


