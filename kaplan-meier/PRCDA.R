# California by PRCDA
fitCAPRCDA <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$'PRCDA 2020'
)

plot(fitCAPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for California"
)

legend("bottomleft",
       legend = levels(california$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Connecticut by PRCDA
fitCTPRCDA <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$'PRCDA 2020'
)

plot(fitCTPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Connecticut"
)

legend("bottomleft",
       legend = levels(connecticut$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Alaska by PRCDA
fitAKPRCDA <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$'PRCDA 2020'
)

plot(fitAKPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Alaska"
)

legend("bottomleft",
       legend = levels(alaska$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Georgia by PRCDA
fitGAPRCDA <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$'PRCDA 2020'
)

plot(fitGAPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Iowa by PRCDA
fitIAPRCDA <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$'PRCDA 2020'
)

plot(fitIAPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Iowa"
)

legend("bottomleft",
       legend = levels(iowa$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Kentucky by PRCDA
fitKTPRCDA <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$'PRCDA 2020'
)

plot(fitKTPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Kentucky"
)

legend("bottomleft",
       legend = levels(kentucky$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Louisiana by PRCDA
fitLAPRCDA <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$'PRCDA 2020'
)

plot(fitLAPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Louisiana"
)

legend("bottomleft",
       legend = levels(louisiana$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# New Jersey by PRCDA
fitNJPRCDA <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$'PRCDA 2020'
)

plot(fitNJPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New Jersey"
)

legend("bottomleft",
       legend = levels(new_jersey$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)


# New Mexico by PRCDA
fitNMPRCDA <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$'PRCDA 2020'
)

plot(fitNMPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New Mexico"
)

legend("bottomleft",
       legend = levels(new_mexico$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# New York by PRCDA
fitNYPRCDA <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$'PRCDA 2020'
)

plot(fitNYPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New York"
)

legend("bottomleft",
       legend = levels(new_york$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Seattle by PCRDA
fitSEAPRCDA <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$'PRCDA 2020'
)

plot(fitSEAPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Seattle"
)

legend("bottomleft",
       legend = levels(seattle$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Texas by PRCDA
fitTXPRCDA <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$'PRCDA 2020'
)

plot(fitTXPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Texas"
)

legend("bottomleft",
       legend = levels(texas$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Utah by PRCDA
fitUTPRCDA <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$'PRCDA 2020'
)

plot(fitUTPRCDA,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Utah"
)

legend("bottomleft",
       legend = levels(utah$`PRCDA 2020`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

