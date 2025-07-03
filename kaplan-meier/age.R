# California by Age
fitCAAge <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$'Age recode with <1 year olds and 90+'
)

plot(fitCAAge,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for California"
)

legend("bottomleft",
       legend = levels(california$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Connecticut"
)

legend("bottomleft",
       legend = levels(connecticut$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Alaska"
)

legend("bottomleft",
       legend = levels(alaska$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Hawaii"
)

legend("bottomleft",
       legend = levels(hawaii$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Iowa"
)

legend("bottomleft",
       legend = levels(iowa$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Kentucky"
)

legend("bottomleft",
       legend = levels(kentucky$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Louisiana"
)

legend("bottomleft",
       legend = levels(louisiana$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New Jersey"
)

legend("bottomleft",
       legend = levels(new_jersey$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New Mexico"
)

legend("bottomleft",
       legend = levels(new_mexico$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for New York"
)

legend("bottomleft",
       legend = levels(new_york$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Seattle"
)

legend("bottomleft",
       legend = levels(seattle$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Texas"
)

legend("bottomleft",
       legend = levels(texas$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
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
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Age for Utah"
)

legend("bottomleft",
       legend = levels(utah$`Age recode with <1 year olds and 90+`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)
