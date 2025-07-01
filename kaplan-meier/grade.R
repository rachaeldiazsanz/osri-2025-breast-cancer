# California by Grade
fitCAGrade <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$'Grade Recode (thru 2017)'
)

plot(fitCAGrade,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for California"
)

legend("bottomleft",
       legend = levels(california$`Grade Recode (thru 2017)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Connecticut by Grade
fitCTGrade <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$'Grade Recode (thru 2017)'
)

plot(fitCTGrade,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Connecticut"
)

legend("bottomleft",
       legend = levels(connecticut$`Grade Recode (thru 2017)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)
# Alaska by Grade
fitAKGrade <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$'Grade Recode (thru 2017)'
)

plot(fitAKGrade,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Alaska"
)

legend("bottomleft",
       legend = levels(alaska$`Grade Recode (thru 2017)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Georgia by Grade
fitGAGrade <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$'Grade Recode (thru 2017)'
)

plot(fitGAGrade,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`Grade Recode (thru 2017)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Hawaii by Grade
fitHIGrade <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$'Grade Recode (thru 2017)'
)

plot(fitHIGrade,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Hawaii"
)

legend("bottomleft",
       legend = levels(hawaii$`Grade Recode (thru 2017)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Iowa by Grade
fitIAGrade <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$'Grade Recode (thru 2017)'
)

plot(fitIAGrade,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Iowa"
)

legend("bottomleft",
       legend = levels(iowa$`Grade Recode (thru 2017)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Kentucky by Grade
fitKYGrade <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$'Grade Recode (thru 2017)'
)

plot(fitKYGrade,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Kenrucky"
)

legend("bottomleft",
       legend = levels(kentucky$`Grade Recode (thru 2017)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)
