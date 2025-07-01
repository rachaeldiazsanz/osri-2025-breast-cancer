# Kaplan-Meier Chemotherapy by States

# California by Chemotherapy
fitCAChemo <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitCAChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for California"
)

legend("bottomleft",
       legend = levels(california$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Connecticut by Chemotherapy
fitCTChemo <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitCTChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Connecticut"
)

legend("bottomleft",
       legend = levels(connecticut$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Alaska by Chemotherapy
fitAKChemo <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitAKChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Alaska"
)

legend("bottomleft",
       legend = levels(california$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Georgia by Chemotherapy
fitGAChemo <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitGAChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Hawaii by Chemotherapy
fitHIChemo <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitHIChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Hawaii"
)

legend("bottomleft",
       legend = levels(hawaii$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Iowa by Chemotherapy
fitIAChemo <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitIAChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Iowa"
)

legend("bottomleft",
       legend = levels(iowa$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

#. Kentucky for Chemotherapy
fitKYChemo <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitKYChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Kentucky"
)

legend("bottomleft",
       legend = levels(kentucky$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

#Louisiana by Chemotherapy
fitLAChemo <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitLAChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Louisiana"
)

legend("bottomleft",
       legend = levels(louisiana$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# New Jersey by Chemotherapy 
fitNJChemo <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitNJChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for New Jersey"
)

legend("bottomleft",
       legend = levels(new_jersey$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# New Mexico by Chemotherapy
fitNMChemo <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitNMChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for New Mexico"
)

legend("bottomleft",
       legend = levels(new_mexico$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# New York by Chemotherapy
fitNYChemo <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitNYChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for New York"
)

legend("bottomleft",
       legend = levels(new_york$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Seattle by Chemotherapy
fitSEAChemo <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitSEAChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Seattle"
)

legend("bottomleft",
       legend = levels(seattle$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Texas by Chemotherapy
fitTXChemo <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitTXChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Texas"
)

legend("bottomleft",
       legend = levels(texas$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)

# Utah by Chemotherapy
fitUTChemo <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$'Chemotherapy recode (yes, no/unk) (2004+)'
)

plot(fitUTChemo,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Chemotherapy for Utah"
)

legend("bottomleft",
       legend = levels(utah$`Chemotherapy recode (yes, no/unk) (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)


