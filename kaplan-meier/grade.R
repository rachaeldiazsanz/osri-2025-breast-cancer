# California by Grade
fitCAGrade <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$'Grade Recode (thru 2017)'
)

plot(fitCAGrade,
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for California"
)

legend("bottomleft",
       legend = levels(california$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
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
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Connecticut"
)

legend("bottomleft",
       legend = levels(connecticut$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
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
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Alaska"
)

legend("bottomleft",
       legend = levels(alaska$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
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
    col = new_palette[1:length(fitAge$strata)],      
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],      
      lwd = 2,
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
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Hawaii"
)

legend("bottomleft",
       legend = levels(hawaii$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
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
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Iowa"
)

legend("bottomleft",
       legend = levels(iowa$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
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
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Kenrucky"
)

legend("bottomleft",
       legend = levels(kentucky$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
       lty = 1:1
)

# Louisiana by Grade
fitLAGrade <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$'Grade Recode (thru 2017)'
)

plot(fitLAGrade,
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Louisiana"
)

legend("bottomleft",
       legend = levels(louisiana$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
       lty = 1:1
)

# New jersey by Grade
fitNJGrade <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$'Grade Recode (thru 2017)'
)

plot(fitNJGrade,
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for New Jersey"
)

legend("bottomleft",
       legend = levels(new_jersey$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
       lty = 1:1
)

# New Mexico by Grade
fitNMGrade <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$'Grade Recode (thru 2017)'
)

plot(fitNMGrade,
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for New Mexico"
)

legend("bottomleft",
       legend = levels(new_mexico$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],      
      lwd = 2,
       lty = 1:1
)

# New York by Grade
fitNYGrade <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$'Grade Recode (thru 2017)'
)

plot(fitNYGrade,
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for New York"
)

legend("bottomleft",
       legend = levels(new_york$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
       lty = 1:1
)

# Seattle by Grade
fitSEAGrade <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$'Grade Recode (thru 2017)'
)

plot(fitSEAGrade,
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Seattle"
)

legend("bottomleft",
       legend = levels(seattle$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],      
      lwd = 2,
       lty = 1:1
)

# Texas by Grade
fitTXGrade <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$'Grade Recode (thru 2017)'
)

plot(fitTXGrade,
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Texas"
)

legend("bottomleft",
       legend = levels(texas$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
       lty = 1:1
)

# Utah by Grade
fitUTGrade <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$'Grade Recode (thru 2017)'
)

plot(fitUTGrade,
    col = new_palette[1:length(fitAge$strata)],       
    lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Grade for Utah"
)

legend("bottomleft",
       legend = levels(utah$`Grade Recode (thru 2017)`),
      col = new_palette[1:length(fitAge$strata)],       
      lwd = 2,
       lty = 1:1
)

