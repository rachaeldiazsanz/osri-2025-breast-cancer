# California by Stage summary
fitCAStage <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitCAStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for California"
)

legend("bottomleft",
       legend = levels(california$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)

# Connecticut by Stage Summary
fitCTStage <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitCTStage,
     col = new_palette[1:length(fitCAStage$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for Connecticut"
)

legend("bottomleft",
       legend = levels(connecticut$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],      
       lwd = 2,
       lty = 1:1
)

# Georgia by Stage Summary
fitGAStage <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitGAStage,
     col = new_palette[1:length(fitCAStage$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for Georgia"
)

legend("bottomleft",
       legend = levels(georgia$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)

# Iowa by Stage Summary 
fitIAStage <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitIAStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for Iowa"
)

legend("bottomleft",
       legend = levels(iowa$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],      
       lwd = 2,
       lty = 1:1
)

# Kentucky by Stage Summary
fitKTStage <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitKTStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for Kentucky"
)

legend("bottomleft",
       legend = levels(kentucky$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)

# Louisiana by Stage Summary
fitLAStage <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitLAStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for Louisiana"
)

legend("bottomleft",
       legend = levels(louisiana$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)

# New Jersey by Stage Summary
fitNJStage <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitNJStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for New Jersey"
)

legend("bottomleft",
       legend = levels(new_jersey$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)

# New Mexico by Stage Summary
fitNMStage <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitNMStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for New Mexico"
)

legend("bottomleft",
       legend = levels(new_mexico$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)

# New York by Stage Summary
fitNYStage <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitNYStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for New York"
)

legend("bottomleft",
       legend = levels(new_york$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],      
       lwd = 2,
       lty = 1:1
)

# Seattle by Stage Summary
fitSEAStage <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitSEAStage,
     col = new_palette[1:length(fitCAStage$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for Seattle"
)

legend("bottomleft",
       legend = levels(seattle$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)

# Texas by Stage Summary
fitTXStage <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitTXStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for Texas"
)

legend("bottomleft",
       legend = levels(texas$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)

# Utah by Stage Summary
fitUTStage <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$'Combined Summary Stage with Expanded Regional Codes (2004+)'
)

plot(fitUTStage,
     col = new_palette[1:length(fitCAStage$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Combined Stage Summary for Utah"
)

legend("bottomleft",
       legend = levels(utah$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = new_palette[1:length(fitCAStage$strata)],       
       lwd = 2,
       lty = 1:1
)























































































