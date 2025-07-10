# Survival Curve by Number of Tumors (Kaplan-Meier)

# ALL by Number of Tumors
fitTumor <- survfit(
  Surv(
    time = breast_cancer_data_clean$`Survival months`,
    event = breast_cancer_data_clean$`SEER cause-specific death classification` == "1"
  ) ~ breast_cancer_data_clean$`Total number of in situ/malignant tumors for patient`
)

plot(fitTumor,
     col = new_palette[1:length(fitAge$strata)],     
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# California by Number of Tumors
fitCATumor <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$`Total number of in situ/malignant tumors for patient`
)

plot(fitCATumor,
     col = new_palette[1:length(fitAge$strata)],     
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for California"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCATumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Connecticut by Number of Tumors
fitCTTumor <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$`Total number of in situ/malignant tumors for patient`
)

plot(fitCTTumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Connecticut"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCTTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Alaska by Number of Tumors
fitAKTumor <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$`Total number of in situ/malignant tumors for patient`
)

plot(fitAKTumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Alaska"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitAKTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],       
       lwd = 2,
       lty = 1
)

# Georgia by Number of Tumors
fitGATumor <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$`Total number of in situ/malignant tumors for patient`
)

plot(fitGATumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Georgia"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitGATumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Hawaii by Number of Tumors
fitHITumor <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$`Total number of in situ/malignant tumors for patient`
)

plot(fitHITumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Hawaii"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitHITumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)


# Iowa by Number of Tumors
fitIATumor <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$`Total number of in situ/malignant tumors for patient`
)

plot(fitIATumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Iowa"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIATumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Kentucky by Number of Tumors
fitKYTumor <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$`Total number of in situ/malignant tumors for patient`
)

plot(fitKYTumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Kentucky"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitKYTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Louisiana by Number of Tumors
fitLATumor <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$`Total number of in situ/malignant tumors for patient`
)
plot(fitLATumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Louisiana"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitLATumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# New Jersey by Number of Tumors 
fitNJTumor <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$`Total number of in situ/malignant tumors for patient`
)
plot(fitNJTumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for New Jersey"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNJTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# New Mexico by Number of Tumors
fitNMTumor <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$`Total number of in situ/malignant tumors for patient`
)
plot(fitNMTumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for New Mexico"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNMTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# New York by Number of Tumors
fitNYTumor <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$`Total number of in situ/malignant tumors for patient`
)
plot(fitNYTumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for New York"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNYTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)


# Seattle by Number of Tumors
fitSEATumor <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$`Total number of in situ/malignant tumors for patient`
)

plot(fitSEATumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Seattle"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitSEATumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Texas by Number of Tumors
fitTXTumor <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$`Total number of in situ/malignant tumors for patient`
)
plot(fitTXTumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Texas"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTXTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Utah by Number of Tumors
fitUTTumor <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$`Total number of in situ/malignant tumors for patient`
)
plot(fitUTTumor,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Number of Tumors for Utah"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitUTTumor$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

