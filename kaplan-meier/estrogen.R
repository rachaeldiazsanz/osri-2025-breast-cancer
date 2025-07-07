# Survival Curve by Estrogen Receptor Status (Kaplan-Meier)

breast_cancer_data_clean_estrogen <- droplevels(breast_cancer_data_clean[breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)` %in%
                                                                       c("Borderline/Unknown", "Negative", "Positive"), ])

# Subseting states
california = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean_estrogen, breast_cancer_data_clean_estrogen$`SEER registry (with CA and GA as whole states)`=='Utah')

# California by Estrogen Receptor
fitCAEstro <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$`ER Status Recode Breast Cancer (2010+)`
)

plot(fitCAEstro,
      col = new_palette[1:length(fitAge$strata)],            
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for California"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCAEstro$strata)),
        col = new_palette[1:length(fitAge$strata)],                  
       lwd = 2,
       lty = 1
)

# Connecticut by Estrogen Receptor
fitCTEstro <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$`ER Status Recode Breast Cancer (2010+)`
)

plot(fitCTEstro,
      col = new_palette[1:length(fitAge$strata)],            
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Connecticut"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCTEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# Alaska by Estrogen Receptor
fitAKEstro <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$`ER Status Recode Breast Cancer (2010+)`
)

plot(fitAKEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Alaska"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitAKEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# Georgia by Estrogen Receptor
fitGAEstro <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$`ER Status Recode Breast Cancer (2010+)`
)

plot(fitGAEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Georgia"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitGAEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# Hawaii by Estrogen Receptor
fitHIEstro <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$`ER Status Recode Breast Cancer (2010+)`
)

plot(fitHIEstro,
      col = new_palette[1:length(fitAge$strata)],            
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Hawaii"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitHIEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)


# Iowa by Estrogen Receptor
fitIAEstro <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$`ER Status Recode Breast Cancer (2010+)`
)

plot(fitIAEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Iowa"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIAEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# Kentucky by Estrogen Receptor
fitKYEstro <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$`ER Status Recode Breast Cancer (2010+)`
)

plot(fitKYEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Kentucky"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitKYEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# Louisiana by Estrogen Receptor
fitLAEstro <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$`ER Status Recode Breast Cancer (2010+)`
)
plot(fitLAEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Louisiana"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitLAEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# New Jersey by Estrogen Receptor 
fitNJEstro <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$`ER Status Recode Breast Cancer (2010+)`
)
plot(fitNJEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for New Jersey"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNJEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# New Mexico by Estrogen Receptor
fitNMEstro <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$`ER Status Recode Breast Cancer (2010+)`
)
plot(fitNMEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for New Mexico"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNMEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# New York by Estrogen Receptor
fitNYEstro <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$`ER Status Recode Breast Cancer (2010+)`
)
plot(fitNYEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for New York"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNYEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)


# Seattle by Estrogen Receptor
fitSEAEstro <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$`ER Status Recode Breast Cancer (2010+)`
)

plot(fitSEAEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Seattle"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitSEAEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# Texas by Estrogen Receptor
fitTXEstro <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$`ER Status Recode Breast Cancer (2010+)`
)
plot(fitTXEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,     
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Texas"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTXEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

# Utah by Estrogen Receptor
fitUTEstro <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$`ER Status Recode Breast Cancer (2010+)`
)
plot(fitUTEstro,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,     
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Estrogen Receptor for Utah"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitUTEstro$strata)),
       col = new_palette[1:length(fitAge$strata)],            
       lwd = 2,
       lty = 1
)

