# Survival Curve by Subtype (Kaplan-Meier)

breast_cancer_data_clean_type <- droplevels(breast_cancer_data_clean[breast_cancer_data_clean$`Breast Subtype (2010+)` %in%
                                      c("HR-/HER2-", "HR-/HER2+", "HR+/HER2-", "HR+/HER2+", "Unknown"), ])

# Subseting states
california = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean_type, breast_cancer_data_clean_type$`SEER registry (with CA and GA as whole states)`=='Utah')

# California by Subtype
fitCAType <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$`Breast Subtype (2010+)`
)

plot(fitCAType,
     col = new_palette[1:length(fitAge$strata)],     
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for California"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCAType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Connecticut by Subtype
fitCTType <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$`Breast Subtype (2010+)`
)

plot(fitCTType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Connecticut"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCTType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Alaska by Subtype
fitAKType <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$`Breast Subtype (2010+)`
)

plot(fitAKType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Alaska"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitAKType$strata)),
       col = new_palette[1:length(fitAge$strata)],       
       lwd = 2,
       lty = 1
)

# Georgia by Subtype
fitGAType <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$`Breast Subtype (2010+)`
)

plot(fitGAType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Georgia"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitGAType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Hawaii by Subtype
fitHIType <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$`Breast Subtype (2010+)`
)

plot(fitHIType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Hawaii"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitHIType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)


# Iowa by Subtype
fitIASubtype <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$`Breast Subtype (2010+)`
)

plot(fitIASubtype,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Iowa"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIASubtype$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Kentucky by Subtype
fitKYType <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$`Breast Subtype (2010+)`
)

plot(fitKYType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Kentucky"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitKYType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Louisiana by Subtype
fitLAType <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$`Breast Subtype (2010+)`
)
plot(fitLAType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Louisiana"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitLAType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# New Jersey by Subtype 
fitNJType <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$`Breast Subtype (2010+)`
)
plot(fitNJType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for New Jersey"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNJType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# New Mexico by Subtype
fitNMType <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$`Breast Subtype (2010+)`
)
plot(fitNMType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for New Mexico"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNMType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# New York by Subtype
fitNYType <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$`Breast Subtype (2010+)`
)
plot(fitNYType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for New York"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNYType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)


# Seattle by Subtype
fitSEAType <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$`Breast Subtype (2010+)`
)

plot(fitSEAType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Seattle"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitSEAType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Texas by Subtype
fitTXType <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$`Breast Subtype (2010+)`
)
plot(fitTXType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Texas"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTXType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

# Utah by Subtype
fitUTType <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$`Breast Subtype (2010+)`
)
plot(fitUTType,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Subtype for Utah"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitUTType$strata)),
       col = new_palette[1:length(fitAge$strata)],      
       lwd = 2,
       lty = 1
)

