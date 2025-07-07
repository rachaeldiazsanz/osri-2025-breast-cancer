# Survival Curve by Race (Kaplan-Meier)

breast_cancer_data_clean_race <- droplevels(breast_cancer_data_clean[breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` %in%
                                                                       c("Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian or Pacific Islander", "Non-Hispanic American Indian/Alaska Native", "Hispanic (All Races)"), ])

# Full Data by Race
fitRace <- survfit(
  Surv(
    time = breast_cancer_data_clean_race$`Survival months`,
    event = breast_cancer_data_clean_race$`SEER cause-specific death classification` == "1"
  ) ~ breast_cancer_data_clean_race$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitRace,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Subseting states
california = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean_race, breast_cancer_data_clean_race$`SEER registry (with CA and GA as whole states)`=='Utah')

# California by Race
fitCARace <- survfit(
  Surv(
    time = california$`Survival months`,
    event = california$`SEER cause-specific death classification` == "1"
  ) ~ california$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitCARace,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for California"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCARace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Connecticut by Race
fitCTRace <- survfit(
  Surv(
    time = connecticut$`Survival months`,
    event = connecticut$`SEER cause-specific death classification` == "1"
  ) ~ connecticut$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitCTRace,
     col = new_palette[1:length(fitAge$strata)],      
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Connecticut"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitCTRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Alaska by Race
fitAKRace <- survfit(
  Surv(
    time = alaska$`Survival months`,
    event = alaska$`SEER cause-specific death classification` == "1"
  ) ~ alaska$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitAKRace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Alaska"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitAKRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Georgia by Race
fitGARace <- survfit(
  Surv(
    time = georgia$`Survival months`,
    event = georgia$`SEER cause-specific death classification` == "1"
  ) ~ georgia$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitGARace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Georgia"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitGARace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Hawaii by Race
fitHIRace <- survfit(
  Surv(
    time = hawaii$`Survival months`,
    event = hawaii$`SEER cause-specific death classification` == "1"
  ) ~ hawaii$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitHIRace,
     col = new_palette[1:length(fitAge$strata)],       
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Hawaii"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitHIRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)


# Iowa by Race
fitIARace <- survfit(
  Surv(
    time = iowa$`Survival months`,
    event = iowa$`SEER cause-specific death classification` == "1"
  ) ~ iowa$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitIARace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Iowa"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitIARace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Kentucky by Race
fitKYRace <- survfit(
  Surv(
    time = kentucky$`Survival months`,
    event = kentucky$`SEER cause-specific death classification` == "1"
  ) ~ kentucky$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitKYRace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Kentucky"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitKYRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Louisiana by Race
fitLARace <- survfit(
  Surv(
    time = louisiana$`Survival months`,
    event = louisiana$`SEER cause-specific death classification` == "1"
  ) ~ louisiana$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)
plot(fitLARace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Louisiana"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitLARace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# New Jersey by Race 
fitNJRace <- survfit(
  Surv(
    time = new_jersey$`Survival months`,
    event = new_jersey$`SEER cause-specific death classification` == "1"
  ) ~ new_jersey$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)
plot(fitNJRace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for New Jersey"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNJRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# New Mexico by Race
fitNMRace <- survfit(
  Surv(
    time = new_mexico$`Survival months`,
    event = new_mexico$`SEER cause-specific death classification` == "1"
  ) ~ new_mexico$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)
plot(fitNMRace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for New Mexico"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNMRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# New York by Race
fitNYRace <- survfit(
  Surv(
    time = new_york$`Survival months`,
    event = new_york$`SEER cause-specific death classification` == "1"
  ) ~ new_york$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)
plot(fitNYRace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for New York"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitNYRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)


# Seattle by Race
fitSEARace <- survfit(
  Surv(
    time = seattle$`Survival months`,
    event = seattle$`SEER cause-specific death classification` == "1"
  ) ~ seattle$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)

plot(fitSEARace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Seattle"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitSEARace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Texas by Race
fitTXRace <- survfit(
  Surv(
    time = texas$`Survival months`,
    event = texas$`SEER cause-specific death classification` == "1"
  ) ~ texas$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)
plot(fitTXRace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Texas"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitTXRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

# Utah by Race
fitUTRace <- survfit(
  Surv(
    time = utah$`Survival months`,
    event = utah$`SEER cause-specific death classification` == "1"
  ) ~ utah$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
)
plot(fitUTRace,
     col = new_palette[1:length(fitAge$strata)], 
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.2, 1),
     main = "Survival Curve by Race for Utah"
)

legend("bottomleft",
       legend = gsub(".*=|`", "", names(fitUTRace$strata)),
       col = new_palette[1:length(fitAge$strata)], 
       lwd = 2,
       lty = 1
)

