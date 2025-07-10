library(readr)
library(dplyr)
library(survival)
library(ggplot2)

# reading the file
breast_cancer_data_new <- read_csv("Desktop/2025-osri-breast-cancer/breast_cancer_data_new.csv")

# remove male instances
breast_cancer_data_clean = subset(breast_cancer_data_new, breast_cancer_data_new$Sex != "Male")

# only include women under 40 years of age
breast_cancer_data_clean <- subset(
  breast_cancer_data_clean,
  `Age recode with <1 year olds and 90+` %in% c(
    "01-04 years",
    "05-09 years",
    "10-14 years",
    "15-19 years",
    "20-24 years",
    "25-29 years",
    "30-34 years",
    "35-39 years"
  )
)

# Removing unnecessary variables
breast_cancer_data_clean = breast_cancer_data_clean[,-c(2, 30, 32, 36)]

# Removing unknown income entries
breast_cancer_data_clean = subset(breast_cancer_data_clean, breast_cancer_data_clean$`Median household income inflation adj to 2023` != "Unknown/missing/no match/Not 1990-2023")

# Recoding income based on guidelines found in paper
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(
    `Median household income inflation adj to 2023` = case_when(
      `Median household income inflation adj to 2023` %in% c("< $40,000", "$40,000 - $44,999", "$45,000 - $49,999") ~ "<$50,000",
      `Median household income inflation adj to 2023` %in% c("$50,000 - $54,999", "$55,000 - $59,999", "$60,000 - $64,999", "$65,000 - $69,999") ~ "$50,000-$74,999",
      `Median household income inflation adj to 2023` %in% c("$70,000 - $74,999", "$75,000 - $79,999", "$80,000 - $84,999", "$85,000 - $89,999") ~ "$75,000-$99,999",
      `Median household income inflation adj to 2023` %in% c("$90,000 - $94,999", "$95,000 - $99,999") ~ "$75,000-$99,999",
      `Median household income inflation adj to 2023` %in% c("$100,000 - $109,999", "$110,000 - $119,999", "$120,000+") ~ ">= $100,000",
      TRUE ~ NA_character_
    )
  )

breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(
    `Age recode with <1 year olds and 90+` = case_when(
      `Age recode with <1 year olds and 90+` %in% c("01-04 years", "05-09 years", "15-19 years", "20-24 years", "25-29 years") ~ ">30 years",
      `Age recode with <1 year olds and 90+` %in% c("30-34 years", "35-39 years") ~ "30-39 years",
      TRUE ~ NA_character_
    )
  )

breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(
    `Total number of in situ/malignant tumors for patient` = case_when(
      `Total number of in situ/malignant tumors for patient` %in% c("1") ~ "1",
      `Total number of in situ/malignant tumors for patient` %in% c("2") ~ "2",
      `Total number of in situ/malignant tumors for patient` %in% c("3", "4", "5") ~ "3+",
      TRUE ~ NA_character_
    )
  )

# Change the Income variable to factor
breast_cancer_data_clean$`Median household income inflation adj to 2023`= as.factor(breast_cancer_data_clean$`Median household income inflation adj to 2023`)

# Change Chemotherapy variable to a factor 
breast_cancer_data_clean$`Chemotherapy recode (yes, no/unk) (2004+)`= as.factor(breast_cancer_data_clean$`Chemotherapy recode (yes, no/unk) (2004+)`)

# Change Grade Recode variable to a factor
breast_cancer_data_clean$'Grade Recode (thru 2017)'= as.factor(breast_cancer_data_clean$`Grade Recode (thru 2017)`)

# Change Age to factor
breast_cancer_data_clean$'Age recode with <1 year olds and 90+'= as.factor(breast_cancer_data_clean$`Age recode with <1 year olds and 90+`)

# Change PRCDA to factor
breast_cancer_data_clean$'PRCDA 2020'= as.factor(breast_cancer_data_clean$`PRCDA 2020`)

# Change Primary site to a factor 
breast_cancer_data_clean$'Primary Site - labeled'= as.factor(breast_cancer_data_clean$`Primary Site - labeled`)

# Changing stage summary variable to a factor
breast_cancer_data_clean$'Combined Summary Stage with Expanded Regional Codes (2004+)'= as.factor(breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`)


# Recoding cause of death to censor for survival analysis
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(
    `SEER cause-specific death classification` = case_when(
      `SEER cause-specific death classification` %in% c("Alive or dead of other cause", "Dead (missing/unknown COD)") ~ "0",
      `SEER cause-specific death classification` %in% c("Dead (attributable to this cancer dx)") ~ "1",
      TRUE ~ NA_character_
    )
  )

breast_cancer_data_clean$`Survival months` = as.numeric(breast_cancer_data_clean$`Survival months`)

breast_cancer_data_clean$event = as.numeric(breast_cancer_data_clean$`SEER cause-specific death classification`)

# Defining a colorblind-friendly color palette
new_palette <- c(
  "#70C1A6", # mint
  "#F79068", # peach
  "#4E79A7", # strong blue
  "#E28DC1", # soft pink
  "#ABD761", # lime
  "#F7DC70", # yellow
  "#D37295", # deep rose
  "#59A14F", # deeper teal green
  "#AF7AA1"  # purple plum
)

# Survival Curve for Income (Kaplan- Meier)
fitIncome <- survfit(
  Surv(
    time = breast_cancer_data_clean$`Survival months`,
    event = breast_cancer_data_clean$`SEER cause-specific death classification` == "1"
  ) ~ breast_cancer_data_clean$`Median household income inflation adj to 2023`
)

plot(fitIncome,
     col = my_palette[1:length(fitIncome$strata)],
     lwd = 2,
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.6, 1),
     main = "Survival Curve by Income"
)

legend("bottomleft",
       legend = levels(breast_cancer_data_clean$`Median household income inflation adj to 2023`),
       col = my_palette[1:length(fitIncome$strata)],
       lwd = 2,
       lty = 1
)

# Survival Curve for Income (Kaplan- Meier)
fitStage <- survfit(
  Surv(
    time = breast_cancer_data_clean$`Survival months`,
    event = breast_cancer_data_clean$`SEER cause-specific death classification` == "1"
  ) ~ breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`
)
plot(fitStage,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.6, 1),
     main = "Survival Curve by Stage Summary"
)

legend("bottomleft",
       legend = levels(breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)
# Survival Curve for Grade (Kaplan-Meier)
fitGrade <- survfit(
  Surv(
    time = breast_cancer_data_clean$`Survival months`,
    event = breast_cancer_data_clean$`SEER cause-specific death classification` == "1"
  ) ~ breast_cancer_data_clean$`Grade Recode (thru 2017)`
)
plot(fitGrade,
     col = c("red", "blue", "green", "purple"),
     xlab = "Time (in months)",
     ylab = "Survival Probability",
     xlim = c(0, 140),
     ylim = c(0.6, 1),
     main = "Survival Curve by Grade"
)

legend("bottomleft",
       legend = levels(breast_cancer_data_clean$`Grade Recode (thru 2017)`),
       col = c("red", "blue", "green", "purple"),
       lty = 1:1
)
breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` <- factor(
  breast_cancer_data_clean$`Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`,
  levels = c("Non-Hispanic White", "Non-Hispanic Black", 
             "Non-Hispanic Asian or Pacific Islander", 
             "Non-Hispanic American Indian/Alaska Native",
             "Non-Hispanic Unknown Race", "Hispanic (All Races)")
)

breast_cancer_data_clean$`Grade Recode (thru 2017)` <- factor(
  breast_cancer_data_clean$`Grade Recode (thru 2017)`,
  levels = c("Well differentiated; Grade I", "Moderately differentiated; Grade II", 
             "Poorly differentiated; Grade III", 
             "Undifferentiated; anaplastic; Grade IV",
             "Unknown")
)

breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)` <- factor(
  breast_cancer_data_clean$`Combined Summary Stage with Expanded Regional Codes (2004+)`,
  levels = c("Localized only", "Regional lymph nodes involved only", 
             "Regional by direct extension only", 
             "Regional by both direct extension and lymph node involvement",
             "Distant site(s)/node(s) involved", "Unknown/unstaged/unspecified/DCO")
)

breast_cancer_data_clean$`Radiation recode (2003+)` <- factor(
  breast_cancer_data_clean$`Radiation recode (2003+)`,
  levels = c("None/Unknown", "Beam radiation", 
             "Combination of beam with implants or isotopes", 
             "Radioactive implants (includes brachytherapy) (1988+)",
             "Not available (IL or TX)", "Radiation, NOS  method or source not specified", 
             "Radioisotopes (1988+)", "Recommended, unknown if administered", "Refused (1988+)")
)

breast_cancer_data_clean$`Breast Subtype (2010+)` <- factor(
  breast_cancer_data_clean$`Breast Subtype (2010+)`,
  levels = c("HR+/HER2-", "HR+/HER2+", 
             "Recode not available", 
             "HR-/HER2+", "HR-/HER2-",
             "Unknown")
)

breast_cancer_data_clean$`Derived HER2 Recode (2010+)` <- factor(
  breast_cancer_data_clean$`Derived HER2 Recode (2010+)`,
  levels = c("Negative", "Positive", 
             "Recode not available", 
             "Borderline/Unknown")
)

breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)` <- factor(
  breast_cancer_data_clean$`ER Status Recode Breast Cancer (2010+)`,
  levels = c("Negative", "Positive", 
             "Recode not available", 
             "Borderline/Unknown")
)

breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)` <- factor(
  breast_cancer_data_clean$`PR Status Recode Breast Cancer (2010+)`,
  levels = c("Negative", "Positive", 
             "Recode not available", 
             "Borderline/Unknown")
)

breast_cancer_data_clean$`Median household income inflation adj to 2023` <- factor(
  breast_cancer_data_clean$`Median household income inflation adj to 2023`,
  levels = c(">= $100,000", "$75,000-$99,999", 
             "$50,000-$74,999", 
             "<$50,000")
)


# Subseting states
california = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='California')
connecticut = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Connecticut')
alaska = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Alaska Natives')
georgia = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Georgia')
hawaii = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Hawaii')
idaho = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Idaho')
iowa = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Iowa')
kentucky = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Kentucky')
louisiana = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Louisiana')
new_jersey = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='New Jersey')
new_mexico = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='New Mexico')
new_york = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='New York')
seattle = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Seattle (Puget Sound)')
texas = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Texas')
utah = subset(breast_cancer_data_clean, breast_cancer_data_clean$`SEER registry (with CA and GA as whole states)`=='Utah')


# Cox Proportional Hazards Model

# Load libraries

library(broom)
library(ggplot2)
library(dplyr)


# ------------------------------
# 1️⃣ Run the Cox PH model
# ------------------------------


cox_model <- coxph(
  Surv(`Survival months`, `SEER cause-specific death classification` == "1") ~ 
    `Age recode with <1 year olds and 90+` +
    `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` +
    `Grade Recode (thru 2017)` +
    `Combined Summary Stage with Expanded Regional Codes (2004+)` +
    `Radiation recode (2003+)` +
    `Chemotherapy recode (yes, no/unk) (2004+)` +
    `Breast Subtype (2010+)` +
    `Derived HER2 Recode (2010+)` +
    `ER Status Recode Breast Cancer (2010+)` +
    `PR Status Recode Breast Cancer (2010+)` +
    `Median household income inflation adj to 2023` +
  `Total number of in situ/malignant tumors for patient`,
  data = breast_cancer_data_clean
)

print(cox_model)

cox_table <- tidy(cox_model)

new_york_cox_model <- coxph(
  Surv(`Survival months`, `SEER cause-specific death classification` == "1") ~ 
    `Age recode with <1 year olds and 90+` +
    `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` +
    `Grade Recode (thru 2017)` +
    `Combined Summary Stage with Expanded Regional Codes (2004+)` +
    `Radiation recode (2003+)` +
    `Chemotherapy recode (yes, no/unk) (2004+)` +
    `Breast Subtype (2010+)` +
    `Derived HER2 Recode (2010+)` +
    `ER Status Recode Breast Cancer (2010+)` +
    `PR Status Recode Breast Cancer (2010+)` +
    `Median household income inflation adj to 2023` +
  `Total number of in situ/malignant tumors for patient`,
  data = new_york
)

print(new_york_cox_model)

california_cox_model <- coxph(
  Surv(`Survival months`, `SEER cause-specific death classification` == "1") ~ 
    `Age recode with <1 year olds and 90+` +
    `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` +
    `Grade Recode (thru 2017)` +
    `Combined Summary Stage with Expanded Regional Codes (2004+)` +
    `Radiation recode (2003+)` +
    `Chemotherapy recode (yes, no/unk) (2004+)` +
    `Breast Subtype (2010+)` +
    `Derived HER2 Recode (2010+)` +
    `ER Status Recode Breast Cancer (2010+)` +
    `PR Status Recode Breast Cancer (2010+)` +
    `Median household income inflation adj to 2023` +
  `Total number of in situ/malignant tumors for patient`,
  data = california
)

print(california_cox_model)


texas_cox_model <- coxph(
  Surv(`Survival months`, `SEER cause-specific death classification` == "1") ~ 
    `Age recode with <1 year olds and 90+` +
    `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)` +
    `Grade Recode (thru 2017)` +
    `Combined Summary Stage with Expanded Regional Codes (2004+)` +
    `Radiation recode (2003+)` +
    `Chemotherapy recode (yes, no/unk) (2004+)` +
    `Breast Subtype (2010+)` +
    `Derived HER2 Recode (2010+)` +
    `ER Status Recode Breast Cancer (2010+)` +
    `PR Status Recode Breast Cancer (2010+)` +
    `Median household income inflation adj to 2023` +
  `Total number of in situ/malignant tumors for patient`,
  data = texas
)

print(texas_cox_model)

# ------------------------------
# 2️⃣ Tidy the model output
# ------------------------------

tidy_cox <- tidy(cox_model, exponentiate = TRUE, conf.int = TRUE)

# ------------------------------
# 3️⃣ Clean it up for easy reading
# ------------------------------

tidy_pretty <- tidy_cox %>%
  select(term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    p.value = signif(p.value, 3)
  )

# View it in the console
print(tidy_pretty, n = Inf)

# ------------------------------
# 4️⃣ Save it to CSV (optional)
# ------------------------------

california_cox_table <- tidy(california_cox_model)  # raw log HRs
write.csv(california_cox_table, "new_california_cox_model.csv", row.names = FALSE)

new_york_cox_table <- tidy(new_york_cox_model)
write.csv(new_york_cox_table, "new_new_york_cox_model.csv", row.names = FALSE)

texas_cox_table <- tidy(texas_cox_model)
write.csv(texas_cox_table, "new_texas_cox_model.csv", row.names = FALSE)

# ------------------------------
# 5️⃣ Forest plot (optional)
# ------------------------------

ggplot(tidy_pretty, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "#4E79A7", size = 2) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  xlab("Hazard Ratio (HR)") +
  ylab("Variable") +
  theme_minimal(base_size = 12) +
  ggtitle("Cox Proportional Hazards: Hazard Ratios with 95% CI")


# 1. Add flag for breast cancer deaths
breast_cancer_data_clean <- breast_cancer_data_clean %>%
  mutate(is_bc_death = `SEER cause-specific death classification` == "1")

# 2. Total & breast cancer deaths per state
state_death_summary <- breast_cancer_data_clean %>%
  group_by(`SEER registry (with CA and GA as whole states)`) %>%
  summarise(
    total_cases = n(),
    bc_deaths = sum(is_bc_death, na.rm = TRUE),
    prop_bc_death = bc_deaths / total_cases,
    .groups = "drop"
  )

# 3. Breast cancer deaths by state & race
death_by_state_race <- breast_cancer_data_clean %>%
  filter(is_bc_death) %>%
  group_by(`SEER registry (with CA and GA as whole states)`,
           `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`) %>%
  summarise(count = n(), .groups = "drop")

# 4. Merge and calculate race proportions
plot_data <- death_by_state_race %>%
  left_join(state_death_summary, by = "SEER registry (with CA and GA as whole states)") %>%
  mutate(
    prop_race_within_bc = count / bc_deaths,
    scaled_height = pmin(prop_bc_death * prop_race_within_bc, 0.15), # cap petal height
    state = `SEER registry (with CA and GA as whole states)`,
    race = `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
  )

# 5. Order states by total proportional deaths (for consistent layout)
state_levels <- plot_data %>%
  group_by(state) %>%
  summarise(total = sum(scaled_height), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(state)

plot_data$state <- factor(plot_data$state, levels = state_levels)

# 6. Final Plot
ggplot(plot_data,
       aes(
         x = state,
         y = scaled_height,
         fill = race,
         group = race
       )) +
  geom_bar(stat = "identity", position = "stack", width = 1, color = "black") +
  coord_polar(start = 0, clip = "off") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 9, angle = 0, vjust = 0.5),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  labs(
    fill = "Race/Ethnicity",
    title = "Proportional Breast Cancer Deaths\nby State and Race (Nightingale Style)"
  )


