# Nightingale Rose Charts

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




library(dplyr)
library(ggplot2)

# 1. Filter only breast cancer deaths
bc_deaths_only <- breast_cancer_data_clean %>%
  filter(`SEER cause-specific death classification` == "1")

# 2. Breast cancer deaths by subtype and race — drop "Recode not available"
subtype_plot_data <- bc_deaths_only %>%
  filter(`Breast Subtype (2010+)` != "Recode not available") %>%
  group_by(
    subtype = `Breast Subtype (2010+)`,
    race = `Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)`
  ) %>%
  summarise(
    count = n(),
    .groups = "drop"
  )

# 3. Order subtype petals by total death count
subtype_levels <- subtype_plot_data %>%
  group_by(subtype) %>%
  summarise(total = sum(count), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(subtype)

subtype_plot_data$subtype <- factor(subtype_plot_data$subtype, levels = subtype_levels)

# 4. Plot with raw counts!
ggplot(subtype_plot_data,
       aes(
         x = subtype,
         y = count,
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
    title = "Breast Cancer Death Counts\nby Subtype and Race (Nightingale Style)"
  )
