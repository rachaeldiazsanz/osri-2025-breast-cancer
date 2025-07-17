# Load libraries
library(randomForestSRC)
library(ggplot2)

# -----------------------------------------------
# Fit Random Survival Forest
# -----------------------------------------------
rsf_model <- rfsrc(
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
  data = breast_cancer_data_clean,
  ntree = 1000,
  nodesize = 15,
  importance = TRUE
)

# Print basic results
print(rsf_model)

# Built-in plot
plot(rsf_model)

# -----------------------------------------------
# Make pretty variable importance plot 
# -----------------------------------------------
imp_df <- data.frame(
  Variable = names(rsf_model$importance),
  Importance = rsf_model$importance
) |>
  transform(Variable = reorder(Variable, Importance))

ggplot(imp_df, aes(x = Variable, y = Importance)) +
  geom_col(fill = "#70C1A6") +  # Minty green
  coord_flip() +
  labs(
    title = "Variable Importance (Random Survival Forest)",
    x = "Variable",
    y = "Importance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

# Save plot
ggsave("rsf_variable_importance.png", width = 8, height = 6, dpi = 300)
