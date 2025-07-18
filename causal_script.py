import pandas as pd
import numpy as np
from sklearn.linear_model import LogisticRegression
from sklearn.neighbors import NearestNeighbors
from lifelines import KaplanMeierFitter, CoxPHFitter
import matplotlib.pyplot as plt

# Load the dataset
df = pd.read_csv("python_data.csv")

# Rename columns for simplicity
df = df.rename(columns={
    "race_numeric": "race",
    "radiation_numeric": "radiation",
    "chemo_numeric": "chemo",
    "subtype_numeric": "subtype",
    "age_group_numeric": "age_group",
    "Grade Recode (thru 2017)": "grade",
    "Combined Summary Stage with Expanded Regional Codes (2004+)": "stage",
    "Derived HER2 Recode (2010+)": "her2",
    "ER Status Recode Breast Cancer (2010+)": "er",
    "PR Status Recode Breast Cancer (2010+)": "pr",
    "Total number of in situ/malignant tumors for patient": "tumor_count",
    "Survival months": "survival_months",
    "event": "event"
})

# Filter only Localized (1) and Distant (3) stages
df = df[df["stage"].isin([1, 3])]

# Define binary treatment: 1 = Localized, 0 = Distant
df["stage_binary"] = (df["stage"] == 1).astype(int)

# Covariates for propensity score estimation
covariates = ["race", "radiation", "chemo", "subtype", "age_group", "grade", "her2", "er", "pr", "tumor_count"]

# Convert any object-type covariates to numeric and shift to start from 0
for col in covariates:
    if df[col].dtype == "object":
        df[col] = df[col].str.extract(r"(\d+)")  # extract numeric part
        df[col] = pd.to_numeric(df[col], errors="coerce")
        
    # Shift values to start from 0
    if df[col].notnull().any():
        df[col] = df[col] - df[col].min()

# Drop rows with missing values
df = df.dropna(subset=covariates + ["survival_months", "event", "stage_binary"])

# Propensity score model
X = df[covariates]
y = df["stage_binary"]

prop_model = LogisticRegression(max_iter=1000)
prop_model.fit(X, y)
df["propensity_score"] = prop_model.predict_proba(X)[:, 1]

# Nearest neighbor matching (1:1)
treated = df[df["stage_binary"] == 1].copy()  # Localized
control = df[df["stage_binary"] == 0].copy()  # Distant

nn = NearestNeighbors(n_neighbors=1)
nn.fit(control[["propensity_score"]])
distances, indices = nn.kneighbors(treated[["propensity_score"]])

matched_control = control.iloc[indices.flatten()].copy()
matched_treated = treated.reset_index(drop=True)
matched_df = pd.concat([matched_treated, matched_control], ignore_index=True)

# Kaplan-Meier plot
kmf_treat = KaplanMeierFitter()
kmf_control = KaplanMeierFitter()

plt.figure(figsize=(8, 6))
kmf_control.fit(matched_control["survival_months"], matched_control["event"], label="Distant")
kmf_treat.fit(matched_treated["survival_months"], matched_treated["event"], label="Localized")

kmf_control.plot_survival_function()
kmf_treat.plot_survival_function()
plt.title("Kaplan-Meier Survival Curves (Matched)")
plt.xlabel("Survival Time (Months)")
plt.ylabel("Survival Probability")
plt.ylim(0.2, 1.0)
plt.grid(True)
plt.show()

# Cox proportional hazards model
matched_df["treatment"] = matched_df["stage_binary"]
cox = CoxPHFitter()
cox.fit(matched_df[["survival_months", "event", "treatment"] + covariates], 
        duration_col="survival_months", event_col="event")
cox.print_summary()