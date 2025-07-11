# ================================
# 📦 Import libraries
# ================================
import pandas as pd
import numpy as np
from sksurv.ensemble import RandomSurvivalForest
from sksurv.metrics import concordance_index_censored
from sklearn.model_selection import train_test_split

# ================================
# 🗂️ Load data
# ================================
df = pd.read_csv("breast_cancer_data_new.csv")

# ================================
# 🧹 Clean & preprocess
# ================================

# Remove male
df = df[df["Sex"] != "Male"]

# Only include women under 40
df = df[df["Age recode with <1 year olds and 90+"].isin([
    "01-04 years", "05-09 years", "10-14 years", "15-19 years",
    "20-24 years", "25-29 years", "30-34 years", "35-39 years"
])]

# Drop unneeded columns (adjust index as needed)
df = df.drop(df.columns[[1, 29, 31, 35]], axis=1)

# Remove missing income
df = df[df["Median household income inflation adj to 2023"] != "Unknown/missing/no match/Not 1990-2023"]

# Recode income
df["Median household income inflation adj to 2023"] = df["Median household income inflation adj to 2023"].replace({
    "< $40,000": "<$50,000", "$40,000 - $44,999": "<$50,000", "$45,000 - $49,999": "<$50,000",
    "$50,000 - $54,999": "$50,000-$74,999", "$55,000 - $59,999": "$50,000-$74,999",
    "$60,000 - $64,999": "$50,000-$74,999", "$65,000 - $69,999": "$50,000-$74,999",
    "$70,000 - $74,999": "$75,000-$99,999", "$75,000 - $79,999": "$75,000-$99,999",
    "$80,000 - $84,999": "$75,000-$99,999", "$85,000 - $89,999": "$75,000-$99,999",
    "$90,000 - $94,999": "$75,000-$99,999", "$95,000 - $99,999": "$75,000-$99,999",
    "$100,000 - $109,999": ">= $100,000", "$110,000 - $119,999": ">= $100,000",
    "$120,000+": ">= $100,000"
})

# Recode cause-specific death
df["SEER cause-specific death classification"] = df["SEER cause-specific death classification"].replace({
    "Alive or dead of other cause": 0,
    "Dead (attributable to this cancer dx)": 1
})

# Fix numeric survival time
df["Survival months"] = pd.to_numeric(df["Survival months"], errors="coerce")

# Make categorical variables
categoricals = [
    "Age recode with <1 year olds and 90+",
    "Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)",
    "Grade Recode (thru 2017)",
    "Combined Summary Stage with Expanded Regional Codes (2004+)",
    "Radiation recode (2003+)",
    "Chemotherapy recode (yes, no/unk) (2004+)",
    "Breast Subtype (2010+)",
    "Derived HER2 Recode (2010+)",
    "ER Status Recode Breast Cancer (2010+)",
    "PR Status Recode Breast Cancer (2010+)",
    "Median household income inflation adj to 2023",
    "Total number of in situ/malignant tumors for patient"
]

for col in categoricals:
    df[col] = df[col].astype("category")

# Drop any rows with missing values
df = df.dropna()

# ================================
# ✂️ Train-test split
# ================================
X = pd.get_dummies(df[categoricals], drop_first=True)
y_struct = np.array([
    (bool(e), t) for e, t in zip(df["SEER cause-specific death classification"], df["Survival months"])
], dtype=[("event", bool), ("time", float)])

X_train, X_test, y_train, y_test = train_test_split(X, y_struct, test_size=0.2, random_state=42)

# ================================
# 🌲 Fit Random Survival Forest
# ================================
rsf = RandomSurvivalForest(n_estimators=200, min_samples_split=10, min_samples_leaf=5, n_jobs=-1, random_state=42)
rsf.fit(X_train, y_train)

# ================================
# 🏅 Evaluate C-index
# ================================
pred_surv = rsf.predict(X_test)
c_index = concordance_index_censored(
    y_test["event"],
    y_test["time"],
    rsf.predict(X_test)
)

print(f"C-index on test data: {c_index[0]:.4f}")
