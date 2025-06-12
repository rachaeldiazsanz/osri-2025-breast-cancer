library(readr)

# reading the file
breast_cancer_data_new <- read_csv("Desktop/2025-osri-breast-cancer/breast_cancer_data_new.csv")

# remove male variable
breast_cancer_data_clean = subset(breast_cancer_data_new, breast_cancer_data_new$Sex != "Male")
