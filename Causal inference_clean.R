rm(list=ls())
library(tableone) 
library(Matching)
library(dplyr)
library(ggplot2)
library(texreg)
library(ggmosaic)
library(fastDummies)
library(MatchIt)
library(survival)

df<-read.csv("breast_cancer_data_clean")[,-1]

################################################################
# Matching based on stage "Distant" = "0","Localized" = "1" as our treatment
################################################################
integer_cols <- ls(select_if(df, is.integer))
for (col in integer_cols) {
  df[col] <- as.numeric(unlist(df[col]))
}
# filter the cases with Stage =0 or 1
df_filt<- subset(df, df$Stage < 2)
# choose the covariates from the dataframe
df_sub <- df_filt[,c("Race and origin recode (NHW, NHB, NHAIAN, NHAPI, Hispanic)","Age recode with <1 year olds and 90+","Grade Recode (thru 2017)","Combined Summary Stage with Expanded Regional Codes (2004+)","Radiation recode (2003+)","Breast Subtype (2010+)","Derived HER2 Recode (2010+)","ER Status Recoded Breast Cancer (2010+)", "PR Status Recoded Breast Cancer (2010+)","SEER cause-specific death classification","Survival months","chemo","Total number of in situ/malignant tumors for patient")]

# run the first round of matching algorithm

greedymatch <- Match(Tr=df_sub$Stage ,M=1, X=df_sub[vars], replace=FALSE) 
matched <- df_sub[unlist(greedymatch[c("index.treated","index.control")]), ]
matchedtab1 <- CreateTableOne(vars=vars,strata="Stage", data=matched, test=T) # => Complete the command to create table 1 after matching
# => Print the balance with smd. 
print(matchedtab1, smd=TRUE)# not completelt balanced


# Adding Caliper, , Weight=2
greedymatch_mdm <- Match(Tr=df_sub$Stage ,M=1, X=df_sub[vars],Weight=2,caliper = 0.2,ties = F) 
matched_mdm <- df_sub[unlist(greedymatch_mdm[c("index.treated","index.control")]), ]
###mahalanobis
#greedymatch_mdm <- Match(Tr = df_sub$Stage, M = 1, X = df_sub[vars],
#                         distance = "mahalanobis", caliper = 0.3, ties = FALSE)

matchedtab2 <- CreateTableOne(vars=vars,strata="Stage", data=matched_mdm, test=T)
print(matchedtab2, smd=TRUE)
######################################
y_distant <- matched_mdm[matched_mdm$Stage == 0, 'Event'] # => Put here the vector of outcomes y (from matched$outcome.died) of those in the treatment group
y_localized <- matched_mdm[matched_mdm$Stage == 1, 'Event'] # Put here the vector of outcomes y (from matched$outcome.die

diffy <- y_distant-y_localized # pairwise difference
t.test(y_distant,y_localized,paired = T,conf.level = 0.95)

# Paired t-test
# 
# data:  y_distant and y_localized
# t = 33.964, df = 3713, p-value < 2.2e-16
# alternative hypothesis: true mean difference is not equal to 0
# 95 percent confidence interval:
#   0.3224637 0.3619736
# sample estimates:
#   mean difference 
# 0.3422186 
# Perform Wilcoxon
wilcox_result <- wilcox.test(y_distant, y_localized, paired = TRUE)

# Display the result
print(wilcox_result)
# Wilcoxon signed rank test with continuity correction
# 
# data:  y_distant and y_localized
# V = 1425654, p-value < 2.2e-16
# alternative hypothesis: true location shift is not equal to 0
################
ate <- mean(y_distant- y_localized)

# Display the average treatment effect
print(ate)
##########################################
##distance = "mahalanobis"
match_out <- matchit(Stage ~ Sex+Race+PSite+Age, 
                    data = df_sub, 
                    method = "nearest", 
                    distance = "mahalanobis")
matched_data <- match.data(match_out)

# View the first few rows of the matched data
head(matched_data)

################
## Counterfacual effect
######################
df_dist<-subset(matched_mdm,matched_mdm$Stage == 0)
df_loc<-subset(matched_mdm,matched_mdm$Stage == 1)

fit<-coxph(Surv(Duration, Event) ~ Sex+ Race+Age+No.Tumors+PSite+Chemotherapy+Grade+Stage+Systemic.Therapy, data = matched_mdm)
surv_dist <- survfit(fit, newdata = df_dist)
surv_loc <- survfit(fit, newdata = df_loc)
ate <- mean(surv_loc$surv - surv_dist$surv)


# Basic plot setup
plot(surv_loc, col = "blue", lty = 1, lwd = 2, xlab = "Time", ylab = "Survival Probability", 
     main = "Survival Probability for Localized vs. Distant Stages")
lines(surv_dist, col = "red", lty = 2, lwd = 2)

# Add a legend
legend("topright", legend = c("Localized Stage", "Distant Stage"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)
##############
newdata_localized <- data.frame(Stage = 1, 
                                Age = 0,  # example age
                                Sex = 1, 
                                Race = 0, 
                                Grade = 2, 
                                No.Tumors = 1,PSite=1,Chemotherapy=1,Systemic.Therapy=1)

newdata_distant <- data.frame(Stage = 0, 
                              Age = 0, 
                              Sex = 1, 
                              Race = 0, 
                              Grade = 2, 
                              No.Tumors = 1,PSite=1,Chemotherapy=1,Systemic.Therapy=1)



surv_localized <- survfit(fit, newdata = newdata_localized)
surv_distant <- survfit(fit, newdata = newdata_distant)

# Basic plot setup
plot(surv_localized, col = "blue", lty = 1, lwd = 2, xlab = "Time", ylab = "Survival Probability", 
     main = "Survival Probability for Localized vs. Distant Stages")
lines(surv_distant, col = "red", lty = 2, lwd = 2)

# Add a legend
legend("topright", legend = c("Localized Stage", "Distant Stage"), 
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)
#### Plot in ggplot
library(survival)
library(ggplot2)
library(dplyr)
mean(surv_localized[["surv"]]-surv_distant[["surv"]])
# Convert survfit objects to data frames
localized_df <- data.frame(
  time = surv_localized$time,
  surv = surv_localized$surv,
  lower = surv_localized$lower,
  upper = surv_localized$upper,
  Stage = "Localized"
)

distant_df <- data.frame(
  time = surv_distant$time,
  surv = surv_distant$surv,
  lower = surv_distant$lower,
  upper = surv_distant$upper,
  Stage = "Distant"
)

# Combine into one data frame for ggplot
surv_df <- bind_rows(localized_df, distant_df)
#write.csv(surv_df,"counter_effect.csv")
##plot
ggplot(surv_df, aes(x = time, y = surv, color = Stage, fill = Stage)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, linetype = "dashed") +  # Dashed confidence interval
  labs(title = "",
       x = "Time",
       y = "Survival Probability") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA),  # Box around plot
    axis.text.x = element_text(size = 14),                    # Increase x-axis font size
    axis.title.x = element_text(size = 16),                   # Increase x-axis label font size
    axis.text.y = element_text(size = 14),                    # Increase y-axis font size
    axis.title.y = element_text(size = 16),                   # Increase y-axis label font size
    legend.title = element_text(size = 16),                   # Increase legend title size
    legend.text = element_text(size = 14)                     # Increase legend categories size
  ) +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red"))
