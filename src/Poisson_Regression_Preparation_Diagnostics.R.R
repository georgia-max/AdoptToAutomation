# Last update: 11/24/2023 
# Author: Georgia Liu
# This code performs model diagnostics for Poisson regression

# Loading required libraries
library(MASS)    # For overdispersion test
library(pscl)    # For zero-inflation test
library(ggplot2) # For plotting
library(dplyr)   # For data manipulation

# Load preprocessed data
load("Result/df_preprocess.RData")

# Correlation Analysis
# Select relevant columns for correlation analysis
df2 <- df %>% 
  select(Error_rate, Error_Count, Auto_fraction, TotalWL, TotalWL_weight,
         MOVE_AUTO_count, MOVE_MAN_count, MOVE_MAN_weight, MOVE_AUTO_weight, 
         CHANGE_AUTO_WW_count, CHANGE_AUTO_AA_count, CHANGE_AUTO_DD_count, 
         CHANGE_AUTO_REMOVE_TIMER_count, SAFETY_count, PHONE_IN_count, PHONE_OUT_count, 
         MESSAGE_OTHER_count, PHONE, JUSTIF_count, Hour, hours_worked, 
         ADAPTATIONS_count, TRAF_COMP, TRAF_DENS_1)

# Calculate and print correlation matrix
vcorr <- cor(df2, use = "complete.obs") %>% round(3)
print(vcorr)

# Optional: Write correlation matrix to CSV file
write.csv(vcorr, "Result/CRIPTON_60_MINUTES_anonymized_all_corr.csv", row.names = TRUE)

# Helper function to create regression formula
make_formula <- function(dependent_var, independent_vars) {
  as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
}

# Define regression formula
formula1 <- make_formula("Error_Count", c("TotalWL", "Auto_fraction", "PHONE", "JUSTIF_count", 
                                          "SAFETY_count", "MESSAGE_OTHER_count", "Hour", 
                                          "TRAF_COMP", "hours_worked"))
print(formula1)

# Independence of Observations (for time-series data)
acf(df$Error_Count)

# Testing for Overdispersion
pois_model <- glm(formula1, data = df, family = "poisson")
dispersion_test <- sum(resid(pois_model, type = "pearson")^2) / df.residual(pois_model)
print(dispersion_test) # Values significantly greater than 1 indicate overdispersion

# Save Poisson model
save(pois_model, file = "Result/pois_model.RData")

# Save summary of Poisson model
model_summary <- summary(pois_model)
summary_df <- as.data.frame(coef(model_summary))
write.csv(summary_df, "Result/pois_model_summary.csv", row.names = TRUE)

# Zero-Inflation Check
zip_model_all <- zeroinfl(formula1, data = df, dist = "poisson")
summary(zip_model_all)

# Save zero-inflated model
save(zip_model_all, file = "Result/zip_model_all.RData")

# Exponentiated coefficients for interpretation
expCoef <- coef(zip_model_all) %>% exp() %>% matrix(ncol = 2)
rownames(expCoef) <- names(coef(zip_model_all))[1:10]
colnames(expCoef) <- c("Count_model", "Zero_inflation_model")
print(expCoef)

# Save exponentiated coefficients
expCoef_summary_df <- as.data.frame(expCoef)
write.csv(expCoef_summary_df, "Result/zip_model_expcoef_summary.csv", row.names = TRUE)

# Note: The count model coefficients are for the Poisson part and the (Intercept) in the zero-inflation part.
# Significant zero-inflation part indicates zero-inflation in the data.

# Result: 
# 1. There is overdispersion in the poisson model
# 2. The zero-inflated model shows a lot of NAs, so therefore we do not use the zero-inflated model. 

