# Last update: 11/23/2023 
# Author: Georgia Liu This code preforms model
# diagnostic for poisson regression


# Loading required libraries
library(MASS)    # For overdispersion test
library(pscl)    # For zero-inflation test
library(ggplot2) # For plotting
library(dplyr)

# Assuming you have a data frame named 'data' with a count variable 'count_var' and predictors

# 1. Correlation Analysis
df2 <- df %>% 
  dplyr::select(Error_rate, Error_Count, Auto_fraction, TotalWL, MOVE_AUTO_count, MOVE_MAN_count,
                MOVE_MAN_weight, MOVE_AUTO_weight, 
                CHANGE_AUTO_WW_count, CHANGE_AUTO_AA_count, CHANGE_AUTO_DD_count, CHANGE_AUTO_REMOVE_TIMER_count, 
                SAFETY_count, PHONE_IN_count, PHONE_OUT_count, MESSAGE_OTHER_count, PHONE,  JUSTIF_count, 
         Hour, hours_worked, ADAPTATIONS_count, TRAF_COMP, TRAF_DENS_1)

vcorr <- round(cor(df2, use = "complete.obs"), 3)
print(vcorr)

# Write correlation matrix to Excel file (commented out)
# write.csv(vcorr, "./Result/CRIPTON_60_MINUTES_anonymized_all_corr.csv", row.names = TRUE)


# Helper function to create formula
make_formula <- function(dependent_var, independent_vars) {
  as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + ")))
}

# Define regression formulas
formula1 <- make_formula("Error_Count", c("TotalWL", "Auto_fraction", "PHONE", "JUSTIF_count", 
                                          "MESSAGE_OTHER_count", "Hour", "TRAF_COMP", "TRAF_DENS_1"))
print(formula1)

# 2. Independence of Observations
# This is more conceptual and depends on your data structure. 
# For time-series data, you might use:
acf(df$Error_Count)

# 4. Testing for Overdispersion
pois_model <- glm(formula1, data = df, family = "poisson")
dispersion_test <- sum(resid(pois_model, type = "pearson")^2) / df.residual(pois_model)
print(dispersion_test) # Values significantly greater than 1 indicate overdispersion

# 5. Zero-Inflation Check
# Fit a zero-inflated model and compare
zip_model <- zeroinfl(Error_Count ~ TotalWL + Auto_fraction + PHONE + JUSTIF_count + 
                        MESSAGE_OTHER_count + Hour + TRAF_COMP + TRAF_DENS_1 | DATE_EVENT+RSE_ID_ANONYM , data = df, dist = "poisson")
summary(zip_model)
# The count model coefficients are for the Poisson part and the (Intercept) in the zero-inflation part.
# Significant zero-inflation part indicates zero-inflation in the data.

# Note: Interpretation of these tests and results should be done in the context of your specific data and research question.
