# Last update: 11/24/2023
# Author: Georgia Liu 

# In Poisson_Regression_Preparation_Diagnostic.R code, we 
# This code covers various aspects of regression model diagnostics for count data, 
# specifically for a negative binomial regression model. 
# This code will include residual analysis, checking for overdispersion, goodness-of-fit tests, 
# and comparing models. 

# Loading required libraries
library(MASS) # For negative binomial regression
library(lmtest) # For diagnostic tests
library(ggplot2) # For plotting
library(AER) # For model diagnostics
library(fixest)

load("Result/df_preprocess.RData")

df$DATE_EVENT <- as.character(df$DATE_EVENT)


# Helper function to create formula
make_formula <- function(dependent_var, independent_vars, fixed_effects) {
  as.formula(paste(dependent_var, "~", paste(independent_vars, collapse = " + "), "|", fixed_effects))
}

# Define regression formula
formula1 <- make_formula("Error_Count", c("TotalWL", "Auto_fraction", "PHONE", "JUSTIF_count", 
                                          "SAFETY_count", "MESSAGE_OTHER_count", "Hour", 
                                          "TRAF_COMP", "hours_worked"), "DATE_EVENT + RSE_ID_ANONYM")
print(formula1)

# Fit a negative binomial model
# nb_model <- fenegbin(formula1 , data = df)
nb_model <- glm.nb(Error_Count ~ TotalWL + Auto_fraction + PHONE + JUSTIF_count + 
                     SAFETY_count + MESSAGE_OTHER_count + Hour + TRAF_COMP + hours_worked + 
                     factor(DATE_EVENT) + factor(RSE_ID_ANONYM), data = df)
save(nb_model, file = "Result/nb_model.RData")
load("Result/nb_model.RData")

# Fit a Poisson model
pois_model_fixed <- fepois(formula1, data = df)
save(pois_model_fixed, file = "Result/pois_model_fixed.RData")

load("Result/pois_model_fixed.RData")

model_result <- etable(nb_model, pois_model_fixed)
write.csv(model_result, "Result/fixe_poisson_nb_result.csv", row.names = TRUE)

# Residual Analysis
# Plotting deviance residuals
plot(pois_model_fixed$fitted.values, resid(pois_model_fixed, type = "deviance"),
     xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(h = 0, col = "red")


# Checking for Overdispersion
dispersiontest(pois_model_fixed)

# Goodness-of-Fit Test
# Compare against a Poisson model
# poisson_model <- glm(count_response ~ predictor1 + predictor2, data = data, family = "poisson")
lrtest(nb_model, pois_model)

# Influence Diagnostics (for example, using Cook's distance)
cooksd <- cooks.distance(nb_model)
plot(cooksd, type="h", ylab="Cook's distance")

# AIC for Model Comparison
AIC(nb_model, pois_model_fixed)
