# Last update: 11/23/2023
# Author: Georgia Liu 
# This code covers various aspects of regression model diagnostics for count data, 
# specifically for a negative binomial regression model. 
# This code will include residual analysis, checking for overdispersion, goodness-of-fit tests, 
# and comparing models. 
# Loading required libraries


library(MASS) # For negative binomial regression
library(lmtest) # For diagnostic tests
library(ggplot2) # For plotting
library(AER) # For model diagnostics

# Assuming you have a dataset `data` and you are interested in modeling `count_response`
# against predictors `predictor1`, `predictor2`, etc.

df$DATE_EVENT <- as.character(df$DATE_EVENT)
# Fit a negative binomial model
nb_model <- glm.nb(Error_Count ~ TotalWL + Auto_fraction 
                   +PHONE+JUSTIF_count + MESSAGE_OTHER_count  + TRAF_COMP
                  , data = df)

pois_model <- glm(Error_Count ~ TotalWL + Auto_fraction 
                   +PHONE+JUSTIF_count + MESSAGE_OTHER_count  + Hour + 
                    TRAF_COMP + TRAF_DENS_1 | DATE_EVENT+RSE_ID_ANONYM 
                   , data = df, family = poisson)

# Summary of the model
summary(pois_model)

# Residual Analysis
# Plotting deviance residuals
plot(pois_model$fitted.values, resid(pois_model, type = "deviance"),
     xlab = "Fitted Values", ylab = "Deviance Residuals")
abline(h = 0, col = "red")

# Checking for Overdispersion
dispersiontest(pois_model)

# Goodness-of-Fit Test
# Compare against a Poisson model
poisson_model <- glm(count_response ~ predictor1 + predictor2, data = data, family = "poisson")
lrtest(nb_model, poisson_model)

# Influence Diagnostics (for example, using Cook's distance)
cooksd <- cooks.distance(nb_model)
plot(cooksd, type="h", ylab="Cook's distance")

# AIC for Model Comparison
AIC(nb_model, poisson_model)

# Optionally, you can include cross-validation or other advanced diagnostics as needed
