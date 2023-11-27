# Last update: 11/23/2023
# Author: Georgia Liu 
# This code is meant to run the 2 stage NB regression to test endogeneity. 
# Dependent variable: Error_Count (count data)
# Suspected endogenous explanatory variable: TotalWL_weight, AUTO_fraction
# Other control variables: PHONE + JUSTIF_count + 
# SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked + 
#   factor(DATE_EVENT) + factor(RSE_ID_ANONYM) 
# Instrumental variable for TotalWL: hours_worked

library(MASS)
library(AER)
library(lmtest)
library(fixest)
library(modelsummary)

load("Result/df_preprocess.RData")

# Table 1. 2-stage NB regression ----------
first_stage_Auto_fraction = feols(Auto_fraction ~ TRAF_COMP + TRAF_DENS_1 + PHONE + JUSTIF_count + 
               SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
             DATE_EVENT + RSE_ID_ANONYM, vcov = "twoway", data = df)

save(first_stage_Auto_fraction, file = "Result/two_stage_NBRegression/first_stage_Auto_fraction.RData")

first_stage_TotalWL_weight = feols(TotalWL_weight ~ TRAF_COMP + TRAF_DENS_1 + PHONE + JUSTIF_count + 
                                    SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                                    DATE_EVENT + RSE_ID_ANONYM, vcov = "twoway", data = df)

save(first_stage_TotalWL_weight, file = "Result/two_stage_NBRegression/first_stage_TotalWL_weight.RData")


df$TotalWL_fitted <- predict(first_stage_TotalWL_weight, df)
df$Auto_fraction_fitted <- predict(first_stage_Auto_fraction, df)


#Fit the Main Negative Binomial Model (with Original Endogenous Variable)
nb_model_original <- fenegbin(Error_Count ~ TotalWL_weight + Auto_fraction + PHONE + JUSTIF_count + 
                              SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                              DATE_EVENT + RSE_ID_ANONYM,vcov = "twoway", data = df)
save(nb_model_original, file = "Result/two_stage_NBRegression/nb_model_original.RData")

#Fit the Negative Binomial Model with Predicted x (from First Stage)
nb_model_fitted <- fenegbin(Error_Count ~ TotalWL_fitted + Auto_fraction_fitted + PHONE + JUSTIF_count + 
                              SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                              DATE_EVENT + RSE_ID_ANONYM,vcov = "twoway", data = df)
save(nb_model_fitted, file = "Result/two_stage_NBRegression/nb_model_fitted.RData")

# Extracting coefficients
coef_original <- coef(nb_model_original)["TotalWL_weight"]
coef_fitted <- coef(nb_model_fitted)["TotalWL_fitted"]


main_results<- etable( first_stage_Auto_fraction, first_stage_TotalWL_weight,nb_model_fitted, nb_model_original) 

write.csv(main_results, file = "Result/two_stage_NBRegression/main_results.csv") 

# Perform the Durbin-Wu-Hausman Test
# The idea is to test whether the coefficients of x in nb_model_original and 
# x_fitted in nb_model_fitted are statistically different.
# Variance of the difference
var_diff <- vcov(nb_model_original)["TotalWL_weight", "TotalWL_weight"] + vcov(nb_model_fitted)["TotalWL_fitted", "TotalWL_fitted"]

# Durbin-Wu-Hausman test statistic
dwh_statistic <- (coef_original - coef_fitted)^2 / var_diff
dwh_pvalue <- 1 - pchisq(dwh_statistic, df = 1)

print(c("DWH Statistic" = dwh_statistic, "P-value" = dwh_pvalue))

# Regression table setup 
estimate_format <- "{estimate} ({std.error}){stars}"
note_content <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')

# Define models for the table
models_list <- list(
  "(1) First stage: Auto Fraction" = first_stage_Auto_fraction,
  "(2) First stage: TotalWL Weight" = first_stage_TotalWL_weight,
  "(3) Second stage: NB regression fitted" = nb_model_fitted, 
  "(4) NB regression Original" =  nb_model_original
)

# Generate the regression table
regression_table <- modelsummary(
  models_list,
  stars = TRUE,
  title = "Two-stage NB Regression model result",
  output = "gt",
  estimate = estimate_format,
  statistic = NULL,  # Optional: Specify a statistic like "p.value" or "t.value" if needed
  coef_omit = "theta",  # Optional: Specify coefficients to omit if needed
  notes = note_content
  # Optional: Uncomment and specify additional options like gof_map or coef_map if needed
  # gof_map = gof_map,
  # coef_map = coef_map,
  # gof_omit = gof_omit
)

# Print the regression table
print(regression_table)
gt::gtsave(regression_table,filename = "Result/two_stage_NBRegression/main_results.docx")


# Table 2. Linear, Quadratic and Interaction NB Regression model  -------

df$TotalWL_fitted_sq <- df$TotalWL_fitted^2 
df$Auto_fraction_fitted_sq <- df$Auto_fraction_fitted^2  

nb_model_quadractic_fitted <- fenegbin(Error_Count ~ TotalWL_fitted + Auto_fraction_fitted + TotalWL_fitted_sq + Auto_fraction_fitted_sq + 
                              PHONE + JUSTIF_count + 
                              SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                              DATE_EVENT + RSE_ID_ANONYM,vcov = "twoway", data = df)

save(nb_model_quadractic_fitted, file ="Result/two_stage_NBRegression/nb_model_quadractic_fitted.RData")

nb_model_interaction_fitted <- fenegbin(Error_Count ~ TotalWL_fitted + Auto_fraction_fitted + TotalWL_fitted_sq + Auto_fraction_fitted_sq + 
                                          TotalWL_fitted * Auto_fraction_fitted + TotalWL_fitted_sq*Auto_fraction_fitted_sq + 
                                          PHONE + JUSTIF_count + 
                              SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                              DATE_EVENT + RSE_ID_ANONYM,vcov = "twoway", data = df)

save(nb_model_interaction_fitted, file ="Result/two_stage_NBRegression/nb_model_interaction_fitted.RData")

table2 <- etable(nb_model_fitted,nb_model_quadractic_fitted, nb_model_interaction_fitted) 
# write.csv(table2, "Result/table2.csv", row.names = TRUE)

# Regression table setup 
estimate_format <- "{estimate} ({std.error}){stars}"
note_content <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')

# Define models for the table
models_list <- list(
  "(1) Linear" = nb_model_fitted,
  "(2) Quadratic" = nb_model_quadractic_fitted,
  "(3) Interaction: NB regression fitted" = nb_model_interaction_fitted
)

# Generate the regression table
regression_table2 <- modelsummary(
  models_list,
  stars = TRUE,
  title = "NB Regression model result (3 model)",
  output = "gt",
  estimate = estimate_format,
  statistic = NULL,  # Optional: Specify a statistic like "p.value" or "t.value" if needed
  coef_omit = "theta",  # Optional: Specify coefficients to omit if needed
  notes = note_content
  # Optional: Uncomment and specify additional options like gof_map or coef_map if needed
  # gof_map = gof_map,
  # coef_map = coef_map,
  # gof_omit = gof_omit
)

# Print the regression table
print(regression_table2)
gt::gtsave(regression_table2,filename = "Result/two_stage_NBRegression/regression_table2.docx")

####### Robustness Check    ####### 

# Table 3. 2-stage OLS regression ----------

#Fit the Main Negative Binomial Model (with Original Endogenous Variable)
ols_model_original <- feols(Error_rate ~ TotalWL_weight + Auto_fraction + PHONE + JUSTIF_count + 
                                SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                                DATE_EVENT + RSE_ID_ANONYM,vcov = "twoway", data = df)
save(ols_model_original, file = "Result/OLS_Regression/ols_model_original.RData")

#Fit the Negative Binomial Model with Predicted x (from First Stage)
ols_model_fitted <- feols(Error_rate ~ TotalWL_fitted + Auto_fraction_fitted + PHONE + JUSTIF_count + 
                              SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                              DATE_EVENT + RSE_ID_ANONYM,vcov = "twoway", data = df)
save(ols_model_fitted, file = "Result/OLS_Regression/ols_model_fitted.RData")



# Extracting coefficients
coef_original <- coef(ols_model_original)["TotalWL_weight"]
coef_fitted <- coef(ols_model_fitted)["TotalWL_fitted"]


ols_main_results<- etable( first_stage_Auto_fraction, first_stage_TotalWL_weight, ols_model_fitted, nb_model_original) 

write.csv(ols_main_results, file = "Result/OLS_Regression/ols_main_results.csv") 

# Regression table setup 
estimate_format <- "{estimate} ({std.error}){stars}"
note_content <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')

# Define models for the table
models_list <- list(
  "(1) First stage: Auto Fraction" = first_stage_Auto_fraction,
  "(2) First stage: TotalWL Weight" = first_stage_TotalWL_weight,
  "(3) Second stage: OLS regression fitted" = nb_model_fitted, 
  "(4) OLS regression Original" =  nb_model_original
)

# Generate the regression table
regression_table <- modelsummary(
  models_list,
  stars = TRUE,
  title = "Two-stage OLS Regression model result",
  output = "gt",
  estimate = estimate_format,
  statistic = NULL,  # Optional: Specify a statistic like "p.value" or "t.value" if needed
  coef_omit = "theta",  # Optional: Specify coefficients to omit if needed
  notes = note_content
  # Optional: Uncomment and specify additional options like gof_map or coef_map if needed
  # gof_map = gof_map,
  # coef_map = coef_map,
  # gof_omit = gof_omit
)

# Print the regression table
print(regression_table)
gt::gtsave(regression_table,filename = "Result/OLS_Regression/ols_main_results.docx")

# Table 4. Linear, Quadratic and Interaction OLS Regression model -------------

ols_model_quadractic_fitted <- feols(Error_rate ~ TotalWL_fitted + Auto_fraction_fitted + TotalWL_fitted_sq + Auto_fraction_fitted_sq + 
                                         PHONE + JUSTIF_count + 
                                         SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                                         DATE_EVENT + RSE_ID_ANONYM,vcov = "twoway", data = df)

save(ols_model_quadractic_fitted, file ="Result/OLS_Regression/ols_model_quadractic_fitted.RData")

ols_model_interaction_fitted <- feols(Error_rate ~ TotalWL_fitted + Auto_fraction_fitted + TotalWL_fitted_sq + Auto_fraction_fitted_sq + 
                                          TotalWL_fitted * Auto_fraction_fitted + TotalWL_fitted_sq*Auto_fraction_fitted_sq + 
                                          PHONE + JUSTIF_count + 
                                          SAFETY_count + MESSAGE_OTHER_count + Hour + hours_worked |
                                          DATE_EVENT + RSE_ID_ANONYM,vcov = "twoway", data = df)

save(ols_model_interaction_fitted, file ="Result/OLS_Regression/ols_model_interaction_fitted")

table4 <- etable(ols_model_fitted,ols_model_quadractic_fitted, ols_model_interaction_fitted) 
write.csv(table4, "Result/OLS_Regression/table4.csv", row.names = TRUE)

# Regression table setup 
estimate_format <- "{estimate} ({std.error}){stars}"
note_content <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')

# Define models for the table
models_list <- list(
  "(1) Linear" = ols_model_fitted,
  "(2) Quadratic" = ols_model_quadractic_fitted,
  "(3) Interaction " = ols_model_interaction_fitted
)

# Generate the regression table
regression_table4 <- modelsummary(
  models_list,
  stars = TRUE,
  title = "OLS Regression model result (3 model)",
  output = "gt",
  estimate = estimate_format,
  statistic = NULL,  # Optional: Specify a statistic like "p.value" or "t.value" if needed
  coef_omit = "theta",  # Optional: Specify coefficients to omit if needed
  notes = note_content
  # Optional: Uncomment and specify additional options like gof_map or coef_map if needed
  # gof_map = gof_map,
  # coef_map = coef_map,
  # gof_omit = gof_omit
)

# Print the regression table
print(regression_table4)
gt::gtsave(regression_table4,filename = "Result/OLS_Regression/regression_table4.docx")

