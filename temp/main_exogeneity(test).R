# Model 3 &4 (Instrumental Variable, Linear)

# 1st stage regression on Manual count
formula_1st_Man <- create_regression_formula(
  dependent_var = "Manual_count",
  
  independent_vars = c( "TRAF_COMP","PHONE", "MESSAGE_OTHER_count", "Hour"), # nolint
  fixed_effect_vars = fixed_vars
)

lm_man <- feols(formula_1st_Man, data = df, vcov = "twoway")

# 1st stage regression on Move auto.
formula_1st_Auto <- create_regression_formula(
  dependent_var = "MOVE_AUTO_count",
  independent_vars = c("TRAF_COMP", "PHONE","MESSAGE_OTHER_count", "Hour"
  ),
  fixed_effect_vars = fixed_vars
)

lm_auto <- feols(formula_1st_Auto, data = df, vcov = "twoway")
print(etable(lm_auto, lm_man))

# Copy a new dataframe for intrumental variable models
df_hat <- df
df_hat["Manual_count"] <- predict(lm_man)
df_hat["MOVE_AUTO_count"] <- predict(lm_auto)
df_hat$MOVE_AUTO_count_sq <- df_hat$MOVE_AUTO_count^2 
df_hat$Manual_count_sq <-df_hat$Manual_count^2

# Model 3 (Instrumental Variable, Linear)
m3 <- fenegbin(formula_linear_fixed, vcov = "twoway", data = df_hat)

# Model 4 (Instrumental Variable, Linear)
m4 <- fenegbin(formula_nonlinear, vcov = "twoway", data = df_hat, fixef.rm = 'perfect')


df$lm_man_residuals <- lm_man$residuals

formula_linear_fixed_residual <- create_regression_formula(
  dependent_var = "Error_Count",
  independent_vars = c(
    "Manual_count", "MOVE_AUTO_count",
    "PHONE",
    "MESSAGE_OTHER_count", "Hour",
    "lm_man_residuals" # add residual
  ),
  fixed_effect_vars = fixed_vars
)

hausman_reg <- fenegbin(formula_linear_fixed_residual, vcov = "twoway", data = df)
summary(hausman_reg)
HausWutest <- waldtest(hausman_reg, . ~ . - lm_man_residuals)
print(HausWutest)

