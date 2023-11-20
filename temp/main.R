source("/Users/Georgia 1/Desktop/Workload&ErrorPaper/MWL_R_Project/APM_data_preprocess.R", encoding = "UTF-8")

# This R scripts explores the 1) effect of manual count on human errors.
# 2) the effect of move auto count on human errors.
# First, I used the negative binomial regression model fenegbin(), using the fixest() package. 
# The second part of analysis tested the validity of exogenous variables, including Hausman test. 


#df <-  eliminated2
#For Cripton dataset. 
#fixed_vars <- c("DATE_EVENT", "RSE_ID_ANONYM")
#For APM dataset. 

# df$RSE_ID_ANONYM <- as.factor(df$RSE_ID_ANONYM)
#df$RAILWAY_DAYS <- as.factor(df$RAILWAY_DAYS)

my_independent_vars <- c("Manual_count", "MOVE_AUTO_count","PHONE","MESSAGE_OTHER_count", 
    "Hour")#,"RSE_ID_ANONYM")#,"RAILWAY_DAYS") # nolint: line_length_linter.
my_independent_vars_2 <- c("Manual_count", "Manual_count_sq","MOVE_AUTO_count","MOVE_AUTO_count_sq","PHONE"
    ,"MESSAGE_OTHER_count", "Hour")#,"RSE_ID_ANONYM")#,"RAILWAY_DAYS") 

fixed_vars <- c("DATE_EVENT","RSE_ID_ANONYM")#, "RAILWAY_DAYS")
# fixed_vars <- c("Day","Year","Month","RSE_ID_ANONYM")




# Formula
formula_linear_fixed <- create_regression_formula(
    dependent_var = "Error_Count",
    independent_vars = my_independent_vars,
    fixed_effect_vars = fixed_vars
)
cat("Model 1 Linear formula: ", as.character(formula_linear_fixed), "\n")

formula_nonlinear <- create_regression_formula(
    dependent_var = "Error_Count",
    independent_vars = my_independent_vars_2,
    fixed_effect_vars = fixed_vars
)

cat("Model 1 Non-Linear formula: ", as.character(formula_nonlinear), "\n")

# Model 1 (Linear).
m1 <- fenegbin(formula_linear_fixed, vcov = "twoway", data = df)

# Model 2 (non-linear)
# Manual_count #Choose this model, since this represents the whole manual count
# of the controllers.
m2 <- fenegbin(formula_nonlinear, vcov = "twoway", data = df)

etable(m1, m2, fitstat = c("bic", "ll", "aic"))

# Model 3 &4 (Instrumental Variable, Linear)

# 1st stage regression on Manual count
formula_1st_Man <- create_regression_formula(
    dependent_var = "Manual_count",

    independent_vars = c( "TRAF_COMP", "TRAF_DENS","PHONE", "MESSAGE_OTHER_count", "Hour"), # nolint
    fixed_effect_vars = fixed_vars
)

lm_man <- feols(formula_1st_Man, data = df, vcov = "twoway")

# 1st stage regression on Move auto.
formula_1st_Auto <- create_regression_formula(
    dependent_var = "MOVE_AUTO_count",
    independent_vars = c("TRAF_COMP", "TRAF_DENS", "PHONE","MESSAGE_OTHER_count", "Hour"
),
    fixed_effect_vars = fixed_vars
)

lm_auto <- feols(formula_1st_Auto, data = df, vcov = "twoway")
print(etable(lm_auto, lm_man))

# Copy a new dataframe for intrumental variable models
df_hat <- df
df_hat["Manual_count"] <- predict(lm_man)
df_hat["MOVE_AUTO_count"] <- predict(lm_auto)

# Model 3 (Instrumental Variable, Linear)
m3 <- fenegbin(formula_linear_fixed, vcov = "twoway", data = df_hat)

# Model 4 (Instrumental Variable, Linear)
m4 <- fenegbin(formula_nonlinear, vcov = "twoway", data = df_hat, fixef.rm = 'perfect')

# The log-likelihood, AIC, and BIC are measures of model fit and goodness of fit
# where lower values indicate better model fit."
print(etable(m1, m2, m3, m4, fitstat = c("bic", "ll", "aic")))


# Plot graphs.
##########
nb_pred <- fenegbin(Error_Count ~ Manual_count + Manual_count_sq + 
                      MOVE_AUTO_count + MOVE_AUTO_count_sq + PHONE + 
                      MESSAGE_OTHER_count + Hour + factor(DATE_EVENT) + factor(RSE_ID_ANONYM), data = df)



# all_my_vars <- c(fixed_vars , my_independent_vars_2)
# preddf <- setNames(data.frame(matrix(0, ncol = length(all_my_vars), nrow =max(df$Manual_count))), all_my_vars)
#
#
# preddf$MOVE_AUTO_count <-  mean(df$MOVE_AUTO_count)
# preddf$MOVE_AUTO_count_sq <-  mean(df$MOVE_AUTO_count_sq)
# preddf$PHONE <-  mean(df$PHONE)
# #preddf$JUSTIF_count <- mean(df$JUSTIF_count)
# preddf$MESSAGE_OTHER_count <- mean(df$MESSAGE_OTHER_count)
# preddf$Hour <- mean(df$Hour)
#
# preddf$RSE_ID_ANONYM <- mean(df$RSE_ID_ANONYM)
# preddf$RSE_ID_ANONYM  <- as.factor(as.integer(preddf$RSE_ID_ANONYM))
# #preddf$RAILWAY_DAYS <- mean(df$RAILWAY_DAYS)
#
#
# preddf$Day <- mean(as.numeric(df$Day))
# preddf$Month <- mean(as.numeric(df$Month))
# preddf$Year <- mean(as.numeric(df$Year))

# preddf$DATE_EVENT <- mean(df$DATE_EVENT)
# preddf$DATE_EVENT <- as.factor(as.date(preddf$DATE_EVENT))

#preddf$DATE_EVENT <- mean(df$DATE_EVENT)

# Plot for manual count.
preddf <- read.csv("data_input_for_figures.csv")

Manual_count <- deparse(substitute(Manual_count)) 
preddf[[Manual_count]] <- seq(1,nrow(preddf))
head(preddf)

Manual_count_sq <- deparse(substitute(Manual_count_sq)) 
preddf[[Manual_count_sq]] <- seq(1,nrow(preddf))^2
head(preddf)

print(sapply(preddf, class))

preddf$DATE_EVENT <- sample(df$DATE_EVENT, 1)
preddf$RSE_ID_ANONYM <- sample(df$RSE_ID_ANONYM, 1)

#write.table(preddf, file = "data_input_for_figures.csv", sep = ",", row.names = FALSE)

# #m2
# preddf$Error_Count <- stats::predict(m2, newdata = preddf, type = "response")
# head(preddf)
# my_plot <- ggplot(preddf, aes(x=Manual_count, y=Error_Count)) + geom_line() + theme_minimal() + xlab("manual count") + ylab("error count")
# ggplotly(my_plot)

# #m4
# preddf$Error_Count <- stats::predict(m4, newdata = preddf, type = "response")
# head(preddf)
# my_plot <- ggplot(preddf, aes(x=Manual_count, y=Error_Count)) + geom_line() + theme_minimal() + xlab("manual count") + ylab("error count")
# ggplotly(my_plot)

#mx <- glm.nb( 
#    Error_Count Manual_count + Manual_count_sq + MOVE_AUTO_count + 
#    MOVE_AUTO_count_sq + PHONE + MESSAGE_OTHER_count +
#    Hour + Day + Year +  
#    Month + RSE_ID_ANONYM, data = df)


preddf$Error_Count_m2 <- predict(nb_pred, newdata = preddf, type = "response")
# preddf$Error_Count_m4 <- stats::predict(m4, newdata = preddf, type = "response")
# head(preddf)


my_plot <- ggplot(preddf, aes(x=Manual_count)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_Count_m2), color = "steelblue", linewidth = 2) + 
  xlab("Task workload")+
  ylab("Number of Human Errors") + 
  
  #geom_line(aes(y = Error_Count_m4), color="steelblue", linetype="twodash") + 
  scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))

plot(my_plot)

ggsave(filename = "APM_results/Manual_predict.png", width = 6, height = 4, dpi = 300, plot = my_plot)


# Plot for Move_Auto count.

preddf <- read.csv("data_input_for_figures.csv")
# 
# all_my_vars <- c(fixed_vars , my_independent_vars_2)
# preddf <- setNames(data.frame(matrix(0, ncol = length(all_my_vars), nrow =max(df$MOVE_ALL_count))), all_my_vars)
# 
# preddf$DATE_EVENT <- as.factor(preddf$DATE_EVENT)
preddf$MOVE_AUTO_count <-  mean(df$MOVE_AUTO_count)
preddf$MOVE_AUTO_count_sq <-  mean(df$MOVE_AUTO_count_sq)
# 
# preddf$PHONE <-  mean(df$PHONE)
# #preddf$JUSTIF_count <- mean(df$JUSTIF_count)
# preddf$MESSAGE_OTHER_count <- mean(df$MESSAGE_OTHER_count)
# preddf$Hour <- mean(df$Hour)
# preddf$RAILWAY_DAYS <- mean(df$RAILWAY_DAYS)
# preddf$DATE_EVENT <- mean(df$DATE_EVENT)

preddf$Manual_count <-  mean(df$Manual_count)
preddf$Manual_count_sq <-  mean(df$Manual_count_sq)

MOVE_AUTO_count <- deparse(substitute(MOVE_AUTO_count)) 
preddf[[MOVE_AUTO_count]] <- seq(1,nrow(preddf))

MOVE_AUTO_count_sq <- deparse(substitute(MOVE_AUTO_count_sq)) 
preddf[[MOVE_AUTO_count_sq]] <- seq(1,nrow(preddf))^2
head(preddf)

preddf$Error_Count_m2 <- stats::predict(m2, newdata = preddf, type = "response")


# preddf$Error_Count_m4 <- stats::predict(m4, newdata = preddf, type = "response")

# head(preddf)
# my_plot <- ggplot(preddf, aes(x=MOVE_AUTO_count, y=Error_Count)) + geom_line() + theme_minimal() + xlab("Move Auto Count") + ylab("error count")
# ggplotly(my_plot)

# preddf$Error_Count <- stats::predict(m4, newdata = preddf, type = "response")
# head(preddf)
# my_plot <- ggplot(preddf, aes(x=MOVE_AUTO_count, y=Error_Count)) + geom_line() + theme_minimal() + xlab("Move Auto Count") + ylab("error count")
# ggplotly(my_plot)

my_plot <- ggplot(preddf, aes(x=MOVE_AUTO_count)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+

  geom_line(aes(y = Error_Count_m2), color = "steelblue", linewidth = 2) + 
  xlab("Monitoring Workload") + 
  ylab("Number of Human Errors") + 

  #geom_line(aes(y = Error_Count_m4), color="steelblue", linetype="twodash") + 
  scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))

plot(my_plot)
ggsave(filename = "APM_results/Monitor_predict.png", width = 6, height = 4, dpi = 300, plot = my_plot)


##############################
# Testing for endogenity

# 1. Instrument Relevance.

# Manual regression.
instrFtest <- waldtest(lm_man, . ~ . - TRAF_COMP - TRAF_DENS)
print(instrFtest)
linearHypothesis(lm_man, c("TRAF_COMP=0", "TRAF_DENS=0"))

# Ftest P<0.05, reject the H0 that IV is irrelevant.

# Automation regression.
instrFtest <- waldtest(lm_auto, . ~ . - TRAF_COMP - TRAF_DENS)
print(instrFtest)
linearHypothesis(lm_auto, c("TRAF_COMP=0", "TRAF_DENS=0"))
# Ftest P<0.05, reject the H0 that IV is irrelevant.

# 2. Test for exogeneity. (Is endogeous)

df$lm_man_residuals <- lm_man$residuals

formula_linear_fixed_residual <- create_regression_formula(
    dependent_var = "Error_Count",
    independent_vars = c(
        "Manual_count", "MOVE_AUTO_count",
        "PHONE+JUSTIF_count",
        "MESSAGE_OTHER_count", "Hour",
        "lm_man_residuals" # add residual
    ),
    fixed_effect_vars = fixed_vars
)

hausman_reg <- fenegbin(formula_linear_fixed_residual, vcov = "twoway", data = df)
summary(hausman_reg)
HausWutest <- waldtest(hausman_reg, . ~ . - lm_man_residuals)
print(HausWutest)
# The result is a p-value of 3.043e-09.
# P<0.05, reject the H0 that IV is not exogenous.

df$lm_auto_residuals <- lm_auto$residuals

formula_linear_fixed_residual <- create_regression_formula(
    dependent_var = "Error_Count",
    independent_vars = c(
        "Manual_count", "MOVE_AUTO_count",
        "PHONE",
        "MESSAGE_OTHER_count", "Hour",
        "lm_auto_residuals" # add residual
    ),
    fixed_effect_vars = fixed_vars
)

hausman_reg <- fenegbin(formula_linear_fixed_residual, vcov = "twoway", data = df)

HausWutest <- waldtest(hausman_reg, . ~ . - lm_auto_residuals)
print(HausWutest)

# The result is a p-value of 1.01e-05.
# P<0.05, reject the H0 that IV is not exogenous.


# Write plots. 
############
cap <- 'Workload Table'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
#gof_map <- c("AIC", "loglik")
note <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')
models<- list("(1) Linear "=m1,"Quadratic "=m2 )

tab<- modelsummary(models, stars = TRUE,  output = "gt", estimate  = estimate,
                   #gof_map =gof,
                   #coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap, notes =note
                   # , gof_omit = omit
)

#library("webshot2")
gt::gtsave(tab,filename = "./Event_Based_results/Workload_table_all_032623.docx")

