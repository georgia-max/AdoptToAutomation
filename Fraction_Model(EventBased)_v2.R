"Date 03/21/23
This file uses Peter's Automation Algoirthm paper data CRIPTON_60_MINUTES_anonymized_all.csv, 
Model 3. Fraction Model" 


library(fixest)
library(MASS)
# source("./Plot.R")
source("./Functions.R")

library(stargazer)
library(plotly)
library(VGAM)
library(ggplot2)
library(psych) 
library(modelsummary)
library(gt)
library(dplyr)

#####
independent_vars_Manual <- c("TotalWL", "Manual_fraction","PHONE","MESSAGE_OTHER_count", "Hour")
all_vars <- c("TotalWL", "Manual_fraction",
              "PHONE","JUSTIF_count", "MESSAGE_OTHER_count",
              'Hour', 'DATE_EVENT','RSE_ID_ANONYM','Error_Count')
####### 

regress_df <- df[ ,c("TotalWL", "Auto_fraction",
                     "PHONE","JUSTIF_count", "MESSAGE_OTHER_count",
                     'Hour', 'DATE_EVENT','RSE_ID_ANONYM','Error_Count', 'Error_rate')]


#create summary table

final_stats <- as.data.frame(describe(regress_df))

write.csv(final_stats,file = "./Event_Based_results/sumstats_091923.csv" )

length(unique(df$RSE_ID_ANONYM))


#Error Rate OLS regression 
###############

df$Error_rate = df$Error_rate_thousand


m1.1= feols(Error_rate ~ TotalWL + Auto_fraction 
            +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP 
               | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
m1.2= feols(Error_rate ~ TotalWL + Auto_fraction + Auto_fraction_sq +TotalWL_sq + 
            PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP 
               | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

m1.3= feols(Error_rate ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + Auto_fraction_sq+TotalWL_sq + Auto_fraction_sq*TotalWL_sq 
            + PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP 
                 | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
etable(m1.1,m1.2, m1.3)

# cap <- 'Error Rate OLS Regression Table'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
#gof_map <- c("AIC", "loglik")
note <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')
models<- list( "(1)Linear"=m1.1,"(2)Quadratic"=m1.2, '(3)Interaction' = m1.3)


tab<- modelsummary(models, stars = TRUE, title="Error Rate OLS Regression Table", output = "gt", estimate  = estimate,
                   #gof_map =gof,
                   #coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   notes =note
                   # gof_omit = omit
)

#library("webshot2")
gt::gtsave(tab,filename = "OLS_ErrorRate_Result_090723.docx")



#Error Count
#####

#Error Count NB regression  
# 
# m1= fenegbin(Error_Count ~ TotalWL + Manual_fraction 
#                +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour + TRAF_COMP
#              | DATE_EVENT+ RSE_ID_ANONYM,vcov = "twoway", data = df
# )
# m2= fenegbin(Error_Count ~ TotalWL + Manual_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+TRAF_COMP+ 
#                Manual_fraction_sq+TotalWL_sq| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
# )
# 
# 
# 
m2.1= fenegbin(Error_Count ~ TotalWL + Auto_fraction 
               +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP
             | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
m2.2= fenegbin(Error_Count ~ TotalWL + Auto_fraction + Auto_fraction_sq+TotalWL_sq
               +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+TRAF_COMP
               | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

m2.3= fenegbin(Error_Count ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + Auto_fraction_sq+TotalWL_sq + Auto_fraction_sq*TotalWL_sq
               + PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP
              | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

etable(m2.1,m2.2,m2.3)

#############


# cm <- c( 'TotalWL' = 'Total WL', 'TotalWL_sq' = 'TotalWL^2', 'Auto_fraction' = 'Automaton Percentage',
#          'Auto_fraction_sq' = 'Auto Precentage^2',
#          'Manual_fraction' = 'Manual Percetnage',
#          'Manual_fraction_sq' = "Manual Percetnage^2")
# 

cap <- 'Error Count NB regression Model'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
#gof_map <- c("AIC", "loglik")
note <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')
models<- list( "(1) Linear"=m2.1,"(2) Quadratic"=m2.2, '(3) Interaction' = m2.3)

tab<- modelsummary(models, stars = TRUE,  output = "gt", estimate  = estimate,
                   #gof_map =gof,
                   #coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap, notes =note
                   # , gof_omit = omit
)

#library("webshot2")
gt::gtsave(tab,filename = "NB_ErrorCount_Result_091923.docx")



#normallized?
########
m_nb= glm.nb(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
               I(Auto_fraction^2)+I(TotalWL^2), data = df, start = c(0.005, 0.006, 2.1, 0.0006,-0.0001,0.004,0.0005,-5.8,-0.000005))
m_qp= glm(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
            I(Auto_fraction^2)+I(TotalWL^2), data = df, family = "quasipoisson")
predicted.y = predict(m_qp, newdata=df, type="response")

dnbinom(100, mu=predicted.y, size=m4$theta)

est <- cbind(Estimate = coef(m4), confint(m4))
fit <- vglm(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
              I(Auto_fraction^2)+I(TotalWL^2), data = df, family=negbinomial(), )
summary(fit)
predict(fit, se.fit=TRUE)


ggplot() +
  geom_qq(aes(sample = rstandard(fit))) +
  geom_abline(color = "red") +
  coord_fixed()









#Test for endogenetiy of manual count 
ob_removed<- m2$obs_selection$obsRemoved*-1

df$index <- seq(1, nrow(df))
#New dataset deleting Fixed removed. 
df2<- df[-c(ob_removed),]
########
# df2$m2.res.moveman <- m2$residuals
# 
# m3= fenegbin(Error_Count ~ TotalWL + Manual_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df2
# )
# m4= fenegbin(Error_Count ~ TotalWL + Manual_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
#                csw(I(Manual_fraction^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df2
# )
# etable(m3,m4)
# 
# Hausman_reg<- fenegbin(Error_Count ~ TotalWL + Manual_fraction  +
#                          PHONE  + csw(I(Manual_fraction^2) +m2.res.moveman)-1
#                        | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df2
# )
# summary(Hausman_reg)
# library(lmtest)
# HausWutest<- waldtest(Hausman_reg, .~.-m4.res.moveman)
# print(HausWutest)
# #H0: Y is exogeneous, H1: Y is endogenous. P = 0.02 
# #result show that we can reject the null hypothesis because the p-value is less than 0.05.
# #Hence, we can conclude that Yt is endogenous and the estimates of the 2SLS model are appropriate.
# 
# 
# ##### Hausman Move_auto 
# m4.1st = feols(Auto_fraction ~ TRAF_COMP+TRAF_DENS 
#                        + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
#                        # + AGE + EXPERIENCE_DAYS + GENDER 
#                        | DATE_EVENT+RSE_ID_ANONYM, data = df2, vcov="twoway")
# 
# df$m4.res.moveauto <- m4.1st$residuals
# 
# # linear= fenegbin(Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df2
# # )
# quadratic= fenegbin(Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
#                I(Auto_fraction^2)| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df2
# )
# 
# 
# Hausman_reg<- fenegbin(Error_Count ~ TotalWL + Auto_fraction  +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
#                         I(Auto_fraction^2) +m4.res.moveauto
#                        | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df2
# )
# summary(Hausman_reg)
# etable(quadratic, Hausman_reg)
# library(lmtest)
# HausWutest<- waldtest(Hausman_reg, .~.-m4.res.moveauto)
# print(HausWutest)
# #H0: Y is Auto_fraction, H1: Y is Auto_fraction P = 0.000
# #result show that we can reject the null hypothesis because the p-value is less than 0.05.
# #Hence, we can conclude that Yt is endogenous and the estimates of the 2SLS model are appropriate.
# 
# 

#Hausaman Test function is in Functions.R script
#########
#Testing Auto_fraction Endogenous 
#1ststage 
formula1 <- Auto_fraction ~ TRAF_COMP+TRAF_DENS + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour |DATE_EVENT+RAILWAY_DAYS+AGE
#WithoutResiduals 
formula2 <- Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)|DATE_EVENT+RAILWAY_DAYS+AGE
#With Residuals 
formula3<- Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)+residuals|DATE_EVENT+RAILWAY_DAYS+AGE


Reg_2nd<- Hasusman_Test(formula1, formula2,formula3, df2)
summary(Reg_2nd)
#Result. Auto_fraction is slightly endogenous  

#Testing Manual_fraction Endogenous 
#1ststage 
formula1 <- Manual_fraction ~ TRAF_COMP+TRAF_DENS + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour |DATE_EVENT+RAILWAY_DAYS+AGE
#WithoutResiduals 
formula2 <- Error_Count ~ TotalWL + Manual_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Manual_fraction^2)+I(TotalWL^2)|DATE_EVENT+RAILWAY_DAYS+AGE
#With Residuals 
formula3<- Error_Count ~ TotalWL + Manual_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Manual_fraction^2)+I(TotalWL^2)+residuals|DATE_EVENT+RAILWAY_DAYS+AGE


Reg_2nd<- Hasusman_Test(formula1, formula2,formula3, df2)
summary(Reg_2nd)
#Result. Manual_fraction is endogenous  

#Testing TWL Endogenous 
#1ststage 
formula1 <- TotalWL ~ TRAF_COMP+TRAF_DENS + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour |DATE_EVENT+RAILWAY_DAYS+AGE
#WithoutResiduals 
formula2 <- Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)|DATE_EVENT+RAILWAY_DAYS+AGE
#With Residuals 
formula3<- Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)+residuals|DATE_EVENT+RAILWAY_DAYS+AGE

Reg_2nd<- Hasusman_Test(formula1, formula2,formula3, df2)
summary(Reg_2nd)
#Result. TWL is exogeous 


# Plot graph for Auto_fraction.
# nb_manual_pred <- fenegbin(Error_Count ~ TotalWL + Manual_fraction +PHONE+ 
#                            JUSTIF_count + MESSAGE_OTHER_count + Hour+
#                            Manual_fraction_sq+TotalWL_sq  + factor(DATE_EVENT) + 
#                            factor(RSE_ID_ANONYM), data = df)


nb_auto_pred <- fenegbin(Error_Count ~ TotalWL + Auto_fraction +PHONE+ 
                             JUSTIF_count + MESSAGE_OTHER_count + Hour+
                             Auto_fraction_sq+TotalWL_sq  + factor(DATE_EVENT) + 
                             factor(RSE_ID_ANONYM), data = df)

# nb_manual_inter_pred <- fenegbin(Error_Count ~ TotalWL + Manual_fraction + TotalWL*Manual_fraction + 
#                                    PHONE+ 
#                              JUSTIF_count + MESSAGE_OTHER_count + Hour+
#                              Manual_fraction_sq+TotalWL_sq  + factor(DATE_EVENT) + 
#                              factor(RSE_ID_ANONYM), data = df)

nb_auto_inter_pred <- fenegbin(Error_Count ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + 
                                 PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour +
                                 Auto_fraction_sq+TotalWL_sq + Auto_fraction_sq*TotalWL_sq + TRAF_COMP + 
                                 factor(DATE_EVENT) + factor(RSE_ID_ANONYM), data = df)


nb_auto_rate_pred<- fepois(Error_rate ~ TotalWL + Auto_fraction +PHONE+ 
                           JUSTIF_count + MESSAGE_OTHER_count + Hour+
                           Auto_fraction_sq+TotalWL_sq  + factor(DATE_EVENT) + 
                           factor(RSE_ID_ANONYM), data = df)


nb_auto_inter_rate_pred <- fepois(Error_rate ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + 
                                 PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour +
                                 Auto_fraction_sq+TotalWL_sq + Auto_fraction_sq*TotalWL_sq + TRAF_COMP + 
                                 factor(DATE_EVENT) + factor(RSE_ID_ANONYM), data = df)



all_my_vars <- c("TotalWL", "TotalWL_sq", "Manual_fraction","Auto_fraction","JUSTIF_count",
                 "PHONE","MESSAGE_OTHER_count", "Hour", "TRAF_COMP",
                 "Manual_fraction_sq", "Auto_fraction_sq", "DATE_EVENT","RSE_ID_ANONYM")
preddf <- setNames(data.frame(matrix(0, ncol = length(all_my_vars), nrow =max(df$TotalWL))), all_my_vars)

preddf$PHONE <-  mean(df$PHONE)
preddf$JUSTIF_count <- mean(df$JUSTIF_count)
preddf$MESSAGE_OTHER_count <- mean(df$MESSAGE_OTHER_count)
preddf$Hour <- mean(df$Hour)
preddf$TotalWL  <- mean(df$TotalWL)
preddf$TotalWL_sq  <- mean(df$TotalWL_sq)
preddf$TRAF_COMP<- mean(df$TRAF_COMP)

preddf$DATE_EVENT <- sample(df$DATE_EVENT, 1)
preddf$RSE_ID_ANONYM <- sample(df$RSE_ID_ANONYM, 1)


# For TotalWL
TotalWL <- deparse(substitute(TotalWL)) 
preddf$TotalWL<- seq(1,nrow(preddf)) 

preddf$TotalWL_sq<- preddf$TotalWL^2 
preddf$Auto_fraction <- mean(df$Auto_fraction)
preddf$Auto_fraction_sq <-preddf$Auto_fraction^2

head(preddf)



preddf$Auto_fraction_sq<- preddf$Auto_fraction^2
head(preddf)



# For manual fraction 
# Manual_fraction <- deparse(substitute(Manual_fraction)) 
# preddf[[Manual_fraction]] <- seq(1,nrow(preddf)) *0.01
# head(preddf)
# 
# preddf$Manual_fraction_sq <- preddf$Manual_fraction^2
# head(preddf)
# 
# print(sapply(preddf, class))


preddf$Error_Count_auto_predict <- predict(nb_auto_pred, newdata = preddf, type = "response")
#preddf$Error_Count_manual_predict <- predict(nb_manual_pred, newdata = preddf, type = "response")
#preddf$Error_Count_manual_inter_predict <- predict(nb_manual_inter_pred, newdata = preddf, type = "response")
preddf$Error_Count_auto_inter_predict <- predict(nb_auto_inter_pred, newdata = preddf, type = "response")

preddf$Error_rate_auto_inter_predict <- predict(nb_auto_inter_rate_pred, newdata = preddf, type = "response")

# head(preddf)

######### plot for totalWL

my_plot <- ggplot(preddf, aes(x=TotalWL)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  geom_line(aes(y = Error_rate_auto_inter_predict), linewidth = 1.5) + 
  xlab("Signaling Workload")+
  ylab("Human Error Rate") 
  

ggplotly(my_plot)

ggsave(filename = "./Event_Based_results/TotalWL_errorrate.png", width = 6, height = 4, dpi = 300, plot = my_plot)


my_plot <- ggplot(preddf, aes(x=TotalWL)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  geom_line(aes(y = Error_Count_auto_inter_predict), linewidth = 2) + 
  xlab("Total WL")+
  ylab("Human Error ") 


ggplotly(my_plot)

ggsave(filename = "./Event_Based_results/TotalWL_error_count.png", width = 6, height = 4, dpi = 300, plot = my_plot)


# Plot for manual 
##########
my_plot <- ggplot(preddf, aes(x=Manual_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_Count_manual_inter_predict), color = "steelblue", linewidth = 2) + 
  xlab("Manual Usage")+
  ylab("Number of Human Errors") + 
  
  #geom_line(aes(y = Error_Count_m4), color="steelblue", linetype="twodash") + 
  scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))

plot(my_plot)

ggsave(filename = "./Event_Based_results/Manual_Usage.png", width = 6, height = 4, dpi = 300, plot = my_plot)


# Plot for auto usage  
my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_Count_auto_predict), color = "steelblue", linewidth = 2) + 
  xlab("Automation Usage")+
  ylab("Number of Human Errors") + 
  
  #geom_line(aes(y = Error_Count_m4), color="steelblue", linetype="twodash") + 
  scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))

plot(my_plot)

ggsave(filename = "./Event_Based_results/Auto_Usage.png", width = 6, height = 4, dpi = 300, plot = my_plot)

#Levels of WL

## Interaction effect divided manual manual fraction.  
## This idea does not work, bc TWL^2 coefficient is too 
######
preddf_highMU <- preddf
preddf_highMU$Manual_fraction = 0.65
preddf_highMU$Manual_fraction_sq <- preddf_highMU$Manual_fraction^2

preddf_lowMU <- preddf
preddf_lowMU$Manual_fraction = 0.47
preddf_lowMU$Manual_fraction_sq <- preddf_lowMU$Manual_fraction^2

preddf_medMU <- preddf
preddf_medMU$Manual_fraction = 0.3
preddf_medMU$Manual_fraction_sq <- preddf_medMU$Manual_fraction^2


# Plot for interaction between manual and total workload
preddf$Error_predict_man_highMU <- predict(nb_manual_inter_pred, newdata = preddf_highMU, type = "response")
preddf$Error_predict_man_lowMU <- predict(nb_manual_inter_pred, newdata = preddf_lowMU, type = "response")
preddf$Error_predict_man_medMU <- predict(nb_manual_inter_pred, newdata = preddf_medMU, type = "response")

my_plot <- ggplot(preddf, aes(x=Manual_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_predict_man_lowMU, colour = "Low Manual Use"), linewidth = 2) + 
  geom_line(aes(y = Error_predict_man_highMU, colour = "High Manual Use"), linewidth = 2) + 
  
  xlab("TotalWL")+
  ylab("Number of Human Errors") + 
  
  # scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))
  scale_colour_manual("", 
                      breaks = c("Low Manual Use", "High Manual Use"),
                      values = c("darkred", "steelblue")) 

plot(my_plot)

ggsave(filename = "./Event_Based_results/TWL_interaction_2levelMU.png", width = 6, height = 4, dpi = 300, plot = my_plot)






##########


# For auto fraction 
Auto_fraction <- deparse(substitute(Auto_fraction)) 
preddf$Auto_fraction<- seq(1,nrow(preddf)) *0.01
head(preddf)

preddf$Auto_fraction_sq<- preddf$Auto_fraction^2
preddf$TotalWL  <- mean(df$TotalWL)
preddf$TotalWL_sq  <- mean(df$TotalWL_sq)

head(preddf)

#High Total Workload 
preddf_highWL <- preddf
preddf_highWL$TotalWL <- 90 #3rdQ
preddf_highWL$TotalWL_sq <- preddf_highWL$TotalWL^2

#Low Total Workload 
preddf_lowWL <- preddf
preddf_lowWL$TotalWL <- 39 #1stQu
preddf_lowWL$TotalWL_sq <- preddf_lowWL$TotalWL^2

#Medium Total Workload 
preddf_medWL <- preddf
preddf_medWL$TotalWL <- 64 #1stQu
preddf_medWL$TotalWL_sq <- preddf_medWL$TotalWL^2


# Plot for interaction between manual and total workload
########
# preddf$Error_predict_man_highWL <- predict(nb_manual_inter_pred, newdata = preddf_highWL, type = "response")
# preddf$Error_predict_man_lowWL <- predict(nb_manual_inter_pred, newdata = preddf_lowWL, type = "response")
# preddf$Error_predict_man_medWL <- predict(nb_manual_inter_pred, newdata = preddf_medWL, type = "response")
# 
# my_plot <- ggplot(preddf, aes(x=Manual_fraction)) + 
#   theme_light()+theme(axis.text.x = element_text(size = 10),
#                       axis.text.y = element_text(size = 10))+
#   
#   geom_line(aes(y = Error_predict_man_lowWL, colour = "Low WL"), linewidth = 2) + 
#   geom_line(aes(y = Error_predict_man_highWL, colour = "High WL"), linewidth = 2) + 
#   
#   xlab("Auto Usage")+
#   ylab("Number of Human Errors") + 
#   
#   # scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))
#   scale_colour_manual("", 
#                     breaks = c("High WL", "Low WL"),
#                     values = c("darkred", "steelblue")) 
# 
# plot(my_plot)
# 
# ggsave(filename = "./Event_Based_results/Manual_interaction.png", width = 6, height = 4, dpi = 300, plot = my_plot)
# 




# Plot one level WL

preddf$Error_predict_autoWL <- predict(nb_auto_inter_pred, newdata = preddf, type = "response")

my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_predict_autoWL), linewidth = 2) + 

  xlab("Automation Usage")+
  ylab("Number of Human Errors") 
plot(my_plot)

ggsave(filename = "./Event_Based_results/Auto_interaction.png", width = 6, height = 4, dpi = 300, plot = my_plot)


# Plot different level WL
########
preddf$Error_predict_auto_highWL <- predict(nb_auto_inter_pred, newdata = preddf_highWL, type = "response")
preddf$Error_predict_auto_lowWL <- predict(nb_auto_inter_pred, newdata = preddf_lowWL, type = "response")
preddf$Error_predict_auto_medWL <- predict(nb_auto_inter_pred, newdata = preddf_medWL, type = "response")

my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_predict_auto_lowWL, colour = "Low WL"), linewidth = 2) + 
  geom_line(aes(y = Error_predict_auto_highWL, colour = "High WL"), linewidth = 2) + 
  geom_line(aes(y = Error_predict_auto_medWL, colour = "Medium WL"), linewidth = 2) + 
  
  xlab("Automation Usage")+
  ylab("Number of Human Errors") + 
  
  # scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))
  scale_colour_manual("", 
                      breaks = c("High WL", "Low WL","Medium WL" ),
                      values = c("darkred", "steelblue", 'green')) 

plot(my_plot)

ggsave(filename = "./Event_Based_results/Auto_interaction_twolvl.png", width = 6, height = 4, dpi = 300, plot = my_plot)

# Plot for Error Rate 
#########

preddf$Error_Count_autorate_predict <- predict(nb_auto_rate_pred, newdata = preddf, type = "response")
preddf$Error_Count_autotate_inter_predict <- predict(nb_auto_inter_rate_pred, newdata = preddf, type = "response")

my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_Count_autotate_inter_predict), linewidth = 2) + 
  
  xlab("Automation Usage")+
  ylab("Human Error Rate") 
plot(my_plot)

ggsave(filename = "./Event_Based_results/Auto_ErrorRate.png", width = 6, height = 4, dpi = 300, plot = my_plot)


# Plot different level WL
########
preddf$ErrorRate_predict_auto_highWL <- predict(nb_auto_inter_rate_pred, newdata = preddf_highWL, type = "response")
preddf$ErrorRate_predict_auto_lowWL <- predict(nb_auto_inter_rate_pred, newdata = preddf_lowWL, type = "response")
preddf$ErrorRate_predict_auto_medWL <- predict(nb_auto_inter_rate_pred, newdata = preddf_medWL, type = "response")

my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = ErrorRate_predict_auto_lowWL, colour = "Low WL"), linewidth = 2) + 
  geom_line(aes(y = ErrorRate_predict_auto_highWL, colour = "High WL"), linewidth = 2) + 
  geom_line(aes(y = ErrorRate_predict_auto_medWL, colour = "Medium WL"), linewidth = 2) + 
  
  xlab("Automation Usage")+
  ylab("Human Error Rate") + 
  
  # scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))
  scale_colour_manual("", 
                      breaks = c("High WL", "Low WL","Medium WL" ),
                      values = c("darkred", "steelblue", 'green')) 

plot(my_plot)

ggsave(filename = "./Event_Based_results/Auto_interactionErrorRate_twolvl.png", width = 6, height = 4, dpi = 300, plot = my_plot)


######

predict.data <- setNames(data.frame(matrix(0, ncol = 11, nrow =2000)), c('TotalWL','Auto_fraction', 'JUSTIF_count','PHONE' , 'TRAF_COMP'
                                                                         ,'MESSAGE_OTHER_count','DATE_EVENT','RSE_ID_ANONYM','Hour'))
#predict.data$DATE_EVENT <- as.character(predict.data$DATE_EVENT)
#predict.data$RSE_ID_ANONYM <- as.character(predict.data$RSE_ID_ANONYM)
predict.data<- predict_X(TotalWL, m_qp, predict.data )
dev.off()
#source("Plot.R", print.eval=TRUE)
#png(filename="MVE_MAN.png")

plt<- plotCI(predict.data, m_qp, TotalWL,"TotalWorkload")
ggsave("./APM_results/TotalWL.png", width = 6, height = 4, dpi = 300, plot = plt)

summary(m2_nb)

#Joint Hypothesis to test if Auto_fraction is worth putting here 
#######

restricted= fenegbin(Error_Count ~ TotalWL  +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour
                     +I(TotalWL^2)| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

anova(m4, restricted)
HausWutest<- waldtest(m4, .~.-Auto_fraction.-I(Auto_fraction^2))
print(HausWutest)
#Not sure if this is done properly, but the idea is to test that joint hypthesis to see if Auto_fraction =0 is rejected. 
#If so, we do not incluede this vars in the model. 
#https://www.econometrics-with-r.org/7-3-joint-hypothesis-testing-using-the-f-statistic.html
#https://www.statalist.org/forums/forum/general-stata-discussion/general/1408413-significance-level-of-quadratic-term


# Print to word document, for paper. 

