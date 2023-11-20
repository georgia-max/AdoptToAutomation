library(modelsummary)
library(fixest)
######## 
#Adding Move_Man, SWAut0, SWMan to Manual Count 
#df$Manual_count <- df$MOVE_MAN_count+df$CHANGE_AUTO_AA_count+ df$CHANGE_AUTO_WW_count+df$CHANGE_AUTO_REMOVE_TIMER_count

# m1= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count+
#                PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour
#              | DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway"
#              ,data = df)

m2= fenegbin(Error_Count ~Manual_count +MOVE_AUTO_count+
               PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour
             | DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway"
             ,data = df)
# m3= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
#                PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour  + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
# )

m3= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
               csw0(I(Manual_count^2) + I(MOVE_AUTO_count^2))-1| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df
)
# m3.1= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
#                sw0(I(Manual_count^2) , I(MOVE_AUTO_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
# )
# etable(m3.1)
etable(m3)
m4= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
                 csw0(I(Manual_count^2) , I(MOVE_AUTO_count^2))-1| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df
)

etable(m1,m2,m3,m4)
etable(m2,m4)
#Removed fixed effect variables that cannot acount in the regression model 

fixed_removed <- m4$fixef_removed
fixed_removed$RSE_ID_ANONYM
fixed_removed$DATE_EVENT

ob_removed<- m4$obs_selection$obsRemoved*-1

df$index <- seq(1, nrow(df))
#New dataset deleting Fixed removed. 
df2<- df[-c(ob_removed),]

#Test for endogenetiy of manual count 

df2$m4.res.moveman <- m4$residuals
Hausman_reg<- fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +
                         PHONE  + csw(I(Manual_count^2) + I(MOVE_AUTO_count^2)+m4.res.moveman)-1
                   | DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df2
)
summary(Hausman_reg)
library(lmtest)
HausWutest<- waldtest(Hausman_reg, .~.-m4.res.moveman)
print(HausWutest)



#The results are significant (p<0.01), 
#fail to reject the null hypothesis that Manual_count is exogenous. Hence two stage approach 

#https://www.youtube.com/watch?v=n_F-01iTquA 
tsls= ivreg(Error_Count ~ Manual_count + MOVE_AUTO_count +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
               I(Manual_count^2) + I(MOVE_AUTO_count^2)| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df2
)
#Hausman and Sargan Test
summary(object =tsls, diagnostics=TRUE)


#model Summary
############
library(modelsummary)
library(gt)
library(dplyr)

cm <- c( 'Manual_count' = 'Manual', 'MOVE_AUTO_count' = 'Monitoring',
          'CHANGE_AUTO_AA_count' = 'SwAuto','CHANGE_AUTO_WW_count' = 'SwMan',
          'I(Manual_count^2)' = 'Manual_Sq',
          'I(MOVE_AUTO_count^2)' = 'Monitoring_Sq'
        #,'MOVE_MAN_resi' ='Manual Residual','MOVE_AUTO_resi' ='Monitoring Residual',
         #'I(MOVE_MAN_resi^2)' = 'MOVE_MAN_resi_Sq','I(MOVE_AUTO_resi^2)' = 'MOVE_AUTO_resi_Sq'
         )

cap <- 'Other Models'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
#models<- list("(1) Linear"=m2,"(2)Non-linear"=m4)
#gof_map <- c("AIC", "loglik")
note <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')
tab<- modelsummary(m3, stars = TRUE,  output = "gt", estimate  = estimate,#gof_map =gof,
                   #coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap, notes =note
                   # , gof_omit = omit
)

#library("webshot2")
gt::gtsave(tab,filename = "Auto_Manual_table.docx")


########
#Instrunment variables (Move Man, Traffic Dense and Traff Comp) stage 1
df_hat <-subset(df)

#Choosing using RSE_ID as fixed effect because the Adjusted R2 is 0.38>0.378
#Add to Robustness check 

m1.lm.moveman = feols(Manual_count ~ TRAF_COMP+TRAF_DENS 
                      + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                      | DATE_EVENT+RAILWAY_DAYS+AGE, data = df, vcov="twoway")

summary(m1.lm.moveman)

MOVE_MAN_count_hat <- predict(m1.lm.moveman)
df_hat['MOVE_MAN_count'] <- MOVE_MAN_count_hat
#m1.lm.moveman_resi <- resid(m1.lm.moveman)
df_hat['MOVE_MAN_resi']<- resid(m1.lm.moveman)
df$MOVE_MAN_resi <- resid(m1.lm.moveman)
df$MOVE_MAN_hat <- MOVE_MAN_count_hat

m1.lm.moveauto = feols(MOVE_AUTO_count ~ TRAF_COMP+TRAF_DENS 
                       + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                       | DATE_EVENT+RAILWAY_DAYS+AGE, data = df, vcov="twoway")
MOVE_AUTO_count_hat <- predict(m1.lm.moveauto)
summary(m1.lm.moveauto)

df_hat['MOVE_AUTO_count'] <- predict(m1.lm.moveauto)

#m1.lm.moveauto_resi <- resid(m1.lm.moveauto)
df_hat['MOVE_AUTO_resi'] <- resid(m1.lm.moveauto)
df$MOVE_AUTO_resi <- resid(m1.lm.moveauto)
df$MOVE_AUTO_hat <- MOVE_AUTO_count_hat

etable(m1.lm.moveman,m1.lm.moveauto)

######## 
#The final model 

#Linear, no IV
m1= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count #Exo
             + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour #Control Var
             | DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway"
             ,data = df)
#linear, IV
m1.ins= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count 
                   + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour #Control Var
                 +MOVE_MAN_resi+MOVE_AUTO_resi
                 | DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway"
                 ,data = df_hat)
#non-linear, no IV
m2= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count 
             + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
             + csw(I(Manual_count^2) + I(MOVE_AUTO_count^2))-1| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df
)
#non-linear,  IV

#How to deal with the residual?
m2.ins= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count 
                 + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                 
                 + csw(I(Manual_count^2) + I(MOVE_AUTO_count^2))
                 +MOVE_MAN_resi+MOVE_AUTO_resi+I(MOVE_MAN_resi^2)+I(MOVE_AUTO_resi^2) #Residual
                 -1| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df_hat
)

etable(m1, m2, m1.ins, m2.ins)
AIC(m1, m2, m1.ins, m2.ins)

library(modelsummary)
library(gt)
library(dplyr)

# cm <- c( 'MOVE_MAN_count' = 'Manual', 'MOVE_AUTO_count' = 'Monitoring',
#          'CHANGE_AUTO_AA_count' = 'SwAuto','CHANGE_AUTO_WW_count' = 'SwMan',
#          'I(MOVE_MAN_count^2)' = 'Manual_Sq',
#          'I(MOVE_AUTO_count^2)' = 'Monitoring_Sq','I(CHANGE_AUTO_AA_count^2)'='SwAuto_Sq',
#          'I(CHANGE_AUTO_WW_count^2)'='SwMan_Sq','MOVE_MAN_resi' ='Manual Residual','MOVE_AUTO_resi' ='Monitoring Residual',
#          'I(MOVE_MAN_resi^2)' = 'MOVE_MAN_resi_Sq','I(MOVE_AUTO_resi^2)' = 'MOVE_AUTO_resi_Sq')

cap <- 'Main Model Results -NB regreession'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
models<- list("(1) Linear"=m1,"(2)Non-Linear"=m2,"(3)Linear"=m1.ins,"(4)Non-Linear"= m2.ins)
#gof_map <- c("AIC", "loglik")

tab<- modelsummary(models, stars = TRUE,  output = "gt", estimate  = estimate,#gof_map =gof,
                   #coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap
                   # , gof_omit = omit
)%>%
  # column labels
  tab_spanner(label = 'No IV', columns = 2:3) %>%
  tab_spanner(label = 'With IV', columns = 4:5) 

#library("webshot2")
gt::gtsave(tab,filename = "Manual_Auto_Table5.docx")

#Stepwise regression 
###########
res_multi1= fenegbin(Error_Count ~Manual_count +MOVE_AUTO_count 
                    +csw0(MESSAGE_OTHER_count,
                JUSTIF_count,PHONE,,Hour)
             | DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway"
             ,data = df)

etable(res_multi1)#, cluster = ~DATE_EVENT+RSE_ID_ANONYM)

res_multi2= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +csw0(PHONE, JUSTIF_count ,MESSAGE_OTHER_count ,Hour,
               I(Manual_count^2) , I(MOVE_AUTO_count^2))| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df)
 
etable(res_multi2)#, cluster = ~DATE_EVENT+RSE_ID_ANONYM)

res_multi3= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +PHONE+ JUSTIF_count+MESSAGE_OTHER_count +Hour+
                       sw(I(Manual_count^2) , I(MOVE_AUTO_count^2))| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df)

etable(res_multi3, tex = TRUE)#, cluster = ~DATE_EVENT+RSE_ID_ANONYM)

cm <- c( 'Manual_count' = 'Manual', 'MOVE_AUTO_count' = 'Monitoring',
       
         'MESSAGE_OTHER_count' = 'MESSAGE_OTHER_count',
         'JUSTIF_count' = 'Justification','PHONE'='Phone',
         'Hour' ="Hour of the day")
cap <- 'Model Results -NB regreession'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
#models<- list("(1) Linear"=m1,"(2)Non-Linear"=m2,"(3)Linear"=m1.ins,"(4)Non-Linear"= m2.ins)
#gof_map <- c("AIC", "loglik")

tab<- modelsummary(res_multi3, stars = TRUE,  output = "gt", estimate  = estimate,#gof_map =gof,
                   #coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap
                   # , gof_omit = omit
)

#library("webshot2")
gt::gtsave(tab,filename = "Table9.docx")


#Correlation plot 
########
df2 <- subset(df, select = c('Error_Count','Manual_count',
                             'MOVE_AUTO_count', 'ADAPTATIONS_count','TRAF_COMP','TRAF_DENS','PHONE', 'AGE','EXPERIENCE_DAYS'))
recorr <- round(cor(df2),3)

write.xlsx(corr, "Addition_manual_corr.xlsx")
corr
