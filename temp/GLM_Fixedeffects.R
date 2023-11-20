library(fixest)
library(modelsummary)

#Import Leon's dataset 
df_event <- read.csv("~/Desktop/Workload&ErrorPaper/Event Data_Leon_20230116/training_set_02_2022_group_level.csv")


df$time <- paste(df$DATE_EVENT, df$Hour,df$Minute, sep="_")
df$individual <- paste(df$WS_NO, df$RSE_ID_ANONYM, sep="_")
print(form4)

###### Descriptive Stats 

df2 <- subset(df, select = c('Error_Count','MOVE_MAN_count','MOVE_AUTO_count','CHANGE_AUTO_AA_count','CHANGE_AUTO_WW_count'
                             , 'TRAF_COMP','TRAF_DENS','PHONE', 'JUSTIF_count','AGE','EXPERIENCE_DAYS'))
#stargazer(df2,type='html')

#df2.sum <- as.data.frame(summary(df2))

library(pastecs)
stat.desc(df2)

#datasum <- datasummary_skim(data = df2, histogram=FALSE, output = "summary.docx")
gt::gtsave(datasum,filename = "datatableall.png")
library(fixest)


######### 
#For Draft 1 
#Negbin final model 123
# m0= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
#                 TRAF_COMP
#              ,data = df)
# 
# m1= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
#                  TRAF_COMP| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
#               ,data = df)
# #etable(m6)
# 
# 
# 
# m2= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
#                 TRAF_COMP  + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_AA_count^2)
#                                                        +I(CHANGE_AUTO_WW_count^2)),data = df)
# 
# m3= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
#                 csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_AA_count^2)
#                                                         +I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
#            )
# 
# # Traffic.low= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
# #                PHONE+ TRAF_COMP  + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_AA_count^2)
# #                                        +I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = subset(df,TRAF_COMP<500)
# # )
# # Traffic.high= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
# #                     PHONE+ TRAF_COMP  + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_AA_count^2)
# #                                             +I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = subset(df,TRAF_COMP>500)
# # )
# 
# etable(m3,Traffic.low,Traffic.high)
# # m4= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
# #                              PHONE+ TRAF_COMP + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_AA_count^2)
# #                                                                      +I(CHANGE_AUTO_WW_count^2)+ PHONE +AGE+GENDER+RAILWAY_DAYS)-1| DATE_EVENT,data = df
# # )
# 
# etable(m0,m2,m1,m3)

# #Generate model summary table 
# library(gt)
# #modelsummary(models, stars = TRUE, output = "table.html", title='Negative Binominal Regression Results')
# 
# cm <- c( '(Intercept)' = 'Constant', 'MOVE_MAN_count' = 'Manual workload', 'MOVE_AUTO_count' = 'Monitoring workload',
#          'CHANGE_AUTO_AA_count' = 'Switch to Auto','CHANGE_AUTO_WW_count' = 'Switch to Manual',
#          'PHONE' = 'Phone','TRAF_COMP'= 'Traffic Complexity','I(MOVE_MAN_count^2)' = 'Manual workload^2',
#          'I(MOVE_AUTO_count^2)' = 'Monitoring workload^2','I(CHANGE_AUTO_AA_count^2)'='Switch to Auto^2',
#          'I(CHANGE_AUTO_WW_count^2)'='Switch to Manual^2','.theta'='theta'
# )
# cap <- 'Negative Binominal Regression Results'
# omit<- 'RMSE|BIC'
# estimate <- "{estimate} ({std.error}){stars}"
# models<- list("Linear"=m0,"Non-Linear"=m2,"Linear"=m1,"Non-Linear"= m3)
# #gof_map <- c("AIC", "loglik")
# 
# tab<- modelsummary(models, stars = TRUE,  output = "gt",coef_map = cm, estimate  = estimate,
#                    statistic = NULL, coef_omit = "theta",
#                    title=cap, gof_omit = omit)%>%
#   # column labels
#   tab_spanner(label = 'Without-fixed effects', columns = 2:3) %>%
#   tab_spanner(label = 'With-fixed effectss', columns = 4:5) 
# library("webshot2")
# gt::gtsave(tab,filename = "table1.png")
# 

########
#Instrunment variables (Move Man, Traffic Dense and Traff Comp) stage 1
df_hat <-subset(df)

m2.lm.moveman = feols(MOVE_MAN_count ~ TRAF_COMP+TRAF_DENS 
                      +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
                     + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                     + AGE + EXPERIENCE_DAYS + GENDER | DATE_EVENT, data = df, vcov="twoway")

#Choosing using RSE_ID as fixed effect because the Adjusted R2 is 0.38>0.378
#Add to Robustness check 

m1.lm.moveman = feols(MOVE_MAN_count ~ TRAF_COMP+TRAF_DENS 
                      +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
                      + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                      | DATE_EVENT+RSE_ID_ANONYM, data = df, vcov="twoway")

m3.lm.moveman = feols(MOVE_MAN_count ~ TRAF_COMP+TRAF_DENS 
                      # +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
                      + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                      | DATE_EVENT+RSE_ID_ANONYM, data = df, vcov="twoway")
etable(m1.lm.moveman,m2.lm.moveman,m3.lm.moveman)

summary(m1.lm.moveman)

MOVE_MAN_count_hat <- predict(m1.lm.moveman)
df_hat['MOVE_MAN_count'] <- MOVE_MAN_count_hat
#m1.lm.moveman_resi <- resid(m1.lm.moveman)
df_hat['MOVE_MAN_resi']<- resid(m1.lm.moveman)
df$MOVE_MAN_resi <- resid(m1.lm.moveman)
df$MOVE_MAN_hat <- MOVE_MAN_count_hat

m1.lm.moveauto = feols(MOVE_AUTO_count ~ TRAF_COMP+TRAF_DENS 
                       +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
                       + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                       | DATE_EVENT+RSE_ID_ANONYM, data = df, vcov="twoway")
MOVE_AUTO_count_hat <- predict(m1.lm.moveauto)
summary(m1.lm.moveauto)

df_hat['MOVE_AUTO_count'] <- predict(m1.lm.moveauto)

#m1.lm.moveauto_resi <- resid(m1.lm.moveauto)
df_hat['MOVE_AUTO_resi'] <- resid(m1.lm.moveauto)
df$MOVE_AUTO_resi <- resid(m1.lm.moveauto)
df$MOVE_AUTO_hat <- MOVE_AUTO_count_hat

##### The final model 

m1= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count+ CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count #Exo
             + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour #Control Var
             | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
             ,data = df)
# m1.ins= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+ 
#                    + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour #Control Var
#                  +I(MOVE_MAN_resi+MOVE_AUTO_resi)
#                  | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
#                  ,data = df_hat)

m1.ins= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+ 
                   + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour #Control Var
                 +MOVE_MAN_resi+MOVE_AUTO_resi
                 | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
                 ,data = df_hat)

m2= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count 
             + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
             + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

#How to deal with the residual?
m2.ins= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count 
                 + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                 
                 + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))
                 +MOVE_MAN_resi+MOVE_AUTO_resi+I(MOVE_MAN_resi^2)+I(MOVE_AUTO_resi^2) #Residual
                 -1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df_hat
)

etable(m1, m2, m1.ins, m2.ins)
AIC(m1, m2, m1.ins, m2.ins)

(est <- cbind(m1 = coef(m1),m2 = coef(m2),m1.ins = coef(m1.ins),m2.ins = coef(m2.ins) ))
exp(est)
#The percent change in the incident rate of error  is a 6% increase for every unit increase in MoveMan
cbind(confint(m1),confint(m2))
# (CI <- cbind(m1.low m1.high = confint(m1),m1.low, m1.high = confint(m2),m1.low, m1.high = confint(m1.ins),m1.low, m1.high = confint(m2.ins) ))
# exp(CI)
exp(cbind(est = coef(m1), confint(m1)))
coef(m2)
confint(m2)
exp(confint(m2))
summary(m1)

library(modelsummary)
library(gt)
library(dplyr)
    
cm <- c( 'MOVE_MAN_count' = 'Manual', 'MOVE_AUTO_count' = 'Monitoring',
         'CHANGE_AUTO_AA_count' = 'SwAuto','CHANGE_AUTO_WW_count' = 'SwMan',
        'I(MOVE_MAN_count^2)' = 'Manual_Sq',
         'I(MOVE_AUTO_count^2)' = 'Monitoring_Sq','I(CHANGE_AUTO_AA_count^2)'='SwAuto_Sq',
         'I(CHANGE_AUTO_WW_count^2)'='SwMan_Sq','MOVE_MAN_resi' ='Manual Residual','MOVE_AUTO_resi' ='Monitoring Residual',
         'I(MOVE_MAN_resi^2)' = 'MOVE_MAN_resi_Sq','I(MOVE_AUTO_resi^2)' = 'MOVE_AUTO_resi_Sq')

cap <- 'Main Model Results -NB regreession'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
models<- list("(1) Linear"=m1,"(2)Non-Linear"=m2,"(3)Linear"=m1.ins,"(4)Non-Linear"= m2.ins)
#gof_map <- c("AIC", "loglik")

tab<- modelsummary(models, stars = TRUE,  output = "gt", estimate  = estimate,#gof_map =gof,
                   coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap
                  # , gof_omit = omit
                   )%>%
  # column labels
  tab_spanner(label = 'No IV', columns = 2:3) %>%
  tab_spanner(label = 'With IV', columns = 4:5) 

#library("webshot2")
gt::gtsave(tab,filename = "table4.docx")

#Predicted
#########
library(fixest)
m2= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count 
             + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
             + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
library("MASS")
m2_nb<- glm.nb(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count 
                + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
                + I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2)-1 , data = df
)
dfnew<- df
temp<- predict(m2_nb, interval="confidence", newdata = newdata, se.fit = TRUE)
temp
alpha <- 0.95  ## 90%
Qt <- c(-1, 1) * qt((1 - alpha) / 2, 35, lower.tail = FALSE)
#[1] -1.681071  1.681071

## 90% confidence interval
CI <- temp$fit + outer(temp$se.fit, Qt)
colnames(CI) <- c("lwr", "upr")
CI
# 
# CI95min <- temp[,2]
# CI95max <- temp[,3]


dfnew <- cbind(m2_nb, "resp" = predict(m2_nb, type = "response", se.fit = TRUE)[1:2])

plot(Error_Count ~ MOVE_AUTO_count, data = df)
lines(resp.fit ~ MOVE_AUTO_count, data = df, col = 2)
lines(resp.fit + 2 * resp.se.fit ~ MOVE_AUTO_count, data = df, col = 2)
lines(resp.fit - 2 * resp.se.fit ~ MOVE_AUTO_count, data = df, col = 2)

#predict.m2.nb <- cbind(m2.nb, "link" = predict(m, type = "link", se.fit = TRUE)[1:2])

m2.predict<-predict(m2, newdata = newdata)
CI95.min<- m2.predict[,2]
CI95.max<-m2.predict[,3]

probs <- data.frame(conc=seq(0,35,by=1))
#cars <- cbind(cars, "resp" = predict(m, type = "response", se.fit = TRUE)[1:2])

se(m2)

yhat <- predict(m2,  interval = "conf")  # list with two elements fit and se.fit
plot(x = newdata$MOVE_MAN_count, y= yhat)


yhat <- data.frame( fit=yhat$fit, se.fit = yhat$se.fit)
probs <- cbind(probs, yhat)
head(probs)

coefplot(m2)

########## 

summary(m1.stage2.ins)
summary(m1.ins, stage=1)

m2.ins= feols(Error_Count ~ 1 | DATE_EVENT+RSE_ID_ANONYM | MOVE_MAN_count +MOVE_AUTO_count~ TRAF_COMP + TRAF_DENS ,vcov = "twoway"
              ,data = df)
etable(m2.stage2.ols,m2.ins)
summary(m2.ins)
summary(m2.ins, stage=1)



#dfnew[c(colnames(df))]->0
#m6 = fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
#                TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO^2))| DATE_EVENT +Hour+WS_NO
#              ,data = df, vcov='twoway')#,fixef.rm ='both')

#how to get confidence interval 


#predict(m3)
######
#Plot
plot_predict<- function(b0,b1,b2,max_x){
  
  x = 0:max(max_x)
  #y = -3.9 + -0.052*x +0.001*x^2
  #y =  0.1326*x +0.0022*x^2
  y =  b0+ b1*x +b2*x^2
  real_y = exp(y)
  result<- list("x"=x,"real_y"=real_y,"y"=y)
  return(result)
}


# confident_interval<- confint(m3)
coeff<- data.frame(m3$coefficients)
# coeff$m3.coefficients[1]
# confident_interval$`2.5 %`[1]
# confident_interval$`97.5 %`[1]
# confident_interval$`97.5 %`[7]
# #https://freakonometrics.hypotheses.org/2289
result <- plot_predict(0,coeff$m3.coefficients[1],coeff$m3.coefficients[7],df$MOVE_MAN_count)
# #result.lb<-  plot_predict(0,confident_interval$`2.5 %`[1],confident_interval$`2.5 %`[7],df$MOVE_MAN_count)
# #result.ub<-  plot_predict(0,confident_interval$`97.5 %`[1],confident_interval$`97.5 %`[7],df$MOVE_MAN_count)
# dev.new()

#png(filename="Manual_fit.png")
png(filename="ALL_fit.png")

par(mfrow=c(2,2))
plot(result$x,result$real_y, xlab= "Manual Workload", ylab ='Number of Errors',main="Negative Binomimal predicted curve")
#dev.off()


result.auto <- plot_predict(0,coeff$m3.coefficients[2],coeff$m3.coefficients[8],df$MOVE_AUTO_count)
#png(filename="Monitor_fit.png")
plot(result.auto$x,result.auto$real_y, xlab= "Monitoring Workload", ylab ='Number of Errors',main="Negative Binomimal predicted curve")
#dev.off()

result.AA <- plot_predict(0,coeff$m3.coefficients[3],coeff$m3.coefficients[9],df$CHANGE_AUTO_AA_count)
#png(filename="AA_fit.png")
plot(result.AA$x,result.AA$real_y, xlab= "Switch to Auto Workload", ylab ='Number of Errors',main="Negative Binomimal predicted curve")
#dev.off()

result.QQ <- plot_predict(0,coeff$m3.coefficients[4],coeff$m3.coefficients[10],df$CHANGE_AUTO_WW_count)
#png(filename="WW_fit.png")
plot(result.QQ$x,result.QQ$real_y, xlab= "Switch to Manual Workload", ylab ='Number of Errors',main="Negative Binomimal predicted curve")
dev.off()




plot(result.ub$x,result.ub$real_y)#, xlab= "Move man  Workload", ylab ='Number of Errors',main="Negative Binomimal predicted curve")
plot(result.lb$x,result.lb$real_y)#, xlab= "Move man  Workload", ylab ='Number of Errors',main="Negative Binomimal predicted curve")


library(ggplot2)
#data_result <- data.frame(cbind("x"=result$x,"real_y"= result$real_y,"lower" = result.lb$real_y
#,"upper"=result.ub$real_y))
data_result <- data.frame(cbind("x"=result$x,"y"= result$y,"lower" = result.lb$y
                                ,"upper"=result.ub$y))

ggplot(data_result, aes(x, y)) +        # ggplot2 plot with confidence intervals
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))

predictions = predict(model, newdata = newdata, interval="predict")
lines(newdata$x, predictions[,1])
lines(newdata$x, predictions[,2], lty=2)
lines(newdata$x, predictions[,3], lty=2)
#####
df$MESSAGE_OTHER_count
m01 = feglm(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + MESSAGE_OTHER_count+
              TRAF_COMP  + PHONE 
            , 'poisson', data = df)
m02 = feglm(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
              TRAF_COMP  + PHONE 
            + I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)
            , 'poisson',data = df)

m1 = feglm(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
             TRAF_COMP  + PHONE +1
           | individual  + DATE_EVENT + Hour
           , 'poisson', se = 'IID',data = df)
m2 = feglm(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
             TRAF_COMP  + PHONE +1
           + I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)|individual+ DATE_EVENT +Hour
           , 'poisson', se = 'IID',data = df)
m3= feglm(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
            TRAF_COMP  + PHONE +1
          + I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO^2)+AGE+GENDER+RAILWAY_DAYS| WS_NO + DATE_EVENT +Hour
          , 'poisson', se = 'IID',data = df)


m3= feglm(Error_Count ~ I(MOVE_MAN_count+CHANGE_AUTO) + MOVE_AUTO_count  + 
            TRAF_COMP  + PHONE +1
          + I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+AGE+GENDER+RAILWAY_DAYS| WS_NO + DATE_EVENT +Hour
          , 'poisson', se = 'IID',data = df)

m4= feglm.nb(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
               TRAF_COMP  + PHONE +1
             + I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+AGE+GENDER+RAILWAY_DAYS| WS_NO + DATE_EVENT +Hour
             , 'poisson', se = 'IID',data = df)

m5= feglm(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
            TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2))| WS_NO + DATE_EVENT +Hour
          , 'poisson', se = 'IID',data = df)

######### 
#poisson 

m0.po= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
               PHONE+ TRAF_COMP
             ,data = df)

m1.po= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
               PHONE+ TRAF_COMP| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
             ,data = df)
#etable(m6)



m2.po= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
               PHONE+ TRAF_COMP  + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_AA_count^2)
                                       +I(CHANGE_AUTO_WW_count^2)),data = df)

m3.po= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
               PHONE+ TRAF_COMP  + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_AA_count^2)
                                       +I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
etable(m0.po,m1)
etable(m0.po,m1.po,m2.po,m3.po)
msummary(list(m0.po,m1.po,m2.po,m3.po))

#df$CHANGE_AUTO_AA_count
#df$CHANGE_AUTO_WW_count
m1= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_WW_count+CHANGE_AUTO_AA_count + 
             TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_WW_count^2)
                                                              +I(CHANGE_AUTO_AA_count^2))| DATE_EVENT +Hour,data = df
           ,fsplit = 'WS_NO')

m2= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count +CHANGE_AUTO_WW_count+CHANGE_AUTO_AA_count +
             TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_WW_count^2)
                                                              +I(CHANGE_AUTO_AA_count^2))| DATE_EVENT +Hour+WS_NO,data = df
)
m3= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_WW_count+CHANGE_AUTO_AA_count +
             TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ WS_NO+csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_WW_count^2)
                                                                    +I(CHANGE_AUTO_AA_count^2))| DATE_EVENT +Hour,data = df
)



m6 = fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_WW_count+CHANGE_AUTO_AA_count +
                TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO_WW_count^2)
                                                                 +I(CHANGE_AUTO_AA_count^2))| DATE_EVENT +Hour+WS_NO
              ,data = subset(df,CHANGE_AUTO_AA_count<20) , vcov='twoway')#,fixef.rm ='both')
summary(m6)
etable(m1,m2,m3,m6)



m6 = fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
                TRAF_COMP  + PHONE + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2))
              ,data = df)

m6= feols(Error_Rate ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
            TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2))| WS_NO + DATE_EVENT +Hour
          ,data = df)
#https://lrberge.github.io/fixest/reference/feglm.html
summary(m6)


summary(fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
         TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ factor(WS_NO)+csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2))| DATE_EVENT +Hour,data = df
       ,fsplit = 'GENDER'))

summary(m01)
etable(m01,m02, m1, m2,m3)
etable(m3[lhs = 1])
m2$coefficients

msummary(list(m01,m02, m1, m2), stars = TRUE)

m3 = glm(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
             TRAF_COMP + factor(shift) + PHONE 
           + I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2),
         'poisson', data = df)
summary(m3)


#adding Lag variables 
#l(MOVE_MAN_count, 0:10) means having lead of 10 for Move Man count.
#Testing if current errors are effected by previous workloads 
m3 = feglm(Error_Count ~ l(MOVE_MAN_count, -10:0) + l(MOVE_AUTO_count,-10:0) + CHANGE_AUTO + 
             TRAF_COMP + factor(shift) + PHONE 
           + I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)| individual + DATE_EVENT 
           , 'poisson', panel.id = c('individual','time'), data = df)
etable(m3)

#Add cumulative variables 

#Plot 

x = 0:80
y =  0.132371*x -0.002146*x^2
real_y = exp(y)
#plot(x,y, xlab= "Change Auto", ylab ='log(Number of Errors)', main="Negative Binomimal predicted curve")
#log()

#MOVE AUTO workload \

x = 0:max(df$MOVE_MAN_count)
#y = -3.9 + -0.052*x +0.001*x^2
#y =  0.1326*x +0.0022*x^2
y =  0.015*x -0.000442*x^2
real_y = exp(y)

plot(x,real_y, xlab= "Move man  Workload", ylab ='Number of Errors',main="Negative Binomimal predicted curve")


real_y = exp(y)
#plot(x,y, xlab= "Change Auto Workload", ylab ='log(Number of Errors)', main="Negative Binomimal predicted curve")
#log()

#plot(x,y, xlab= "Change Auto AA Workload", ylab ='log Number of Errors',main="Negative Binomimal predicted curve")

plot(x,real_y, xlab= "Move Auto  Workload", ylab ='Number of Errors',main="Negative Binomimal predicted curve")


#plot(x,real_y, xlab= "Change Auto", ylab ='Number of Errors',main="Poisson predicted curve")

ggplot(df, aes(x=factor(WS_NO), y=MOVE_AUTO_count)) +  geom_boxplot(fill='green')
ggplot(df, aes(x=factor(GENDER))) +  geom_bar(fill='green')
ggplot(df, aes(y=CHANGE_AUTO_AA_count)) +  geom_boxplot(fill='green')

df_no-out <- df[,df$CHANGE_AUTO_AA_count<20]

#### HOURLY

df_TCCQ <- subset(df, TCC_ANONYM =="Q")# & WS_NO =="01")
m1= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
             TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO^2))| DATE_EVENT ,data = df_TCCQ
           ,fsplit = 'WS_NO')

m2= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
             TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO^2))| DATE_EVENT,data = df_TCCQ
)
m3= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
             TRAF_COMP  + PHONE +AGE+GENDER+RAILWAY_DAYS+ WS_NO+csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO^2))| DATE_EVENT ,data = df_TCCQ
)



m6 = fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
                TRAF_COMP  + PHONE +AGE+GENDER+as.numeric(RAILWAY_DAYS)+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO^2))| DATE_EVENT
              ,data = df_TCCQ)#, vcov='twoway')#,fixef.rm ='both')

etable(m6)

m6 = fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO + 
              TRAF_COMP  + PHONE +AGE+GENDER+as.numeric(RAILWAY_DAYS)+ csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+I(CHANGE_AUTO^2))
            ,data = df_TCCQ)#, vcov='twoway')#,fixef.rm ='both')
etable(m6)

etable(m1,m2,m3,m6)
