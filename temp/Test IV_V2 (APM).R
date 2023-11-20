library(ivreg)
library(car)
library(lmtest)
#Testing with APM DATASET 

#1. Instrument Relevance 
#df$Manual_count <- df$MOVE_MAN_count+df$CHANGE_AUTO_AA_count+ df$CHANGE_AUTO_WW_count+df$CHANGE_AUTO_REMOVE_TIMER_count
#Make sure that the data does not have na values
df<-df_model
###### 
#First, we test whether we picked useful IVs (TraffComp & TraffDens). 

m2.lm.moveman = feols(Manual_count ~ TRAF_COMP+TRAF_DENS 
                      + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour | DATE_EVENT+RAILWAY_DAYS+AGE, data = df, vcov="twoway")

instrFtest <- waldtest(m2.lm.moveman, .~.-TRAF_COMP-TRAF_DENS )

print(instrFtest)
linearHypothesis(m2.lm.moveman, c("TRAF_COMP=0", "TRAF_DENS=0"))

#Ftest is 544.11, with P<0.05, reject the H0 that IV is irrelevant 

m2.lm.moveauto = feols(MOVE_AUTO_count ~ TRAF_COMP+TRAF_DENS 
                      + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                     # + AGE + EXPERIENCE_DAYS + GENDER 
                      | DATE_EVENT+RAILWAY_DAYS+AGE, data = df, vcov="twoway")

instrFtest <- waldtest(m2.lm.moveauto, .~.-TRAF_COMP-TRAF_DENS )
print(instrFtest)
#Ftest is 20587, with P<0.05, reject the H0 that IV is irrelevant 



#2. Test for exogeneity 
######
df$s1.res.moveman <- m2.lm.moveman$residuals
Hausman_reg<- fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +s1.res.moveman+
                             csw(I(Manual_count^2) + I(MOVE_AUTO_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
summary(Hausman_reg)

HausWutest<- waldtest(Hausman_reg, .~.-s1.res.moveman)
print(HausWutest)
#The result is a p-value of 0.01086. So at an a = 0.05 we just fail to reject the null of x is not exogenous.

df$s1.res.moveauto <- m2.lm.moveauto$residuals
Hausman_reg<- fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +s1.res.moveauto+
                         csw(I(Manual_count^2) + I(MOVE_AUTO_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
HausWutest<- waldtest(Hausman_reg, .~.-s1.res.moveauto)
print(HausWutest)
#The result is a p-value of 0.02. So at an a = 0.05 we just fail to reject the null of x is not exogenous.

#3. Sargan test for instrument validity 
########
library("ivreg")
m= ivreg(log(Error_Count) ~   Manual_count + MOVE_AUTO_count+ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
         | .-Manual_count - MOVE_AUTO_count + TRAF_COMP+TRAF_DENS , data = df)
summary(m)

Sargan_reg <- lm(m$residuals ~Manual_count +MOVE_AUTO_count + TRAF_COMP+TRAF_DENS 
               
                 + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour, data = df )
Sargan_reg_sm <- summary(Sargan_reg)
Sargan_test <- Sargan_reg_sm$r.squared*nrow(df)
print(Sargan_test)
print(1-pchisq(Sargan_test,1))

#We find that the p-value of this test is 0 and hence 
#we do not reject the null hypothesis of that IV is correlaated with errors 

#####
library(cragg)
cragg_donald(X=~ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour, # Control Variables
             D=~Manual_count + MOVE_AUTO_count, # Treatments
             Z=~TRAF_COMP+TRAF_DENS,# Instruments
             data = df)

#Larger than 10, final equation! 



# stock_yogo_test(X=~ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour, # Control Variables
#              D=~MOVE_MAN_count , # Treatments
#              Z=~TRAF_COMP+TRAF_DENS,# Instruments
#              size_bias="bias", #Default
#              B=.05, #Default,
#              data = df)
# 
# cragg_donald(X=~ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour, # Control Variables
#              D=~MOVE_MAN_count + MOVE_AUTO_count+CHANGE_AUTO_WW_count+CHANGE_AUTO_AA_count, # Treatments
#              Z=~TRAF_COMP,# Instruments
#              data = df)

library(car)
m1.lm = lm(MOVE_AUTO_count ~ TRAF_COMP+TRAF_DENS 
           +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
           +  AGE  + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour, data = df)
linearHypothesis(m1.lm, "TRAF_COMP = 0", 
                 vcov = vcovHC, type = "HC1")
#compute the Sargan–Hansen test( J-statistic) to test exclusion restriction 
#https://en.wikipedia.org/wiki/Instrumental_variables_estimation
#The most common test of these overidentifying restrictions, called the Sargan–Hansen test, is based on the observation 
#that the residuals should be uncorrelated with the set of exogenous variables if the instruments are truly exogenous
library(plm)

#######################
m1_iv_OR <- lm(residuals(m1.lm) ~ TRAF_COMP+TRAF_DENS , data = df)

cig_OR_test <- linearHypothesis(m1_iv_OR, 
                                c("TRAF_COMP = 0", "TRAF_DENS = 0"), 
                                test = "Chisq")
cig_OR_test

pchisq(cig_OR_test[2, 5], df = 1, lower.tail = FALSE)
#Since this value is 1 > 0.05 we do not reject the hypothesis that both instruments are exogenous at the level of 5%.

#https://www.econometrics-with-r.org/12-4-attdfc.html
#cm <- c( 'TRAF_COMP'= 'Traffic Complexity','TRAF_DENS'= 'Traffic Density')
cap <- 'First stage OLS regression Results'
omit<- 'AIC|BIC'
estimate <- "{estimate} ({std.error}){stars}"
models<- list("Manual"=m1.lm.moveman,"Monitoring"=m1.lm.moveauto,"SwAuto"= m1.lm.AA,"SwMan"= m1.lm.WW )

tab<- modelsummary(models, stars = TRUE,  output = "gt", estimate  = estimate, gof_omit = omit,#coef_map = cm,
                   statistic = NULL,
                   title=cap)
library("webshot2")
gt::gtsave(tab,filename = "1ststage.png")
### 
# m1.ins= feols(Error_Count ~ MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
#                PHONE | DATE_EVENT+RSE_ID_ANONYM | MOVE_MAN_count ~ TRAF_COMP  ,vcov = "twoway"
#              ,data = df)

# m1.ins= feols(log(Error_Count) ~ 1 | DATE_EVENT+RSE_ID_ANONYM | MOVE_MAN_count +MOVE_AUTO_count ~ TRAF_COMP + TRAF_DENS ,vcov = "twoway"
#               ,data = df)

m1= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count+ CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
               PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour
             | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
             ,data = df)

# m1.stage2.ins= fenegbin(Error_Count ~ MOVE_MAN_hat + MOVE_AUTO_hat +
#                PHONE| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
#              ,data = df)

m2.stage2.ins= fenegbin(Error_Count ~ MOVE_MAN_hat + MOVE_AUTO_hat + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
                          PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour
                        #+Eroor
                        | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
                        ,data = df)

m3.stage2.ins= fenegbin(Error_Count ~ MOVE_MAN_hat + MOVE_AUTO_hat + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
                          PHONE  + csw(I(MOVE_MAN_hat^2) + I(MOVE_AUTO_hat^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2)
                          )-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)


m3= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
               PHONE  + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

etable(m1,m1.stage2.ins,m2.stage2.ins, m3, m3.stage2.ins)



######### 
#######
library(modelsummary)

summary(m2.lm.moveman)
cap <- 'First stage OLS regression Results'
omit<- 'AIC|BIC'
estimate <- "{estimate} ({std.error}){stars}"
models<- list("Manual"=m2.lm.moveman,"Monitoring"=m2.lm.moveauto )

tab<- modelsummary(models, stars = TRUE,  output = "table3.docx", estimate  = estimate, gof_omit = omit,#coef_map = cm,
                   statistic = NULL,
                   title=cap)

library("webshot2")
gt::gtsave(tab,filename = "1ststage_v2.docx")



