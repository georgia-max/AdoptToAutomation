library(ivreg)
library(car)
#1. Instrument Relevance 
###### 
#First, we test whether we picked useful IVs (TraffComp & TraffDens). 

m2.lm.moveman = feols(MOVE_MAN_count ~ TRAF_COMP+TRAF_DENS 
                      +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
                      + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                      #+ AGE + EXPERIENCE_DAYS + GENDER 
                      | DATE_EVENT+RSE_ID_ANONYM, data = df, vcov="twoway")

instrFtest <- waldtest(m2.lm.moveman, .~.-TRAF_COMP-TRAF_DENS )

print(instrFtest)
linearHypothesis(m2.lm.moveman, c("TRAF_COMP=0", "TRAF_DENS=0"))

#Ftest is 2677, with P<0.05, reject the H0 that IV is irrelevant 

m2.lm.moveauto = feols(MOVE_AUTO_count ~ TRAF_COMP+TRAF_DENS 
                      +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
                      + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                     # + AGE + EXPERIENCE_DAYS + GENDER 
                      | DATE_EVENT+RSE_ID_ANONYM, data = df, vcov="twoway")

instrFtest <- waldtest(m2.lm.moveauto, .~.-TRAF_COMP-TRAF_DENS )
print(instrFtest)
#Ftest is 1215.8, with P<0.05, reject the H0 that IV is irrelevant 
# When discard AA & WW as exo, Ftest is 1430, with P<0.05, reject the H0 that IV is irrelevant 

m2.lm.AA = feols(CHANGE_AUTO_AA_count ~ TRAF_COMP+TRAF_DENS 
                       + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                       + AGE + EXPERIENCE_DAYS + GENDER | DATE_EVENT, data = df, vcov="twoway")
summary(m2.lm.AA)
instrFtest <- waldtest(m2.lm.AA, .~.-TRAF_COMP-TRAF_DENS )
print(instrFtest)
#Ftest is 396, with P<0.05, reject the H0 that IV is irrelevant 

m2.lm.WW = feols(CHANGE_AUTO_WW_count ~ TRAF_COMP+TRAF_DENS 
                 + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour 
                 + AGE + EXPERIENCE_DAYS + GENDER | DATE_EVENT, data = df, vcov="twoway")
summary(m2.lm.WW)
instrFtest <- waldtest(m2.lm.WW, .~.-TRAF_COMP-TRAF_DENS )
print(instrFtest)
#Ftest is 220, with P<0.05, reject the H0 that IV is irrelevant 



#2. Test for exogeneity 
######
df$s1.res.moveman <- m2.lm.moveman$residuals
Hausman_reg<- fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+s1.res.moveman+
                             csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
summary(Hausman_reg)

HausWutest<- waldtest(Hausman_reg, .~.-s1.res.moveman)
print(HausWutest)
#The result is a p-value of 0.081. So at an a = 0.05 we just fail to reject the null of x is not exogenous.

df$s1.res.moveauto <- m2.lm.moveauto$residuals
Hausman_reg<- fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+s1.res.moveauto+
                         csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
HausWutest<- waldtest(Hausman_reg, .~.-s1.res.moveauto)
print(HausWutest)
#The result is a p-value of 0.0051. So at an a = 0.05 we just fail to reject the null of x is not exogenous.

df$s1.res.AA <- m2.lm.AA$residuals
Hausman_reg<- fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+s1.res.AA+
                         csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
HausWutest<- waldtest(Hausman_reg, .~.-s1.res.AA)
print(HausWutest)
#The result is a p-value of 0.01. So at an a = 0.05 we just fail to reject the null of x is not exogenous.

df$s1.res.WW <- m2.lm.WW$residuals
Hausman_reg<- fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+s1.res.WW+
                         csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
HausWutest<- waldtest(Hausman_reg, .~.-s1.res.WW)
print(HausWutest)
#The result is a p-value of 8.39 e-5  So at an a = 0.05 we just fail to reject the null of x is not exogenous.


#3. Sargan test for instrument validity 
########

m= ivreg(Error_Count ~   MOVE_MAN_count + MOVE_AUTO_count+ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
         | .-MOVE_MAN_count - MOVE_AUTO_count -CHANGE_AUTO_AA_count -CHANGE_AUTO_WW_count+ TRAF_COMP+TRAF_DENS , data = df)
summary(m)

Sargan_reg <- lm(m$residuals ~MOVE_MAN_count +MOVE_AUTO_count + TRAF_COMP+TRAF_DENS 
                 +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count
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
             D=~CHANGE_AUTO_WW_count+CHANGE_AUTO_AA_count+ MOVE_MAN_count + MOVE_AUTO_count, # Treatments
             Z=~TRAF_COMP+TRAF_DENS,# Instruments
             data = df)
#Smaller than 10 reject, 
cragg_donald(X=~ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour+CHANGE_AUTO_WW_count, # Control Variables
             D=~CHANGE_AUTO_AA_count+ MOVE_MAN_count + MOVE_AUTO_count, # Treatments
             Z=~TRAF_COMP+TRAF_DENS,# Instruments
             data = df)
#Smaller than 10, reject 
cragg_donald(X=~ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour+CHANGE_AUTO_AA_count, # Control Variables
             D=~CHANGE_AUTO_WW_count+ MOVE_MAN_count + MOVE_AUTO_count, # Treatments
             Z=~TRAF_COMP+TRAF_DENS,# Instruments
             data = df)
#Smaller than 10 reject 

cragg_donald(X=~ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour+CHANGE_AUTO_AA_count+CHANGE_AUTO_WW_count, # Control Variables
             D=~MOVE_MAN_count + MOVE_AUTO_count, # Treatments
             Z=~TRAF_COMP+TRAF_DENS,# Instruments
             data = df)
#Larger than 10, 137, final equation! 

cragg_donald(X=~ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour+CHANGE_AUTO_WW_count+CHANGE_AUTO_AA_count, # Control Variables
             D=~MOVE_MAN_count + MOVE_AUTO_count, # Treatments
             Z=~TRAF_COMP+TRAF_DENS,# Instruments
             data = df)

cragg_donald(X=~ PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour+MOVE_MAN_count + MOVE_AUTO_count, # Control Variables
             D=~CHANGE_AUTO_WW_count+CHANGE_AUTO_AA_count, # Treatments
             Z=~TRAF_COMP+TRAF_DENS,# Instruments
             data = df)

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
#gt::gtsave(tab,filename = "1ststage.png")



