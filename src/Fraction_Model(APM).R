library(fixest)
library(MASS)
source("./Plot.R")
source("utils/Functions.R")

library(stargazer)


#create summary table
# df_sum < - round(describe(df_model, fast = TRUE), 3)
# 
# write.csv(df_sum, file = "sumstats.csv")
#stargazer(df_sum,title="Descriptive Satistics", type ="text",  digit =2,  out= "/APM_results/sumstats.txt")



#####

m1= fenegbin(Error_Count ~ TotalWL + 
 +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour-1| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df
)
m2= fenegbin(Error_Count ~ TotalWL + Manual_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
               I(Manual_fraction^2)+I(TotalWL^2)| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df
)
m3= fenegbin(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour-1| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df
)
m4= fenegbin(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ 
               I(Auto_fraction^2)+I(TotalWL^2)| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df
)
#Why in m4, Auto_fraction is not significant but ^2 is significant 
etable(m1,m2,m3,m4)

etable(m3,m4)

#normallized?
m_nb= glm.nb(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
            I(Auto_fraction^2)+I(TotalWL^2), data = df, start = c(0.005, 0.006, 2.1, 0.0006,-0.0001,0.004,0.0005,-5.8,-0.000005))
m_qp= glm(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
               I(Auto_fraction^2)+I(TotalWL^2), data = df, family = "quasipoisson")
predicted.y = predict(m_qp, newdata=df, type="response")

dnbinom(100, mu=predicted.y, size=m4$theta)

est <- cbind(Estimate = coef(m4), confint(m4))
library(VGAM)
fit <- vglm(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
              I(Auto_fraction^2)+I(TotalWL^2), data = df, family=negbinomial(), )
summary(fit)
predict(fit, se.fit=TRUE)

library(ggplot2)

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

#########
#Hausaman Test function is in Functions.R script
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

#####
########
library(MASS)

# nbin= fepois(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
#                I(Auto_fraction^2)+I(TotalWL^2)| DATE_EVENT+RAILWAY_DAYS+AGE,vcov = "twoway", data = df
# )

m4= glm(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count +# MESSAGE_OTHER_count + Hour+
               I(Auto_fraction^2)+I(TotalWL^2), family = negative.binomial(0.5), data = df
)

predict.data$predicted_all <- predict(m4, predict.data, type = 'response')
predict.data$predicted_all_link <- predict(m4, predict.data, type = 'link')


m4=  glm.nb(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
          I(Auto_fraction^2)+I(TotalWL^2), data = df)
#m2_nb= glm.nb(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+DATE_EVENT+I(Auto_fraction^2)+I(TotalWL^2),data = df)

#m2_nb= glm.nb(Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+EXPERIENCE_DAYS+DATE_EVENT+
#               I(Auto_fraction^2)+I(TotalWL^2),data = df)
predict.data <- setNames(data.frame(matrix(0, ncol = 9, nrow =100)), c('TotalWL','Auto_fraction', 'PHONE','JUSTIF_count' ,
                                                                       'MESSAGE_OTHER_count','Hour','RAILWAY_DAYS','AGE','DATE_EVENT'))
# predict.data$Auto_fraction<-predict.data$Auto_fraction*0.01
#predict.data$DATE_EVENT <- as.character(predict.data$DATE_EVENT)
predict.data1<- predict_X(Auto_fraction, m_qp, predict.data)

dev.off()

plt<- plotCI(predict.data1, m_qp, Auto_fraction)

ggsave("./APM_results/Auto_fraction.png", width = 6, height = 4, dpi = 300, plot = plt)

#effect_plot(m_qp, pred = Auto_fraction, interval = TRUE, rug = TRUE)

#ggplot(predict.data, aes(x=Auto_fraction, y=predicted_all)) + geom_point()


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

library(modelsummary)
library(gt)
library(dplyr)

Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)| DATE_EVENT+RAILWAY_DAYS+AGE

cm <- c( 'TotalWL' = 'Total WL', 'Auto_fraction' = 'Automaton Percentage',
         'I(Auto_fraction^2)' = 'Auto_fraction^2',
         'I(TotalWL^2)' = 'TotalWL^2', 'Manual_fraction' = "Manual Usage"
         # ,'MOVE_MAN_resi' ='Manual Residual','MOVE_AUTO_resi' ='Monitoring Residual',
         #'I(MOVE_MAN_resi^2)' = 'MOVE_MAN_resi_Sq','I(MOVE_AUTO_resi^2)' = 'MOVE_AUTO_resi_Sq'
)

cap <- 'Other Models'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
#gof_map <- c("AIC", "loglik")
note <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')
models<- list("(1) Linear"=m3,"(2) Quadratic "=m4)

tab<- modelsummary(models, stars = TRUE,  output = "gt", estimate  = estimate,#gof_map =gof,
                   #coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap, notes =note
                   # , gof_omit = omit
)

#library("webshot2")
gt::gtsave(tab,filename = "Auto_Manual_table2.docx")

