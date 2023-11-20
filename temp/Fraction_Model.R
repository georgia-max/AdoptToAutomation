#####
# df$TotalWL<-  df$MOVE_MAN_count+df$CHANGE_AUTO_AA_count+ df$CHANGE_AUTO_WW_count+df$MOVE_AUTO_count+df$ADAPTATIONS_count
# #Adding Adaptations to total WL 
# df$Manual_count<- df$MOVE_MAN_count+df$CHANGE_AUTO_AA_count+ df$CHANGE_AUTO_WW_count+df$ADAPTATIONS_count
# df$Manual_fraction<- df$Manual_count/df$TotalWL
# df$Auto_fraction <- df$MOVE_AUTO_count/df$TotalWL
# df$ADAPTATIONS_count
m1= fenegbin(Error_Count ~ TotalWL + Manual_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
m2= fenegbin(Error_Count ~ TotalWL + Manual_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
               I(Manual_fraction^2)+I(TotalWL^2)| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
m3= fenegbin(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
m4= fenegbin(Error_Count ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+
               I(Auto_fraction^2)+I(TotalWL^2)| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)
#Why in m4, Auto_fraction is not significant but ^2 is significant 
#etable(m1,m2,m3,m4)
etable(m3,m4)


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
formula1 <- Auto_fraction ~ TRAF_COMP+TRAF_DENS + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour |DATE_EVENT+RSE_ID_ANONYM
#WithoutResiduals 
formula2 <- Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)|DATE_EVENT+RSE_ID_ANONYM
#With Residuals 
formula3<- Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)+residuals|DATE_EVENT+RSE_ID_ANONYM


Reg_2nd<- Hasusman_Test(formula1, formula2,formula3, df2)
summary(Reg_2nd)
#Result. Auto_fraction is slightly endogenous  

#Testing Manual_fraction Endogenous 
#1ststage 
formula1 <- Manual_fraction ~ TRAF_COMP+TRAF_DENS + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour |DATE_EVENT+RSE_ID_ANONYM
#WithoutResiduals 
formula2 <- Error_Count ~ TotalWL + Manual_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Manual_fraction^2)+I(TotalWL^2)|DATE_EVENT+RSE_ID_ANONYM
#With Residuals 
formula3<- Error_Count ~ TotalWL + Manual_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Manual_fraction^2)+I(TotalWL^2)+residuals|DATE_EVENT+RSE_ID_ANONYM


Reg_2nd<- Hasusman_Test(formula1, formula2,formula3, df2)
summary(Reg_2nd)
#Result. Manual_fraction is endogenous  

#Testing TWL Endogenous 
#1ststage 
formula1 <- TotalWL ~ TRAF_COMP+TRAF_DENS + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour |DATE_EVENT+RSE_ID_ANONYM
#WithoutResiduals 
formula2 <- Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)|DATE_EVENT+RSE_ID_ANONYM
#With Residuals 
formula3<- Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
  I(Auto_fraction^2)+I(TotalWL^2)+residuals|DATE_EVENT+RSE_ID_ANONYM

Reg_2nd<- Hasusman_Test(formula1, formula2,formula3, df2)
summary(Reg_2nd)
#Result. TWL is exogeous 

#####
########
m2_nb= glm.nb(Error_Count ~ TotalWL + Auto_fraction +PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour+
               I(Auto_fraction^2)+I(TotalWL^2),data = df
)
predict.data <- setNames(data.frame(matrix(0, ncol = 11, nrow =35)), c('TotalWL','Auto_fraction', 'JUSTIF_count','PHONE' , 'TRAF_COMP'
                                                                       ,'MESSAGE_OTHER_count','DATE_EVENT','RSE_ID_ANONYM','Hour'))
predict.data$DATE_EVENT <- as.character(predict.data$DATE_EVENT)
predict.data$RSE_ID_ANONYM <- as.character(predict.data$RSE_ID_ANONYM)
predict.data<- predict_X(TotalWL, m2_nb, predict.data )
dev.off()
#source("Plot.R", print.eval=TRUE)
#png(filename="MVE_MAN.png")

plt<- plotCI(predict.data, m2_nb, TotalWL)
ggsave("TotalWL.png", width = 6, height = 4, dpi = 300, plot = plt)

summary(m2_nb)

######

predict.data <- setNames(data.frame(matrix(0, ncol = 11, nrow =35)), c('TotalWL','Auto_fraction', 'JUSTIF_count','PHONE' , 'TRAF_COMP'
                                                                       ,'MESSAGE_OTHER_count','DATE_EVENT','RSE_ID_ANONYM','Hour'))
predict.data$DATE_EVENT <- as.character(predict.data$DATE_EVENT)
predict.data$RSE_ID_ANONYM <- as.character(predict.data$RSE_ID_ANONYM)
predict.data<- predict_X(Auto_fraction, m2_nb, predict.data )
dev.off()
#source("Plot.R", print.eval=TRUE)
#png(filename="MVE_MAN.png")

plt<- plotCI(predict.data, m2_nb, Auto_fraction)
ggsave("Auto_fraction.png", width = 6, height = 4, dpi = 300, plot = plt)

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