nb= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count 
             + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
             + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

po= fepois(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count 
             + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
             + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)



qupoi= feglm(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count + CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count 
           + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
           + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,family = "quasipoisson", vcov = "twoway", data = df
)

library(modelsummary)
library(gt)
library(dplyr)

cm <- c( 'MOVE_MAN_count' = 'Manual', 'MOVE_AUTO_count' = 'Monitoring',
         'CHANGE_AUTO_AA_count' = 'SwAuto','CHANGE_AUTO_WW_count' = 'SwMan',
         'I(MOVE_MAN_count^2)' = 'Manual_Sq',
         'I(MOVE_AUTO_count^2)' = 'Monitoring_Sq','I(CHANGE_AUTO_AA_count^2)'='SwAuto_Sq',
         'I(CHANGE_AUTO_WW_count^2)'='SwMan_Sq','MOVE_MAN_resi' ='Manual Residual','MOVE_AUTO_resi' ='Monitoring Residual',
         'I(MOVE_MAN_resi^2)' = 'MOVE_MAN_resi_Sq','I(MOVE_AUTO_resi^2)' = 'MOVE_AUTO_resi_Sq')

cap <- 'Other Models'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
models<- list("(1) NB"=nb,"(2)Poisson"=po,"(3)Quasi-poisson"=qupoi)
#gof_map <- c("AIC", "loglik")
note <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')
tab<- modelsummary(models, stars = TRUE,  output = "gt", estimate  = estimate,#gof_map =gof,
                   coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap, notes =note
                   # , gof_omit = omit
)

#library("webshot2")
gt::gtsave(tab,filename = "table5_differentmodels.docx")
