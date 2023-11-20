"Appendix for the Reliance on Automation paper

Date: 05052923 

Contains other models include NB, passion and Quasai poisson " 




# m1= fepois(Error_rate ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP 
#              | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
# )
# m2= fepois(Error_rate ~ TotalWL + Auto_fraction +PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP + 
#                Auto_fraction_sq+TotalWL_sq| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
# )
# 
# 
# m3= fepois(Error_rate ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP+ 
#                Auto_fraction_sq+TotalWL_sq + Auto_fraction_sq*TotalWL_sq| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
# )

df$TotalWL_cubic = df$TotalWL^3
df$Auto_fraction_cubic = df$Auto_fraction^3

df$TotalWL_lag = df$
m_cubic = fepois(Error_rate ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP+ 
             Auto_fraction_sq+ TotalWL_sq + Auto_fraction_sq*TotalWL_sq + TotalWL_cubic +Auto_fraction_cubic
           | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

df$shift <- ifelse(df$Hour >= 6 & df$Hour < 13, "Morning", "Afternoon")

library(data.table)
library(dplyr)


grouped_df_lag<- df%>%
  group_by(DATE_EVENT, shift, TCC_ANONYM, RSE_ID_ANONYM, WS_NO)%>%
  mutate(lag_WL= dplyr::lag(TotalWL, n=1))


m_lag_wl = 
  fepois(Error_rate ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP+ 
                 Auto_fraction_sq+ TotalWL_sq + Auto_fraction_sq*TotalWL_sq +lag_WL
               | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = grouped_df_lag
)
summary(m_lag_wl)


grouped_df <- df %>% 
  group_by(DATE_EVENT, shift, TCC_ANONYM, RSE_ID_ANONYM, WS_NO)%>%
  mutate(lag_auto = dplyr:: lag(Auto_fraction, n=1))

# view the resulting grouped data frame
grouped_df




m_lag = fepois(Error_rate ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + PHONE+JUSTIF_count + MESSAGE_OTHER_count + Hour+ TRAF_COMP+ 
             Auto_fraction_sq+ TotalWL_sq + Auto_fraction_sq*TotalWL_sq +lag_auto
           | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = grouped_df
)
summary(m_lag)


################
cap <- 'Automation Usage Error Rate Table'
#omit<- 'RMSE|BIC'
estimate <- "{estimate} ({std.error}){stars}"
#gof <-  c("nobs", "AIC","logLik")
#gof_map <- c("AIC", "loglik")
note <- list('Signif. Codes: 0 “***” 0.001 “**” 0.01 “*” 0.05 “.” 0 “ “ 1 ')
models<- list("(1) =m_cubic,"(2) Automation Usage(Quadratic)"=m4.1,
              '(3) Auto Usage (Interaction)' = m6.1)

tab<- modelsummary(models, stars = TRUE,  output = "gt", estimate  = estimate,
                   #gof_map =gof,
                   #coef_map = cm,
                   statistic = NULL, coef_omit = "theta",
                   title=cap, notes =note
                   # , gof_omit = omit
)

#library("webshot2")
gt::gtsave(tab,filename = "Auto_table_rate_all_043023.docx")


