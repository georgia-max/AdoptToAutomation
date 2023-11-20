#plot fixed OLS Error Rate 
# @date 09/06/2023
# @author Georgia 
# 
# This script plots the relationship between Error Rate and Workload and Auto 
# Reliance with the fixed OLS regression. 
# The regression model is ran in Fraction_Model(EventBased)_v2.R script. 


ols_auto_rate_pred<- feols(Error_rate ~ TotalWL + Auto_fraction +PHONE+ 
                           JUSTIF_count + MESSAGE_OTHER_count + Hour+
                           Auto_fraction_sq+TotalWL_sq  + factor(DATE_EVENT) + 
                           factor(RSE_ID_ANONYM), data = df)


ols_auto_inter_rate_pred <- feols(Error_rate ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + 
                                 PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour +
                                 Auto_fraction_sq+TotalWL_sq + Auto_fraction_sq*TotalWL_sq + TRAF_COMP + 
                                 factor(DATE_EVENT) + factor(RSE_ID_ANONYM), data = df)



all_my_vars <- c("TotalWL", "TotalWL_sq", "Manual_fraction","Auto_fraction","JUSTIF_count",
                 "PHONE","MESSAGE_OTHER_count", "Hour", "TRAF_COMP",
                 "Manual_fraction_sq", "Auto_fraction_sq", "DATE_EVENT","RSE_ID_ANONYM")

preddf <- setNames(data.frame(matrix(0, ncol = length(all_my_vars), nrow =200)), all_my_vars)
preddf$PHONE <-  mean(df$PHONE)
preddf$JUSTIF_count <- mean(df$JUSTIF_count)
preddf$MESSAGE_OTHER_count <- mean(df$MESSAGE_OTHER_count)
preddf$Hour <- mean(df$Hour)
preddf$TotalWL  <- mean(df$TotalWL)
preddf$TotalWL_sq  <- mean(df$TotalWL_sq)
preddf$TRAF_COMP<- mean(df$TRAF_COMP)

preddf$DATE_EVENT <- sample(df$DATE_EVENT, 1)
preddf$RSE_ID_ANONYM <- sample(df$RSE_ID_ANONYM, 1)


# For Workload and Human Error Rate
TotalWL <- deparse(substitute(TotalWL)) 
preddf$TotalWL<- seq(1,nrow(preddf)) 
preddf$TotalWL_sq<- preddf$TotalWL^2 
preddf$Auto_fraction <- mean(df$Auto_fraction)
preddf$Auto_fraction_sq <-preddf$Auto_fraction^2
head(preddf)

# Predict
preddf$Error_rate_auto_inter_predict <- predict(ols_auto_inter_rate_pred, newdata = preddf, type = "response")


my_plot <- ggplot(preddf, aes(x=TotalWL)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  geom_line(aes(y = Error_rate_auto_inter_predict), linewidth = 1.5) + 
  xlab("Signaling Workload")+
  ylab("Human Error Rate (Per thousand workload)") 

# ggplotly(my_plot)

ggsave(filename = "./Event_Based_results/September2023/TotalWL_errorrate_v2.png", width = 6, height = 4, dpi = 300, plot = my_plot)


# For Auto Reliance v.s. Human Error Rate
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

# Plot one level WL

preddf$Error_predict_autoWL <- predict(ols_auto_inter_rate_pred, newdata = preddf, type = "response")

my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_predict_autoWL), linewidth = 2) + 
  
  xlab("Degree of Automation Reliance")+
  ylab("Human Error Rate (Per thousand workload)") 
plot(my_plot)

ggsave(filename = "./Event_Based_results/September2023/Auto_errorrate.png", width = 6, height = 4, dpi = 300, plot = my_plot)

# Plot different level WL
########
preddf$ErrorRate_predict_auto_highWL <- predict(ols_auto_inter_rate_pred, newdata = preddf_highWL, type = "response")
preddf$ErrorRate_predict_auto_lowWL <- predict(ols_auto_inter_rate_pred, newdata = preddf_lowWL, type = "response")
preddf$ErrorRate_predict_auto_medWL <- predict(ols_auto_inter_rate_pred, newdata = preddf_medWL, type = "response")

my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = ErrorRate_predict_auto_lowWL, colour = "Low WL"), linewidth = 2) + 
  geom_line(aes(y = ErrorRate_predict_auto_highWL, colour = "High WL"), linewidth = 2) + 
  geom_line(aes(y = ErrorRate_predict_auto_medWL, colour = "Medium WL"), linewidth = 2) + 
  
  xlab("Degree of Automation Reliance")+
  ylab("Human Error Rate (Per thousand workload)") + 
  
  # scale_color_manual(name = "Model", values = c("m2" = "darkred", "m4" = "steelblue"))
  scale_colour_manual("", 
                      breaks = c("High WL", "Low WL","Medium WL" ),
                      values = c("darkred", "steelblue", 'green')) 

plot(my_plot)

ggsave(filename = "./Event_Based_results/September2023/Auto_interactionErrorRate.png", width = 6, height = 4, dpi = 300, plot = my_plot)

# This script plots the relationship between Error Count and Workload and Auto 
# Reliance with the fixed NB regression. 
# The regression model is ran in Fraction_Model(EventBased)_v2.R script. 


nb_auto_count_pred<- fenegbin(Error_Count ~ TotalWL + Auto_fraction +PHONE+ 
                             JUSTIF_count + MESSAGE_OTHER_count + Hour+
                             Auto_fraction_sq+TotalWL_sq  + factor(DATE_EVENT) + 
                             factor(RSE_ID_ANONYM), data = df)


nb_auto_inter_count_pred <- fenegbin(Error_Count ~ TotalWL + Auto_fraction + TotalWL*Auto_fraction + 
                                    PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour +
                                    Auto_fraction_sq+TotalWL_sq + Auto_fraction_sq*TotalWL_sq + TRAF_COMP + 
                                    factor(DATE_EVENT) + factor(RSE_ID_ANONYM), data = df)



all_my_vars <- c("TotalWL", "TotalWL_sq", "Manual_fraction","Auto_fraction","JUSTIF_count",
                 "PHONE","MESSAGE_OTHER_count", "Hour", "TRAF_COMP",
                 "Manual_fraction_sq", "Auto_fraction_sq", "DATE_EVENT","RSE_ID_ANONYM")

preddf <- setNames(data.frame(matrix(0, ncol = length(all_my_vars), max(df$TotalWL))), all_my_vars)
preddf$PHONE <-  mean(df$PHONE)
preddf$JUSTIF_count <- mean(df$JUSTIF_count)
preddf$MESSAGE_OTHER_count <- mean(df$MESSAGE_OTHER_count)
preddf$Hour <- mean(df$Hour)
preddf$TotalWL  <- mean(df$TotalWL)
preddf$TotalWL_sq  <- mean(df$TotalWL_sq)
preddf$TRAF_COMP<- mean(df$TRAF_COMP)

preddf$DATE_EVENT <- sample(df$DATE_EVENT, 1)
preddf$RSE_ID_ANONYM <- sample(df$RSE_ID_ANONYM, 1)


# For Workload and Human Error Rate
TotalWL <- deparse(substitute(TotalWL)) 
preddf$TotalWL<- seq(1,nrow(preddf)) 
preddf$TotalWL_sq<- preddf$TotalWL^2 
preddf$Auto_fraction <- mean(df$Auto_fraction)
preddf$Auto_fraction_sq <-preddf$Auto_fraction^2
head(preddf)

# Predict
preddf$Error_count_auto_inter_predict <- predict(nb_auto_inter_count_pred, newdata = preddf, type = "response")


my_plot <- ggplot(preddf, aes(x=TotalWL)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  geom_line(aes(y = Error_count_auto_inter_predict), linewidth = 1.5) + 
  xlab("Workload")+
  ylab("Human Error Count") 

ggplotly(my_plot)

ggsave(filename = "./Event_Based_results/September2023/TotalWL_errorcount_WL200.png", width = 6, height = 4, dpi = 300, plot = my_plot)


# For Auto Reliance v.s. Human Error Count
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

# Plot one level WL

preddf$Error_predict_autoWL <- predict(nb_auto_inter_count_pred, newdata = preddf, type = "response")

my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10))+
  
  geom_line(aes(y = Error_predict_autoWL), linewidth = 2) + 
  
  xlab("Degree of Automation Reliance")+
  ylab("Human Error Count") 
ggplotly(my_plot)

ggsave(filename = "./Event_Based_results/September2023/Auto_errorcount.png", width = 6, height = 4, dpi = 300, plot = my_plot)

# Plot different level WL
########
preddf$Errorcount_predict_auto_highWL <- predict(nb_auto_inter_count_pred, newdata = preddf_highWL, type = "response")
preddf$Errorcount_predict_auto_lowWL <- predict(nb_auto_inter_count_pred, newdata = preddf_lowWL, type = "response")
preddf$Errorcount_predict_auto_medWL <- predict(nb_auto_inter_count_pred, newdata = preddf_medWL, type = "response")


my_plot <- ggplot(preddf, aes(x=Auto_fraction)) + 
  theme_light()+theme(axis.text.x = element_text(size = 10),
                      axis.text.y = element_text(size = 10)) +
  geom_line(aes(y = Errorcount_predict_auto_lowWL, linetype = "Low Workload"), linewidth = 2) + 
  geom_line(aes(y = Errorcount_predict_auto_highWL, linetype = "High Workload"), linewidth = 2) + 
  geom_line(aes(y = Errorcount_predict_auto_medWL, linetype = "Medium Workload"), linewidth = 2) + 
  
  xlab("Degree of Automation Reliance") +
  ylab("Human Error Count") + 

  scale_linetype_manual("", values=c("Low Workload" = "solid", "High Workload" ="twodash",  "Medium Workload"= "dotted"))

ggplotly(my_plot)
ggsave(filename = "./Event_Based_results/September2023/Auto_interactionErrorCount_blackwhite.png", width = 6, height = 4, dpi = 300, plot = my_plot)

