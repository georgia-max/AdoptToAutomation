## packages
library('readr')
library('tibble')
library('dplyr')
library('ggplot2')
#########
plotCI<- function(newdata,mymodel, X, X_Name){
  
  X <- deparse(substitute(X))
  ## plot it
  plt <- ggplot(newdata, aes(x = .data[[X]], y = fit)) +
    #geom_line() +
    geom_point() +
    
    #geom_rug(aes(y = visited, colour = lvisited), data = wasp) +
    #scale_colour_discrete(name = 'Visited') +
    labs(x = X_Name,y = 'Estimated Number of Errors')
  plt
  
  # plt<-plt + geom_ribbon(data = newdata,
  #                   aes(ymin = right_lwr, ymax = right_upr),
  #                   alpha = 0.1)
  # # plt<- plt+ geom_errorbar(data = newdata, aes(ymin=right_lwr, ymax=right_upr)) + theme_bw()
  print(plt)
  return(plt)
}



predict_X <- function(col_name,mymodel, newdata){
  col_name <- deparse(substitute(col_name))
  newdata[[col_name]] <- seq(1,nrow(newdata))
  #print(newdata[[col_name]])
  newdata <- add_column(newdata, fit = predict(mymodel, newdata = newdata, type = 'response'))
  # 
  # plt <- ggplot(newdata, aes(x = col_name, y = fit)) +
  #   geom_line() +
  #   #geom_rug(aes(y = visited, colour = lvisited), data = df) +
  #   labs(x = col_name, y = 'Probability of Error')
  # print(plt)
  # 
  
  ## grad the inverse link function #only exist in glm() functions, does not exist in fixest functions. 
  # ilink <- family(mymodel)$linkinv
  # #print(ilink)
  # #ilink<- pmax(exp(eta), .Machine$double.eps)
  # ## add fit and se.fit on the **link** scale

  #TODO What the hell.... se.fit does not work for negenbin() models. it is okay, 
    # newdata <- bind_cols(newdata, setNames(as_tibble(predict(mymodel, newdata, type = 'link', se.fit = TRUE)[1:2]),
  #                                        c('fit_link','se_link')))
  # ## create the interval and backtransform
  # newdata <- mutate(newdata,
  #                   fit_resp  = ilink(fit_link),
  #                   right_upr = ilink(fit_link + (2 * se_link)),
  #                   right_lwr = ilink(fit_link - (2 * se_link)))
  
  return(newdata)
}
######################
#Model 1, predicting the Manual count and Move_autocount plot 

# m2_nb<- glm.nb(Error_Count ~ Manual_count + MOVE_AUTO_count 
#             + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
#             + I(Manual_count^2) + I(MOVE_AUTO_count^2)
#             +DATE_EVENT+RAILWAY_DAYS+AGE
#             , data = df_hat
# )

#TODO: fix the ploting problem. 
# Using the glm function are hard to be able to converge. 
# To plot the graphs, one way to do it is to plot it using the fixest() pacakage. 
# I need to find a way to get the link function, and then plot it.

df_hat$DATE_EVENT <- as.numeric(as.character(df_hat$DATE_EVENT)) 
m2_nb <- glm.nb(Error_Count ~ Manual_count + MOVE_AUTO_count 
            + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
            + I(Manual_count^2) + I(MOVE_AUTO_count^2)
            #+DATE_EVENT+RAILWAY_DAYS
            , data = df_hat)
summary(m2_nb)
m2_qp <- glm(Error_Count ~ Manual_count + MOVE_AUTO_count 
               + PHONE + JUSTIF_count + MESSAGE_OTHER_count + Hour#Control Var
              # + I(Manual_count^2) + I(MOVE_AUTO_count^2)
               #+ DATE_EVENT+ RAILWAY_DAYS
               , family ="quasipoisson"
               , data = df_hat
)
summary(m2_qp)
max_var <- max(df_hat$Manual_count)

all_my_vars <- c(fixed_vars , my_independent_vars)
#print(all_my_vars)
predict.data <- setNames(data.frame(matrix(0, ncol = length(all_my_vars), nrow =1000)), all_my_vars)
#predict.data$DATE_EVENT <- as.character(predict.data$DATE_EVENT)
#predict.data$RSE_ID_ANONYM <- as.character(predict.data$RSE_ID_ANONYM)

predict.data<- predict_X(Manual_count, m4, predict.data )

dev.off()
#source("Plot.R", print.eval=TRUE)
#png(filename="MVE_MAN.png")

plt<- plotCI(predict.data, m4, Manual_count,"Manual Count")
ggsave("./APM_results/M2_Manual_count.png", width = 6, height = 4, dpi = 300, plot = plt)


max_var <- max(df_hat$MOVE_AUTO_count)
predict.data <- setNames(data.frame(matrix(0, ncol = 8, nrow =150)), c('Manual_count','MOVE_AUTO_count', 'JUSTIF_count','PHONE' 
                                                                        ,'MESSAGE_OTHER_count','DATE_EVENT','RSE_ID_ANONYM','Hour'))
#predict.data$DATE_EVENT <- as.character(predict.data$DATE_EVENT)
#predict.data$RSE_ID_ANONYM <- as.character(predict.data$RSE_ID_ANONYM)
predict.data<- predict_X(MOVE_AUTO_count, m2_qp, predict.data )
dev.off()
#source("Plot.R", print.eval=TRUE)
#png(filename="MVE_MAN.png")

plt<- plotCI(predict.data, m_qp, MOVE_AUTO_count,"Move Auto count")
ggsave("./APM_results/M2_MoveAuto.png", width = 6, height = 4, dpi = 300, plot = plt)


summary(m2_qp)

