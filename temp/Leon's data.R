
#Import Leon's dataset 

library('lintr')
df_event <- read.csv("~/Desktop/Workload&ErrorPaper/Event Data_Leon_20230116/training_set_02_2022_group_level.csv", )
df_event<- subset(df_event, select = -c(X),)
df_leon<- df_event[c(1:41)]
summary(df_leon)

df2 <- subset(df_leon, select = c('ERROR','workload',
                             'active_workload', 'automation', 'manual_move','automation_conflicts','ACTIVE_EVENT','aut','experience'))
#Remame columns 
df_leon <- df_leon %>% 
  rename("Error_Count" = "ERROR",
         "Manual_count"= "active_workload" ,
         "MOVE_AUTO_count"= "automation" , 
         "Hour"= "HOUR_EVENT",
         "EXPERIENCE_DAYS"= "experience", 
         "RSE_ID_ANONYM"= "RSE_ID")
#This mapping is wrong. Need to wait for Leon's reply for what vars represents manual/monitor WL

#Correlation Matrix
corr <- round(cor(df2),3)
corr 
library("PerformanceAnalytics")

chart.Correlation(df2, histogram=TRUE, pch=19)

######## 
df<- df_leon
# m1= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count+ CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
#                PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour
#              | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
#              ,data = df)

m2= fenegbin(Error_Count ~Manual_count +MOVE_AUTO_count+
              # PHONE+ JUSTIF_count + MESSAGE_OTHER_count + 
               Hour
             | DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway"
             ,data = df)
# m3= fenegbin(Error_Count ~ MOVE_MAN_count + MOVE_AUTO_count +CHANGE_AUTO_AA_count + CHANGE_AUTO_WW_count+
#                PHONE+ JUSTIF_count + MESSAGE_OTHER_count + Hour  + csw(I(MOVE_MAN_count^2) + I(MOVE_AUTO_count^2)+ I(CHANGE_AUTO_AA_count^2)+ I(CHANGE_AUTO_WW_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
# )


m4= fenegbin(Error_Count ~ Manual_count + MOVE_AUTO_count +
             #PHONE+ JUSTIF_count + MESSAGE_OTHER_count + 
               Hour+
               csw(I(Manual_count^2) + I(MOVE_AUTO_count^2))-1| DATE_EVENT+RSE_ID_ANONYM,vcov = "twoway", data = df
)

#etable(m1,m2,m3,m4)
etable(m2,m4)