# basic histogram
library(plotly)
library(PerformanceAnalytics)

p <- ggplot(data_no_outlier, aes(x=Error_Count)) + 
  geom_histogram(bins = 50)+
  theme_bw()+
  labs(x = "Number of Errors",y = 'Count (Thousand)') + 
  scale_y_continuous(name = "Count (Thousand)", labels = c("0","100",'200',"300","400")
                  )
ggplotly(p)
ggsave(filename = "Event_Based_results/Error_Count_APM.png", width = 6, height = 4, dpi = 300, plot = p)

p <- ggplot(data_no_outlier, aes(x=Error_rate)) + 
  geom_histogram(bins = 50) + 
  theme_bw()+
  labs(x = "Error Rate",y = 'Count (Thousand)') +
  scale_y_continuous(name = "Count (Thousand)", labels = c("0","100",'200',"300","400")
  )
ggplotly(p)
ggsave(filename = "Event_Based_results/Error_Rate_APM.png", width = 6, height = 4, dpi = 300, plot = p)


p <- ggplot(df,aes(x=TotalWL)) + 
  geom_histogram(bins = 30)+
  theme_bw()+
  labs(x = "Signaling workload", y ='Count')
  #scale_y_continuous(name = "Count (Thousand)", labels = c("0","100",'200',"300","400"))

  ggplotly(p)
ggsave(filename = "Event_Based_results/Total_WL.png", width = 6, height = 4, dpi = 300, plot = p)

p <- ggplot(df, aes(x=Manual_count)) + 
  geom_histogram(bins = 30)+
  theme_bw()+
  labs(x = "Manual_count",y = 'Count')
ggplotly(p)
#ggsave(filename = "APM_results/Total_APM.png", width = 6, height = 4, dpi = 300, plot = p)



p <- ggplot(data_no_outlier, aes(x=Auto_fraction)) + 
  geom_histogram(bins = 30)+
  theme_bw()+
  labs(x = "Automaton usage (%)",y = 'Count')
ggplotly(p)
ggsave(filename = "Event_Based_results/Auto_fraction.png", width = 6, height = 4, dpi = 300, plot = p)

# 
# 
# p <- ggplot(df, aes(x=MOVE_AUTO_count)) + 
#   geom_histogram(bins = 30)+
#   theme_bw()+
#   labs(x = "Monitoring Workload",y = 'Count')
# ggplotly(p)
# #ggsave(filename = "MOVE_AUTO_APM.png", width = 6, height = 4, dpi = 300, plot = p)
# 
# p <- ggplot(df, aes(x=MOVE_MAN_count)) + 
#   geom_histogram(bins = 30)+
#   theme_bw()+
#   labs(x = "Manual Workload",y = 'Count')
# #ggsave(filename = "MOVE_MAN.png", width = 6, height = 4, dpi = 300, plot = p)
# 
# p <- ggplot(df, aes(x=CHANGE_AUTO_AA_count)) + 
#   geom_histogram(bins = 30)+
#   theme_bw()+
#   labs(x = "SWAuto",y = 'Count')
# #ggsave(filename = "SWAuto.png", width = 6, height = 4, dpi = 300, plot = p)
# 
# p <- ggplot(df, aes(x=CHANGE_AUTO_WW_count)) + 
#   geom_histogram(bins = 30)+
#   theme_bw()+
#   labs(x = "SwMan",y = 'Count')
# #ggsave(filename = "SwMan.png", width = 6, height = 4, dpi = 300, plot = p)
# df3 <- subset(eliminated2, select = c('Error_Count','MOVE_MAN_count','MOVE_ALL_count','Manual_count','MOVE_AUTO_count','Auto_fraction',
#                             'PHONE','JUSTIF_count','MESSAGE_OTHER_count'))


#chart.Correlation(df3, histogram=TRUE, pch=10)
#ggsave(filename = "APM_results/pairplot.png", width = 6, height = 4, dpi = 300, plot = p)


# Plot
# df3 %>%
#   ggplot( aes(x=name, y=value, fill=name)) +
#     geom_boxplot() +

# Melt the dataframe to convert it from wide to long format
mydata_melted <- reshape2::melt(df3, measure.vars = names(df3))


# Create a boxplot using ggplot2
p <- ggplot(mydata_melted, aes(variable, value)) +
  geom_boxplot() +
  ggtitle("Boxplot of All Numerical Columns in mydata")
ggplotly(p)
