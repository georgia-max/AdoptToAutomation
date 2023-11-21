source("utils/dataset_builder.R", encoding = "UTF-8")

library(vtable)

# Load Data

#Read APM dataset.
# df <- read_csv(
#     "APM_class2019_Aug_Sep_Oct_2019_only_TC.csv",
#     col_types = cols(.default = "c",
#         DELAY_PREV_SIG_avg = col_skip(),
#         DELAY_PREV_SIG_avg_abs_val = col_skip(), 
#         DELAY_PREV_SIG_PASS_ONLY_avg = col_skip(), 
#         DELAY_PREV_SIG_PASS_ONLY_avg_abs_val = col_skip(), 
#         YEAR_OF_BIRTH = col_skip(), 
#         FATIGUE_NEW_SHIFT_RISK = col_skip(), 
#         GENDER = col_skip(), 
#         FATIGUE_NEW_RISK_INDEX = col_skip(), 
#         FATIGUE_NEW_EXPOSURE = col_skip(),
#         DATE_EVENT = col_date(format = "%d/%m/%Y"))
#         )

# Read CRIPTON dataset.
df_raw <- read_csv(
    "data/CRIPTON_60_MINUTES_anonymized_all.csv",
    col_types = cols(.default = "c",
    DATE_EVENT = col_date(format = "%Y-%m-%d")
    )
)

# # Specify the columns to transform
# cols_to_transform <- c("MOVE_AUTO_count", "MOVE_MAN_count", "c")

# Transform selected columns to numeric
# my_data_numeric <- df %>% 
#   mutate_at(vars(cols_to_transform), function(x) {
#     #x <- as.numeric(x)
#     ifelse(is.na(x), NA, x)
#   })

# Print the transformed data frame
# print(my_data_numeric)        

df <- df_raw %>%
  mutate(
         MOVE_ALL_count = as.numeric(gsub(",", "", MOVE_ALL_count), na.rm = TRUE),
         MOVE_AUTO_count = as.numeric(gsub(",", "", MOVE_AUTO_count), na.rm = TRUE), # nolint
         MOVE_MAN_count = as.numeric(MOVE_MAN_count, na.rm = TRUE),
         ADAPTATIONS_count = as.numeric(ADAPTATIONS_count, na.rm = TRUE),
         CHANGE_AUTO_AA_count = as.numeric(CHANGE_AUTO_AA_count, na.rm = TRUE),
         CHANGE_AUTO_WW_count = as.numeric(CHANGE_AUTO_WW_count, na.rm = TRUE),
         PHONE_IN_count = as.numeric(PHONE_IN_count, na.rm = TRUE),
         PHONE_OUT_count = as.numeric(PHONE_OUT_count, na.rm = TRUE),
         MESSAGE_OTHER_count = as.numeric(MESSAGE_OTHER_count, na.rm = TRUE),
         MESSAGE_HUMAN_ERROR_count = as.numeric(MESSAGE_HUMAN_ERROR_count),# na.rm = TRUE),
         JUSTIF_count = as.numeric(JUSTIF_count, na.rm = TRUE),
         #TRAF_DENS = as.numeric(gsub(",", "", TRAF_DENS)), #na.rm = TRUE),
         TRAF_COMP = as.numeric(gsub(",", "", TRAF_COMP)),# na.rm = TRUE),
         #RAILWAY_DAYS = as.numeric(gsub(",", "", RAILWAY_DAYS), na.rm = TRUE),
         #EXPERIENCE_DAYS = as.numeric(gsub(",", "", EXPERIENCE_DAYS), na.rm = TRUE),
         #AGE = as.numeric(gsub(",", "", AGE), na.rm = TRUE)
         )

# find row indices with NAs
# na_rows <- which(apply(is.na(df), 1, any))
# # print rows with NAs
# print(df[na_rows,])

#Exclude Weekends and night shifts.
df <- exclude_weekend_night(df)

df <- change_column_names(df, c("MESSAGE_HUMAN_ERROR_count", "HOUR_EVENT"), 
                          c("Error_Count", "Hour"))
#### Manipulate the variables.
df$Date_hour <- df$DATE_EVENT + hours(df$Hour)

df$PHONE = df$PHONE_IN_count + df$PHONE_OUT_count
# df$TotalWL <-  df$MOVE_MAN_count + df$CHANGE_AUTO_AA_count +
#     df$CHANGE_AUTO_WW_count + df$MOVE_AUTO_count  +   df$ADAPTATIONS_count

df$TotalWL <-  df$MOVE_MAN_count + df$MOVE_AUTO_count 


df$Manual_count <- df$MOVE_MAN_count + df$CHANGE_AUTO_AA_count +
     df$CHANGE_AUTO_WW_count + df$ADAPTATIONS_count



df$Month <-  as.numeric(format(df$DATE_EVENT, format = "%m"))
df$Year <- as.numeric(format(df$DATE_EVENT, format = "%Y"))
df$Day <- as.numeric(format(df$DATE_EVENT, format = "%d"))
# df$timeorder <- as.numeric(format(df$DATE_EVENT(format = "%y-%m-%d"))*100 + df$HOUR_EVENT)

df$Manual_count_sq <- df$Manual_count^2
df$MOVE_AUTO_count_sq <- df$MOVE_ALL_count^2

#df[is.na(df)] <- 0


#print dataframe class
print(sapply(df, class))

# Filter rows with NA in any column
df <- df[complete.cases(df), ]
print(describe(df))

df$Manual_fraction <- df$MOVE_MAN_count / df$TotalWL
df$Auto_fraction <- df$MOVE_AUTO_count / df$TotalWL

df$Error_rate = df$Error_Count/ df$TotalWL
df$Error_rate_thousand = df$Error_Count/ df$TotalWL *1000

df$Manual_fraction_sq = df$Manual_fraction^2
df$Auto_fraction_sq = df$Auto_fraction^2
df$TotalWL_sq = df$TotalWL^2


#Delete Rows if MOVE_ALL = 0 & ERROR != 0 #Since we view this as outliers.
df <- df %>% filter(!(TotalWL == 0 & Error_Count != 0 ))

df <- df %>% filter(!(TotalWL == 0))
df <- df %>% filter((Error_rate <=1))

#df <- df %>% filter((TotalWL <207 ))


# Subet when Auto_Fraction = 0. Means that no one has used ADAS. 
# df_sub <- subset(df_sub, df$Auto_fraction != 0)


# TotalWL
Q <- quantile(df$TotalWL, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(df$TotalWL)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range


data_no_outlier<- subset(df, df$TotalWL > low & df$TotalWL < up)


df <- data_no_outlier

p <- ggplot(data = df, aes(x = "", y = TotalWL)) + 
  geom_boxplot() #+
#coord_cartesian(ylim = c(0, 150)) 
ggplotly(p)


#Correlation plot 
########
library("xlsx")
df2 <- subset(df, select = c('Error_rate','Auto_fraction',"TotalWL","PHONE","MESSAGE_OTHER_count", "Hour"
                             ,"JUSTIF_count",
                             'ADAPTATIONS_count','TRAF_COMP'))

vcorr <- round(cor(df2),3)

# write.xlsx(vcorr, "./Event_Based_results/Addition_manual_corr_050523.xlsx")





# 
# 
# # Manual Count 
# Q <- quantile(df$Manual_count, probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(df$Manual_count)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# eliminated<- subset(df, df$Manual_count > (Q[1] - 1.5*iqr) & df$Manual_count < (Q[2]+1.5*iqr))
# 
# p <- ggplot(data = eliminated, aes(x = "", y = Manual_count)) + 
#   geom_boxplot() #+
#   #coord_cartesian(ylim = c(0, 150)) 
# #ggplotly(p)
# 
# # Move Auto Count 
# Q <- quantile(eliminated$MOVE_AUTO_count, probs=c(.25, .75), na.rm = FALSE)
# iqr <- IQR(eliminated$MOVE_AUTO_count)
# up <-  Q[2]+1.5*iqr # Upper Range  
# low<- Q[1]-1.5*iqr # Lower Range
# eliminated2<- subset(eliminated, eliminated$MOVE_AUTO_count > (Q[1] - 1.5*iqr) &eliminated$MOVE_AUTO_count < (Q[2]+1.5*iqr))
# 
# p <- ggplot(data = eliminated2, aes(x = "", y = MOVE_AUTO_count)) + 
#   geom_boxplot() #+
#   #coord_cartesian(ylim = c(0, 150)) 
# #ggplotly(p)
# 
# nrow(eliminated2)
# 
# df <- eliminated2
# # Reset index 
# rownames(df)<- NULL
# 



