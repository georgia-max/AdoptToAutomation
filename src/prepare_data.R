# Last update: 11/21/2023
# Author: Georgia Liu 
# This code is meant to clean up the dataset and run the the statistical analysis. 
# The dataset used in the R code is CRIPTON_60_MINUTES_anonymized_all.csv, which is documented 
# The data set contains 410, 269 controller-hour observations that cover a whole national railway network for 22 months
# (from June 2018 to March 2020) and 872 unique controllers.
# This same dataset is used in Peter Madsen's paper. 

# Load libraries
library(vtable)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(xlsx)

# Load utilities
source("utils/dataset_builder.R", encoding = "UTF-8")

# Load Data
cripton_data_path <- "data/CRIPTON_60_MINUTES_anonymized_all.csv"
cripton_data_raw <- read_csv(
  cripton_data_path,
  col_types = cols(.default = "c", DATE_EVENT = col_date(format = "%Y-%m-%d"))
)

# Data Transformation --------------------------------
# Convert selected columns to numeric
transform_columns <- function(df, columns) {
  df %>%
    mutate(across(all_of(columns), ~as.numeric(gsub(",", "", .x), na.rm = TRUE)))
}

numeric_columns <- c("MOVE_ALL_count", "MOVE_AUTO_count", "MOVE_MAN_count",
                     "ADAPTATIONS_count", "CHANGE_AUTO_AA_count", "CHANGE_AUTO_WW_count",
                     "PHONE_IN_count", "PHONE_OUT_count", "MESSAGE_OTHER_count", 
                     "MESSAGE_HUMAN_ERROR_count", "JUSTIF_count", 'TRAF_COMP', 'TRAF_DENS_1')

df <- transform_columns(cripton_data_raw, numeric_columns)

#Exclude weekends and night shifts
df <- exclude_weekend_night(df)

# Change Column Names 
df <- change_column_names(df, c("MESSAGE_HUMAN_ERROR_count", "HOUR_EVENT"), 
                          c("Error_Count", "Hour"))
# Add new columns
df$Date_hour <- df$DATE_EVENT + hours(df$Hour)
df$PHONE = df$PHONE_IN_count + df$PHONE_OUT_count
df$TotalWL <-  df$MOVE_MAN_count + df$MOVE_AUTO_count 
df$Manual_count <- df$MOVE_MAN_count + df$CHANGE_AUTO_AA_count + df$CHANGE_AUTO_WW_count + df$ADAPTATIONS_count

df$Month <-  as.numeric(format(df$DATE_EVENT, format = "%m"))
df$Year <- as.numeric(format(df$DATE_EVENT, format = "%Y"))
df$Day <- as.numeric(format(df$DATE_EVENT, format = "%d"))

df$Manual_count_sq <- df$Manual_count^2
df$MOVE_AUTO_count_sq <- df$MOVE_ALL_count^2

df$Manual_fraction <- df$MOVE_MAN_count / df$TotalWL
df$Auto_fraction <- df$MOVE_AUTO_count / df$TotalWL

df$Error_rate = df$Error_Count/ df$TotalWL
df$Error_rate_thousand = df$Error_Count/ df$TotalWL *1000

df$Manual_fraction_sq = df$Manual_fraction^2
df$Auto_fraction_sq = df$Auto_fraction^2
df$TotalWL_sq = df$TotalWL^2

# Add hours of worked based on shift 
df <- df %>%
  mutate(
    shift = ifelse(Hour >= 6 & Hour < 13, "morning", "afternoon"),
    hours_worked = ifelse(shift == "morning", pmin(Hour, 13) - 6, pmin(Hour, 21) - 13)
  )

# Filter rows with NA in any column
df <- df[complete.cases(df), ]

# Check if dataset have NA  
has_na <- any(is.na(df))

# Output whether NA exists in the dataset
if (has_na) {
  print("There are NA values in the dataset")
} else {
  print("There are no NA values in the dataset")
}

# Delete Rows if MOVE_ALL = 0 & ERROR != 0 (viewed as outliers)
df <- df %>%
  filter(!(TotalWL == 0 & Error_Count != 0)) %>%
  filter(!(TotalWL == 0)) %>%
  filter(Error_rate <= 1)

# Calculate IQR and filter out outliers in TotalWL
Q <- quantile(df$TotalWL, probs = c(.25, .75), na.rm = FALSE)
iqr <- IQR(df$TotalWL)
up <- Q[2] + 1.5 * iqr  # Upper Range
low <- Q[1] - 1.5 * iqr # Lower Range
df <- subset(df, TotalWL > low & TotalWL < up)


# Write df to Excel file (commented out)
write.csv(df, "./data/preprocess_data/CRIPTON_60_MINUTES_anonymized_all_prepared.csv", row.names = TRUE)


# Data Analysis --------------------------------
# Boxplot for workload 
p <- ggplot(data = df, aes(x = "", y = TotalWL)) +
  geom_boxplot()
print(p)

# Correlation plot
df2 <- df %>% 
  select(Error_rate, Error_Count, Auto_fraction, TotalWL, PHONE, MESSAGE_OTHER_count, 
         Hour, JUSTIF_count, ADAPTATIONS_count, TRAF_COMP, TRAF_DENS_1)

vcorr <- round(cor(df2, use = "complete.obs"), 3)
print(vcorr)

# Write correlation matrix to Excel file (commented out)
# write.csv(vcorr, "./data/preprocess_data/CRIPTON_60_MINUTES_anonymized_all_corr.csv", row.names = TRUE)


# Commented out code for future reference --------------------------------
# df <- df %>% filter(TotalWL < 207)
# Subset when Auto_Fraction = 0 (no one has used ADAS)
# df_sub <- subset(df, Auto_fraction != 0)

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


#Print data frame class
print(sapply(df, class))

