#################################################################################################
### SET PARAMETERS
USE_THESE_PACKAGES <- c(
 "sandwich","MASS","tidyverse","Hmisc","jtools",
  "dplyr", "ggplot2", "data.table", "lubridate", 
                        "stringr", "forecast",
                        "fixest","MASS", "pglm"
                        ,"plm", "lmtest", "fixest",
                        "lmtest", "ivreg", "car","plotly", "plm")
GET_LATEST_DATA <- TRUE 
SAVE_OUTPUT = TRUE

#################################################################################################
### IMPORT PACKAGES
new_packages <- USE_THESE_PACKAGES[!(USE_THESE_PACKAGES %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages)
  sapply(USE_THESE_PACKAGES, require, character.only = TRUE)
} else {
  sapply(USE_THESE_PACKAGES, require, character.only = TRUE)
}


exclude_weekend_night <- function(df) {
    ### Exlclude Weekend and NightShift.
    #Leave out night shift.
    df$HOUR_EVENT <- as.numeric(df$HOUR_EVENT)
    df <- subset(df, df$HOUR_EVENT >= 6 & df$HOUR_EVENT < 22)

    #Leave out weekends.
    #Exclude weekends 
    x <- df$DATE_EVENT[!weekdays(df$DATE_EVENT) 
    %in% c("Saturday","Sunday")]

    df <- subset(df, DATE_EVENT %in% x)
    return(df)
}

change_column_names <- function(data, old_names, new_names) {
  # Convert the data to a data frame, if it isn't already
  data <- as.data.frame(data)
  # Check that the old_names and new_names vectors have the same length
  if (length(old_names) != length(new_names)) {
    stop("The old_names and new_names vectors must have the same length.")
  }
  # Change the column names
  colnames(data)[match(old_names, colnames(data))] <- new_names
  # Return the modified data frame
  return(data)
}

create_regression_formula <- function(dependent_var, independent_vars, fixed_effect_vars = NULL) {
  # Start with the dependent variable
  formula <- paste(dependent_var, "~")
  # Add the independent variables
  formula <- paste(formula, paste(independent_vars, collapse = "+"))
  # Add the fixed effect variables, if any
  if (!is.null(fixed_effect_vars)) {
    formula <- paste(formula, "|", paste(fixed_effect_vars, collapse = "+"))
  }
  # Return the formula
  formula <- as.formula(paste(formula, sep = "", collapse = " "))


  return(formula)
}


