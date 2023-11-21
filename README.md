# AdoptToAutomation
This repository contains the R code for Adopt to Automation Paper that suppose to publish to MSOM. 


## Getting Started

The clean code is in the [src](https://github.com/georgia-max/AdoptToAutomation/tree/master/src) folder. 


## Prerequisites

Before you begin, ensure you have met the following requirements:

* You have installed [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/).
* You are familiar with basic R operations and package management.

This project uses `renv` for dependency management. The `renv` package ensures that this project runs with the correct versions of R packages, making it easier to manage project dependencies and share the project with others.

## Setting Up the Project

To set up the project on your local machine:

1. **Clone the Repository:**

   First, clone this repository to your local machine:

   ```bash
   git clone https://github.com/georgia-max/AdoptToAutomation.git
   ```

   Second, install 'renv'. If you haven't already installed renv, you can do so by running the following in your R console:

   ```
   install.packages("renv")
   ```

   Third, restore dependencies, Navigate to the project directory and run the following in your R console:
   ```
   renv::restore()
   ```
   This command will install all the necessary R packages as specified in the renv.lock file, ensuring you have the correct package versions required for this project.


## Using the Project
   
The clean code is in the [src](https://github.com/georgia-max/AdoptToAutomation/tree/master/src) folder. 
The [temp](https://github.com/georgia-max/AdoptToAutomation/tree/master/temp) folder contains temporary code that I need to clean up. 

The Dataset is stored in the Dataset folder. 

First, run the data_prepare.R file to get the preprocessed dataset. 
Second, run the main.R file to get the regression results. 

## TODO

1. ADD analysis to account for endogeneity issue, see file *MSOM_Decision*
2. Clean up Main.R

## Notes: 
1. Currently, the data anlaysis is performed in this dataset: **CRIPTON_60_MINUTES_anonymized_all.csv** 
