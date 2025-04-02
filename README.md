# The Relationships among Workload, Automation Reliance, and Human Errors in Safety-Critical Monitoring Roles

[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Python](https://img.shields.io/badge/python-3.11%2B-blue.svg)](https://www.python.org/downloads/)


This repository contains the R code for our study on the interplay between human workload, automation usage, and error rates in safety-critical socio-technical systems. Despite the increasing use of automation to reduce human workload, human error remains a major contributor to operational failures—especially in high-stakes environments like railway traffic control.

- Link to paper: https://www.sciencedirect.com/science/article/pii/S0925753524003655?via%3Dihub
- Citation: Liu N., Triantis, K., Madsen, P., & Roets, B. (2025). The Relationships among Workload, Automation Reliance, and Human Errors in Safety-Critical Monitoring Roles. Safety Science, 185, 106775. https://doi.org/10.1016/j.ssci.2024.106775![image](https://github.com/user-attachments/assets/7dd660c2-5415-47cd-b87e-b8925b3219bd)



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
   
First, run the data_prepare.R file to get the preprocessed dataset. 
Second, run the main.R file to get the regression results. 


## Notes: 
1. Currently, the data anlaysis is performed in this dataset: **CRIPTON_60_MINUTES_anonymized_all.csv** 
