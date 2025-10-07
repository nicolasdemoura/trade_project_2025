###############################################################################
# Topic: Estimating Trump's Tariff Impact on Global Welfare
# Goal: Assess the economic impact of Trump's tariffs on global welfare
# Keywords: Tariffs, Global Welfare, Trade Policy, International Trade
# Autor: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

###############################################################################
# Organize the working environment
###############################################################################

# Clean the working environment
rm(list = ls())
load.lib <- c("dplyr", "ggplot2", "stargazer",
              "readxl", "tidyverse", "data.table", "lubridate", "fixest", "pracma",
              "remotes", "tidyr", "nprobust", "chron", "haven", "readr",
              "writexl", "modelsummary","lmtest", "progress")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib, dependencies = TRUE)
sapply(load.lib, require, character = TRUE)
rm(load.lib, install.lib, lib)
gc()

# Set the random seed for reproducibility
set.seed(20251001)

# Source the functions

# Source all required functions
source("code/functions.R")
source("code/data_cleaning.R")
source("code/estimation.R")
source("code/counterfactual.R")

###############################################################################
# Load the data
###############################################################################

# Load wiot_full.dta
wiot_full <- read_dta("code/data/wiot_full.dta")

# Load Socio_Economic_Accounts_July14.xlsx
socioeco <- read_excel("code/data/Socio_Economic_Accounts_July14.xlsx")

###############################################################################
# Explore the data structure
###############################################################################

cat("=== WIOD Full Data Structure ===\n")
cat("Dimensions:", paste(dim(wiot_full), collapse = " x "), "\n")
cat("Column names:\n")
print(colnames(wiot_full))
cat("\nFirst few observations:\n")
print(head(wiot_full, 3))

# Look for key identifying variables
if ("Country" %in% colnames(wiot_full)) {
  cat("\nUnique countries in WIOD:\n")
  print(sort(unique(wiot_full$Country)))
}

if ("IndustryCode" %in% colnames(wiot_full) | "Sector" %in% colnames(wiot_full)) {
  sector_col <- ifelse("IndustryCode" %in% colnames(wiot_full), "IndustryCode", "Sector")
  cat("\nUnique sectors in WIOD:\n") 
  print(sort(unique(wiot_full[[sector_col]])))
}

if ("Year" %in% colnames(wiot_full)) {
  cat("\nAvailable years in WIOD:\n")
  print(sort(unique(wiot_full$Year)))
}

cat("\n=== Socio-Economic Accounts Structure ===\n")
cat("Dimensions:", paste(dim(socioeco), collapse = " x "), "\n")
cat("Column names:\n")
print(colnames(socioeco))
cat("\nFirst few observations:\n")
print(head(socioeco, 3))

# Look for labor-related variables
labor_cols <- colnames(socioeco)[grepl("emp|labor|labour|wage", colnames(socioeco), ignore.case = TRUE)]
if (length(labor_cols) > 0) {
  cat("\nLabor-related columns found:\n")
  print(labor_cols)
}

# Look for countries and sectors in socioeco
if ("Country" %in% colnames(socioeco)) {
  cat("\nUnique countries in Socio-Economic data:\n")
  print(sort(unique(socioeco$Country)))
}
