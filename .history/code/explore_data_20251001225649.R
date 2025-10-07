###############################################################################
# Data Structure Exploration for WIOD 2011 Dataset
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

# Load required libraries
library(haven)
library(readxl)
library(dplyr)

###############################################################################
# Examine WIOD Full Dataset Structure
###############################################################################

# Load the data
wiot_full <- read_dta("data/wiot_full.dta")
socioeco <- read_excel("data/Socio_Economic_Accounts_July14.xlsx")

cat("=== DETAILED WIOD STRUCTURE EXPLORATION ===\n\n")

# Basic info
cat("Dataset dimensions:", paste(dim(wiot_full), collapse = " x "), "\n")
cat("Total observations:", nrow(wiot_full), "\n")
cat("Total variables:", ncol(wiot_full), "\n\n")

# Variable names and types
cat("Variable names and types:\n")
for (i in 1:ncol(wiot_full)) {
  cat(sprintf("%-3d %-20s %s\n", i, colnames(wiot_full)[i], class(wiot_full[[i]])))
}

cat("\n=== EXPLORING KEY VARIABLES ===\n")

# Look for country identifiers
country_vars <- colnames(wiot_full)[grepl("country|origin|destination|from|to", colnames(wiot_full), ignore.case = TRUE)]
if (length(country_vars) > 0) {
  cat("\nPossible country variables:\n")
  for (var in country_vars) {
    cat("Variable:", var, "\n")
    cat("Unique values:", length(unique(wiot_full[[var]])), "\n")
    cat("Sample values:", paste(head(unique(wiot_full[[var]]), 10), collapse = ", "), "\n\n")
  }
}

# Look for sector identifiers  
sector_vars <- colnames(wiot_full)[grepl("sector|industry|code", colnames(wiot_full), ignore.case = TRUE)]
if (length(sector_vars) > 0) {
  cat("Possible sector variables:\n")
  for (var in sector_vars) {
    cat("Variable:", var, "\n")
    cat("Unique values:", length(unique(wiot_full[[var]])), "\n") 
    cat("Sample values:", paste(head(unique(wiot_full[[var]]), 10), collapse = ", "), "\n\n")
  }
}

# Look for year variables
year_vars <- colnames(wiot_full)[grepl("year|time|date", colnames(wiot_full), ignore.case = TRUE)]
if (length(year_vars) > 0) {
  cat("Possible year variables:\n")
  for (var in year_vars) {
    cat("Variable:", var, "\n")
    cat("Unique values:", paste(sort(unique(wiot_full[[var]])), collapse = ", "), "\n\n")
  }
}

# Look for value/flow variables
value_vars <- colnames(wiot_full)[grepl("value|flow|export|import|output|trade", colnames(wiot_full), ignore.case = TRUE)]
if (length(value_vars) > 0) {
  cat("Possible value/flow variables:\n")
  for (var in value_vars) {
    cat("Variable:", var, "\n")
    cat("Type:", class(wiot_full[[var]]), "\n")
    if (is.numeric(wiot_full[[var]])) {
      cat("Range:", round(range(wiot_full[[var]], na.rm = TRUE), 2), "\n")
      cat("Mean:", round(mean(wiot_full[[var]], na.rm = TRUE), 2), "\n")
    }
    cat("Missing values:", sum(is.na(wiot_full[[var]])), "\n\n")
  }
}

cat("=== SAMPLE OF FIRST 10 OBSERVATIONS ===\n")
print(head(wiot_full, 10))

cat("\n=== SOCIO-ECONOMIC ACCOUNTS STRUCTURE ===\n\n")

# Basic info
cat("Dataset dimensions:", paste(dim(socioeco), collapse = " x "), "\n")
cat("Total observations:", nrow(socioeco), "\n")
cat("Total variables:", ncol(socioeco), "\n\n")

# Variable names and types
cat("Variable names and types:\n")
for (i in 1:ncol(socioeco)) {
  cat(sprintf("%-3d %-30s %s\n", i, colnames(socioeco)[i], class(socioeco[[i]])))
}

# Look for employment/labor variables
labor_vars <- colnames(socioeco)[grepl("emp|labor|labour|wage|work|person|hour", colnames(socioeco), ignore.case = TRUE)]
if (length(labor_vars) > 0) {
  cat("\nLabor-related variables:\n")
  for (var in labor_vars) {
    cat("Variable:", var, "\n")
    if (is.numeric(socioeco[[var]])) {
      cat("Range:", range(socioeco[[var]], na.rm = TRUE), "\n")
      cat("Missing:", sum(is.na(socioeco[[var]])), "\n")
    } else {
      cat("Type:", class(socioeco[[var]]), "\n")
    }
    cat("\n")
  }
}

# Look for country/region variables
country_vars_socio <- colnames(socioeco)[grepl("country|region|code", colnames(socioeco), ignore.case = TRUE)]
if (length(country_vars_socio) > 0) {
  cat("Country/region variables:\n")
  for (var in country_vars_socio) {
    cat("Variable:", var, "\n")
    cat("Unique values:", length(unique(socioeco[[var]])), "\n")
    cat("Sample:", paste(head(unique(socioeco[[var]]), 10), collapse = ", "), "\n\n")
  }
}

cat("Sample of first 5 observations from socio-economic data:\n")
print(head(socioeco, 5))

cat("\n=== DATA STRUCTURE ANALYSIS COMPLETE ===\n")
cat("Based on this exploration, update the data_cleaning.R functions accordingly.\n")