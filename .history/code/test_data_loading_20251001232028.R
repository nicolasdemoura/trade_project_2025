###############################################################################
# Test Script: Validate CDK Data Loading and Cleaning
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

# Load required libraries
library(haven)
library(readxl)
library(dplyr)

# Test 1: Load and examine WIOD data
cat("=== TEST 1: Loading WIOD Data ===\n")

# Try to load the WIOD data
tryCatch({
  wiot_full <- read_dta("data/wiot_full.dta")
  cat("✓ Successfully loaded WIOD data\n")
  cat("Dimensions:", paste(dim(wiot_full), collapse = " x "), "\n")
  cat("Columns:", paste(colnames(wiot_full), collapse = ", "), "\n")
  
  # Check for expected columns
  expected_cols <- c("year", "row_country", "col_country", "row_item", "col_item", "value")
  missing_cols <- expected_cols[!expected_cols %in% colnames(wiot_full)]
  
  if (length(missing_cols) == 0) {
    cat("✓ All expected columns found\n")
  } else {
    cat("⚠ Missing columns:", paste(missing_cols, collapse = ", "), "\n")
  }
  
  # Sample the data
  cat("\nData sample (first 3 rows):\n")
  print(head(wiot_full, 3))
  
  # Check years available
  if ("year" %in% colnames(wiot_full)) {
    years <- unique(wiot_full$year)
    cat("\nAvailable years:", paste(sort(years), collapse = ", "), "\n")
    
    if (2011 %in% years) {
      cat("✓ Target year 2011 is available\n")
    } else {
      cat("⚠ Target year 2011 not found\n")
    }
  }
  
  # Check countries
  if ("row_country" %in% colnames(wiot_full)) {
    countries <- unique(c(wiot_full$row_country, wiot_full$col_country))
    cat("\nNumber of countries:", length(countries), "\n")
    cat("Sample countries:", paste(head(countries, 5), collapse = ", "), "...\n")
  }
  
  # Check sectors/items
  if ("row_item" %in% colnames(wiot_full)) {
    items <- unique(c(wiot_full$row_item, wiot_full$col_item))
    cat("\nNumber of items/sectors:", length(items), "\n")
    cat("Sample items:", paste(head(items, 5), collapse = ", "), "...\n")
  }
  
}, error = function(e) {
  cat("✗ Error loading WIOD data:", conditionMessage(e), "\n")
})

cat("\n" , rep("=", 50), "\n")

# Test 2: Load and examine Socio-Economic data
cat("=== TEST 2: Loading Socio-Economic Data ===\n")

tryCatch({
  socioeco <- read_excel("data/Socio_Economic_Accounts_July14.xlsx")
  cat("✓ Successfully loaded socio-economic data\n")
  cat("Dimensions:", paste(dim(socioeco), collapse = " x "), "\n")
  cat("Columns:", paste(colnames(socioeco), collapse = ", "), "\n")
  
  # Sample the data
  cat("\nData sample (first 3 rows):\n")
  print(head(socioeco, 3))
  
}, error = function(e) {
  cat("✗ Error loading socio-economic data:", conditionMessage(e), "\n")
})

cat("\n" , rep("=", 50), "\n")

# Test 3: Test data cleaning function (if data loaded successfully)
cat("=== TEST 3: Testing Data Cleaning Function ===\n")

tryCatch({
  # Source the functions
  source("functions.R")
  source("data_cleaning.R")
  
  # Test with small subset if full data is too large
  if (exists("wiot_full")) {
    cat("Testing with WIOD data subset...\n")
    
    # Take a small sample for testing
    wiot_sample <- wiot_full[sample(nrow(wiot_full), min(1000, nrow(wiot_full))), ]
    
    # Test the cleaning function
    cleaned_data <- clean_wiod_data_cdk(wiot_sample)
    
    cat("✓ Data cleaning function executed successfully\n")
    cat("Countries identified:", length(cleaned_data$countries), "\n")
    cat("Sectors identified:", length(cleaned_data$sectors), "\n")
    cat("Trade flows array dimensions:", paste(dim(cleaned_data$trade_flows_array), collapse = " x "), "\n")
    
  } else {
    cat("⚠ WIOD data not available for testing\n")
  }
  
}, error = function(e) {
  cat("✗ Error in data cleaning function:", conditionMessage(e), "\n")
})

cat("\n=== Test Complete ===\n")