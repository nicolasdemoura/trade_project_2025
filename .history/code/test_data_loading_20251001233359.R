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
  wiot_full <- read_dta("code/data/wiot_full.dta")
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
  # Try loading both sheets
  socioeco_dict <- read_excel("data/Socio_Economic_Accounts_July14.xlsx", sheet = 1)
  socioeco_data <- read_excel("data/Socio_Economic_Accounts_July14.xlsx", sheet = "DATA")
  
  cat("✓ Successfully loaded socio-economic data\n")
  cat("Dictionary sheet dimensions:", paste(dim(socioeco_dict), collapse = " x "), "\n")
  cat("Data sheet dimensions:", paste(dim(socioeco_data), collapse = " x "), "\n")
  cat("Data columns:", paste(colnames(socioeco_data), collapse = ", "), "\n")
  
  # Sample the data
  cat("\nData sample (first 3 rows):\n")
  print(head(socioeco_data, 3))
  
  # Check for 2011 data
  if ("_2011" %in% colnames(socioeco_data)) {
    countries_2011 <- socioeco_data %>%
      filter(!is.na(`_2011`)) %>%
      pull(Country) %>%
      unique()
    cat("\n✓ Found", length(countries_2011), "countries with 2011 data\n")
    cat("Sample countries with 2011 data:", paste(head(countries_2011, 5), collapse = ", "), "\n")
  } else {
    cat("\n⚠ Column '_2011' not found in socio-economic data\n")
  }
  
  # Check for key variables
  key_vars <- c("EMP", "EMPE", "LAB", "COMP", "GO", "VA")
  available_vars <- intersect(key_vars, unique(socioeco_data$Variable))
  cat("Key variables available:", paste(available_vars, collapse = ", "), "\n")
  
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
    
    # Take a small sample for testing (but ensure we have 2011 data)
    wiot_2011_sample <- wiot_full[wiot_full$year == 2011, ]
    if (nrow(wiot_2011_sample) > 5000) {
      wiot_2011_sample <- wiot_2011_sample[sample(nrow(wiot_2011_sample), 5000), ]
    }
    
    # Test the individual cleaning function
    cleaned_wiod <- clean_wiod_data_cdk(wiot_2011_sample)
    
    cat("✓ WIOD cleaning function executed successfully\n")
    cat("Countries identified:", length(cleaned_wiod$countries), "\n")
    cat("Sectors identified:", length(cleaned_wiod$sectors), "\n")
    cat("Trade flows array dimensions:", paste(dim(cleaned_wiod$trade_flows_array), collapse = " x "), "\n")
    
    # Test the full cleaning workflow if socioeconomic data is available
    if (file.exists("data/Socio_Economic_Accounts_July14.xlsx")) {
      cat("Testing complete data cleaning workflow...\n")
      cleaned_data <- clean_all_data_cdk(wiot_2011_sample, "data/Socio_Economic_Accounts_July14.xlsx", 2011)
      
      if (!is.null(cleaned_data)) {
        cat("✓ Complete CDK data cleaning executed successfully\n")
        cat("Final countries with complete data:", length(cleaned_data$countries), "\n")
        cat("Countries:", paste(cleaned_data$countries, collapse = ", "), "\n")
        cat("Employment data rows:", nrow(cleaned_data$employment_data), "\n")
      } else {
        cat("⚠ Complete data cleaning returned NULL\n")
      }
    } else {
      cat("⚠ Socio-economic data file not found for complete workflow test\n")
    }
    
  } else {
    cat("⚠ WIOD data not available for testing\n")
  }
  
}, error = function(e) {
  cat("✗ Error in data cleaning function:", conditionMessage(e), "\n")
})

cat("\n=== Test Complete ===\n")