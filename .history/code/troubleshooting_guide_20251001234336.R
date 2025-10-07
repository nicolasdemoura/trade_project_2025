###############################################################################
# CDK Analysis Troubleshooting Guide
# Quick reference for common issues and solutions
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

# TROUBLESHOOTING GUIDE
# =====================

# ISSUE 1: "Cannot find function 'clean_all_data_cdk'"
# SOLUTION: Make sure you're running from the /code directory
# COMMANDS:
setwd("code")  # If needed
source("functions.R")
source("data_cleaning.R")

# ISSUE 2: "File not found: Socio_Economic_Accounts_July14.xlsx"
# SOLUTION: Check file path and make sure you're in the right directory
# CHECK:
file.exists("code/data/Socio_Economic_Accounts_July14.xlsx")  # Should return TRUE

# ISSUE 3: "No countries with 2011 data found"
# SOLUTION: Check socio-economic data structure
# DEBUG:
socioeco_data <- readxl::read_excel("code/data/Socio_Economic_Accounts_July14.xlsx", sheet = 2)
colnames(socioeco_data)  # Should include "_2011"
sum(!is.na(socioeco_data$`_2011`))  # Should be > 0

# ISSUE 4: "Error in array indexing"
# SOLUTION: Check that countries match between datasets
# DEBUG:
wiod_countries <- unique(c(wiot_full$row_country, wiot_full$col_country))
socio_countries <- unique(socioeco_data$Country)
intersect(wiod_countries, socio_countries)  # Should have common countries

# ISSUE 5: "Regression failed with no observations"
# SOLUTION: Check trade flows are non-zero
# DEBUG:
sum(cleaned_data$trade_flows_array > 0)  # Should be > 0

# ISSUE 6: "US not found for tariff scenario"
# SOLUTION: Check country names in filtered dataset
# DEBUG:
print(cleaned_data$countries)
grep("US|USA|AMERICA|STATES", cleaned_data$countries, ignore.case = TRUE)

# ISSUE 7: Memory issues with large datasets
# SOLUTION: Use smaller sample for testing
# COMMANDS:
wiot_sample <- wiot_full[sample(nrow(wiot_full), 5000), ]  # Smaller sample

# QUICK DIAGNOSTIC COMMANDS
# ========================

# Check current directory
getwd()

# Check available files
list.files(".", pattern = "*.R")
list.files("code/data", pattern = "*.dta|*.xlsx")

# Check package availability
required_packages <- c("haven", "readxl", "dplyr", "fixest", "writexl")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Test basic function loading
tryCatch({
  source("functions.R")
  cat("✓ Functions loaded\n")
}, error = function(e) {
  cat("✗ Functions error:", conditionMessage(e), "\n")
})

# PERFORMANCE OPTIMIZATION
# =======================

# For large datasets, use these settings:
options(max.print = 1000)  # Limit output
gc()  # Garbage collection

# Monitor memory usage:
# pryr::mem_used()  # If pryr package available

# VALIDATION CHECKLIST
# ===================

# Before running main analysis, verify:
# □ All files exist in correct locations
# □ All functions load without error  
# □ Data cleaning produces non-null results
# □ At least 5+ countries have complete 2011 data
# □ Trade flows array has non-zero values
# □ Regression data preparation succeeds
# □ US identified in country list for tariff analysis

# If all checks pass: source("main.R")