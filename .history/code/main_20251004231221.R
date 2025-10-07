###############################################################################
# CDK Model Analysis - Reorganized Workflow
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

# Clean the working environment
rm(list = ls())

# Load required packages
load.lib <- c("dplyr", "ggplot2", "stargazer", "readxl", "tidyverse", "data.table", 
              "lubridate", "fixest", "pracma", "remotes", "tidyr", "nprobust", 
              "chron", "haven", "readr", "writexl", "modelsummary","lmtest", 
              "progress", "softImpute", "stats")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib, dependencies = TRUE)
sapply(load.lib, require, character = TRUE)
rm(load.lib, install.lib, lib)
gc()

# Set the random seed for reproducibility
set.seed(20251001)

# Set working directory 
setwd("code")

# Source all reorganized functions
source("data_processing.R")        # Task 1: Data cleaning
source("parameter_extraction.R")   # Task 2: Parameter extraction functions  
source("model_calibration.R")      # Task 3: Model calibration with proper equilibrium solver
source("counterfactual.R")         # Task 4: Counterfactual analysis
source("plotting.R")               # Plotting functions

# Note: Updated to use proper CDK equilibrium system from body.tex instead of obsolete regression approach

###############################################################################
# TASK 1: DATA PROCESSING
###############################################################################

cat("=== TASK 1: DATA PROCESSING ===\n")

# Define file paths and target year
wiot_file_path <- "data/wiot_full.dta"
socioeco_file_path <- "data/Socio_Economic_Accounts_July14.xlsx"
exchange_rates_file_path <- "data/Exchange_Rates.xlsx"
tariff_file_path <- "data/tariffs.csv" 
target_year <- 2009

# Define aggregation groups
country_groups <- list(
    "EU" = c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", 
             "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", 
             "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", 
             "SVN", "ESP", "SWE"),
    "RoW" = c("AUS","IDN","KOR","RUS","TUR","TWN","RoW")
)

sector_groups <- list(
    "Food" = c("AtB", "c1", "15t16", "c3"),
    "Textiles" = c("19", "c5", "17t18", "c4"),
    "Paper" = c("21t22", "c7", "20", "c6"),
    "Chemical" = c("24", "c9", "25", "c10"),
    "Metal" = c("26", "c11", "27t28", "c12"),
    "Manufacture" = c("29", "c13", "30t33", "c14", "34t35", "c15", "36t37", "c16"),
    "Mining" = c("C", "c2"),
    "Energy" = c("23", "c8", "E", "c17"),
    "Construction" = c("F", "c18", "70", "c29"),
    "Retail and Wholesale" = c("50", "c19", "51", "c20", "52", "c21"),
    "Transport" = c("60", "c23", "61", "c24", "62", "c25", "63", "c26"),
    "Services" = c("H", "c22", "J", "c28", "M", "c32", "N", "c33", "O", "c34", 
                   "64", "c27", "P", "c35", "L", "c31", "71t74", "c30")
)

# Task 1: Load and clean raw data
raw_database <- load_and_clean_raw_data(
    wiot_file_path = wiot_file_path,
    socioeco_file_path = socioeco_file_path,
    exchange_rates_file_path = exchange_rates_file_path,
    tariff_file_path = tariff_file_path,
    target_year = target_year,
    country_groups = country_groups,
    sector_groups = sector_groups
)

###############################################################################
# TASK 2: PARAMETER EXTRACTION AND PROCESSING
###############################################################################

cat("\n=== TASK 2: PARAMETER EXTRACTION AND PROCESSING ===\n")

# Process raw data and extract all model parameters
results <- process_and_extract_all_parameters(raw_database, theta = 6.53)
model_parameters <- results$parameters
processed_database <- results$processed_database

# Display parameter structure
cat("Parameters extracted:\n")
cat("  - Alpha (expenditure shares):", paste(dim(model_parameters$alpha), collapse = " x "), "\n")
cat("  - Beta (labor shares):", paste(dim(model_parameters$beta), collapse = " x "), "\n")  
cat("  - Gamma (intermediate shares):", paste(dim(model_parameters$gamma), collapse = " x "), "\n")
cat("  - Technology parameters:", paste(dim(model_parameters$technology), collapse = " x "), "\n")
cat("  - Trade elasticity:", model_parameters$theta, "\n")
cat("  - Tariff data:", paste(dim(model_parameters$tariffs), collapse = " x "), "\n")

###############################################################################
# TASK 3: MODEL CALIBRATION
###############################################################################

cat("\n=== TASK 3: MODEL CALIBRATION ===\n")

# Solve the CDK equilibrium system
equilibrium_solution <- solve_cdk_equilibrium(
    database = processed_database,
    parameters = model_parameters,
    max_iter = 1000,
    tolerance = 1e-6
)

# Check convergence and display results
if (equilibrium_solution$converged) {
    cat("✓ Equilibrium converged in", equilibrium_solution$iterations, "iterations\n")
    cat("  Final error:", sprintf("%.2e", equilibrium_solution$final_error), "\n")
    
    # Display solution structure
    cat("Equilibrium Solution:\n")
    cat("  - Trade shares π:", paste(dim(equilibrium_solution$trade_shares), collapse = " x "), "\n")
    cat("  - Unit costs c:", paste(dim(equilibrium_solution$unit_costs), collapse = " x "), "\n")
    cat("  - Price indices P:", paste(dim(equilibrium_solution$price_indices), collapse = " x "), "\n")
    cat("  - Expenditures X:", paste(dim(equilibrium_solution$expenditures), collapse = " x "), "\n")
    cat("  - Income Y:", length(equilibrium_solution$income), "countries\n")
    cat("  - Wages w:", length(equilibrium_solution$wages), "countries\n")
} else {
    warning("Equilibrium did not converge after ", equilibrium_solution$iterations, " iterations")
    cat("Final error:", sprintf("%.2e", equilibrium_solution$final_error), "\n")
}

###############################################################################
# TASK 4: COUNTERFACTUAL ANALYSIS
###############################################################################

cat("\n=== TASK 4: COUNTERFACTUAL ANALYSIS ===\n")

# Create Trump tariff scenario (default: +10pp US tariffs)
counterfactual_tariffs <- create_tariff_scenario(
    baseline_tariffs = model_parameters$tariffs,
    scenario = "trump_10pp",
    countries = processed_database$countries,
    sectors = processed_database$sectors
)

# Solve counterfactual equilibrium with new tariffs
counterfactual_solution <- solve_cdk_equilibrium(
    database = processed_database,
    parameters = list(
        alpha = model_parameters$alpha,
        beta = model_parameters$beta,
        gamma = model_parameters$gamma,
        technology = model_parameters$technology,
        theta = model_parameters$theta,
        tariffs = counterfactual_tariffs,
        iceberg_costs = equilibrium_solution$iceberg_costs  # Use calibrated iceberg costs
    ),
    max_iter = 1000,
    tolerance = 1e-6
)

if (counterfactual_solution$converged) {
    cat("✓ Counterfactual equilibrium converged in", counterfactual_solution$iterations, "iterations\n")
} else {
    warning("Counterfactual equilibrium did not converge")
}

# Analyze welfare effects using new equilibrium solutions
welfare_analysis <- analyze_welfare_effects_cdk(
    baseline_solution = equilibrium_solution,
    counterfactual_solution = counterfactual_solution,
    countries = processed_database$countries
)

# Display welfare results
cat("Welfare Analysis Results:\n")
print(welfare_analysis$welfare_effects)
cat("\nWelfare Summary:\n")
print(welfare_analysis$welfare_summary)

###############################################################################
# SAVE RESULTS
###############################################################################

if (!dir.exists("output")) dir.create("output")

# Save all results
saveRDS(list(
    raw_database = raw_database,
    processed_database = processed_database,
    parameters = model_parameters,
    equilibrium_solution = equilibrium_solution,
    counterfactual_solution = counterfactual_solution,
    welfare_analysis = welfare_analysis
), "output/reorganized_cdk_results.rds")

cat("\n✓ Analysis completed - all results saved to output/reorganized_cdk_results.rds\n")