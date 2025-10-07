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

###############################################################################
# Main Analysis Workflow
###############################################################################

cat("\n=== Starting Main Analysis ===\n")

# Step 1: Clean and prepare the data using CDK (2012) methodology
cat("Step 1: Data cleaning and preparation (CDK 2012)...\n")
cleaned_data <- clean_all_data_cdk(wiot_full, socioeco, year = 2011)

# Step 2: Set up CDK (2012) parameters
cat("Step 2: Setting up CDK (2012) parameters...\n")
countries <- cleaned_data$countries
sectors <- cleaned_data$sectors
n_countries <- length(countries)
n_sectors <- length(sectors)

# Trade elasticities by sector (from CDK 2012 Table 3)
theta_vec <- rep(8.28, n_sectors)  # Manufacturing average
names(theta_vec) <- sectors

cat("Using", n_countries, "countries and", n_sectors, "sectors\n")
cat("Trade elasticities:", paste(round(theta_vec, 2), collapse = ", "), "\n")

# Step 3: Prepare CDK regression data
if (!is.null(cleaned_data$trade_flows_array)) {
  cat("Step 3: Preparing CDK regression data...\n")
    
    # Prepare regression data
    regression_data <- prepare_regression_data(cleaned_data$trade_matrices, countries, sectors)
    
    # Step 4: Estimate technology parameters
    cat("Step 4: Estimating technology parameters...\n")
    tech_estimation <- estimate_technology_params(regression_data, theta_fixed = theta)
    
    # Extract technology matrix
    T_matrix <- extract_technology_matrix(tech_estimation, countries, sectors)
    
    # Step 5: Calibrate baseline equilibrium
    cat("Step 5: Calibrating baseline equilibrium...\n")
    
    # Need observed trade shares for calibration
    observed_shares <- calculate_observed_trade_shares(cleaned_data$trade_matrices, 
                                                     cleaned_data$wiod_clean$outputs)
    
    baseline_results <- calibrate_baseline_equilibrium(observed_shares, T_matrix, 
                                                      cleaned_data$socioeco_clean$labor_inputs, theta)
    
    # Step 6: Model diagnostics
    cat("Step 6: Checking model fit...\n")
    observed_data <- list(
      trade_shares = observed_shares,
      gdp_per_capita = NULL  # Would extract from socioeco if available
    )
    
    fit_diagnostics <- check_model_fit(baseline_results, observed_data)
    
    # Step 7: Trump tariff counterfactual
    cat("Step 7: Running Trump tariff counterfactual...\n")
    
    # Create tariff scenario
    trump_tariffs <- create_trump_tariff_scenario(countries, sectors)
    
    # Compute counterfactual equilibrium
    counterfactual_results <- compute_counterfactual_equilibrium(baseline_results, trump_tariffs, theta)
    
    # Analyze welfare effects
    welfare_analysis <- analyze_welfare_effects(baseline_results, counterfactual_results, countries)
    
    # Step 8: Robustness analysis
    cat("Step 8: Running robustness analysis...\n")
    robustness_results <- run_robustness_analysis(baseline_results, countries, sectors, theta)
    
    # Step 9: Generate results tables and figures
    cat("Step 9: Generating results and output tables...\n")
    
    # Main results table
    main_results_table <- format_results_table(baseline_results, counterfactual_results, countries)
    
    # Scenario comparison
    scenario_comparison <- compare_scenarios(welfare_analysis, robustness_results, countries)
    
    # Summary tables
    summary_tables <- generate_summary_tables(welfare_analysis, countries)
    
    # Step 10: Export results
    cat("Step 10: Exporting results...\n")
    
    # Save main results table
    write_xlsx(main_results_table, "Tables/trump_tariff_results.xlsx")
    
    # Save welfare analysis
    write_xlsx(welfare_analysis$welfare_table, "Tables/welfare_analysis.xlsx")
    
    # Save scenario comparison
    write_xlsx(scenario_comparison, "Tables/scenario_comparison.xlsx")
    
    # Print summary
    cat("\n=== Analysis Summary ===\n")
    cat("Global welfare change under Trump tariffs:", round(welfare_analysis$global_change, 2), "%\n")
    cat("Countries benefiting:", paste(welfare_analysis$winners, collapse = ", "), "\n")
    cat("Countries losing:", paste(welfare_analysis$losers, collapse = ", "), "\n")
    
    cat("\nResults saved to Tables/ directory\n")
    cat("Analysis completed successfully!\n")
  }
} else {
  cat("Data cleaning incomplete - check data structure and update cleaning functions.\n")
}

cat("\n=== End of Analysis ===\n")
