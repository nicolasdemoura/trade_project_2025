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
              "writexl", "modelsummary","lmtest", "progress", "softImpute")
install.lib <- load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib) install.packages(lib, dependencies = TRUE)
sapply(load.lib, require, character = TRUE)
rm(load.lib, install.lib, lib)
gc()

# Set the random seed for reproducibility
set.seed(20251001)

# Set working directory 
setwd("code")

# Source all required CDK (2012) functions
source("variables.R")
source("functions.R")
source("data_cleaning.R")
source("estimation.R")
source("counterfactual.R")

###############################################################################
# Load the data
###############################################################################

# Define file paths and target year
wiot_file_path <- "data/wiot_full.dta"
socioeco_file_path <- "data/Socio_Economic_Accounts_July14.xlsx"
target_year <- 2009

# EU countries (excluding UK)
EU_COUNTRIES <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", 
                  "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", 
                  "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", 
                  "SVN", "ESP", "SWE")

# Define country groups
country_groups <- list(
  "USA" = c("USA"),
  "China" = c("CHN"),
  "Brazil" = c("BRA"),
  "EU" = EU_COUNTRIES,
  "RoW" = c("AUS","CAN","GBR","IDN","IND","JPN","KOR","MEX","RUS","TUR","TWN")
)


# Load and clean the data with country aggregation applied BEFORE cleaning
cleaned_data <- clean_all_data_cdk(wiot_file_path, socioeco_file_path, target_year, country_groups)

# Extract key components from cleaned data
countries <- cleaned_data$countries
sectors <- cleaned_data$sectors
n_countries <- length(countries)
n_sectors <- length(sectors)

###############################################################################
# Estimate the model and run counterfactuals
###############################################################################

# Set trade elasticity parameter (theta)
# Using a common value from the literature (e.g., 6.53 from Costinot, Donaldson, and Komunjer, 2012)
theta <- 6.53

# Prepare data for regression and estimation
regression_data <- prepare_cdk_regression_data(cleaned_data$trade_flows_array, 
                                              cleaned_data$absorption_matrix,
                                              countries, sectors)

# Estimate technology parameters
tech_estimation <- estimate_cdk_technology_params(regression_data, theta = theta)

rownames(tech_estimation$technology_composite)

# Run baseline calibration
baseline_results <- calibrate_cdk_baseline(cleaned_data$observed_trade_shares, 
                                         NULL,  # No predetermined trade balance matrix
                                         tech_estimation$technology_composite, 
                                         cleaned_data$labor_matrix, 
                                         theta, 
                                         cleaned_data$beta_vec,  # Use proper beta_vec from data
                                         countries, 
                                         sectors)
  
observed_data <- list(
  trade_shares = cleaned_data$observed_trade_shares,
  gdp_per_capita = cleaned_data$gdp_vector  # Use proper GDP vector
)

fit_diagnostics <- check_model_fit(baseline_results, observed_data)
  
trump_tariffs <- create_trump_tariff_scenario_cdk(countries, sectors)

counterfactual_results <- compute_counterfactual_equilibrium_cdk(baseline_results, trump_tariffs)

welfare_analysis <- analyze_welfare_effects_cdk(baseline_results, counterfactual_results, countries)
  
robustness_results <- run_robustness_analysis(baseline_results, countries, sectors, theta)

main_results_table <- format_results_table(baseline_results, counterfactual_results, countries)

scenario_comparison <- compare_scenarios(welfare_analysis, robustness_results, countries)

summary_tables <- generate_summary_tables(welfare_analysis, countries)
  
write_xlsx(main_results_table, "Tables/trump_tariff_results.xlsx")

write_xlsx(welfare_analysis$welfare_table, "Tables/welfare_analysis.xlsx")

write_xlsx(scenario_comparison, "Tables/scenario_comparison.xlsx")
