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

# Source all required CDK (2012) functions
source("code/functions.R")
source("code/data_cleaning.R")
source("code/estimation.R")
source("code/counterfactual.R")

###############################################################################
# Load the data
###############################################################################

wiot_file_path <- "code/data/wiot_full.dta"
socioeco_file_path <- "code/data/Socio_Economic_Accounts_July14.xlsx"
cleaned_data <- clean_all_data_cdk(wiot_file_path, socioeco_file_path, year = 2011)

countries <- cleaned_data$countries
sectors <- cleaned_data$sectors
n_countries <- length(countries)
n_sectors <- length(sectors)

theta_vec <- rep(8.28, n_sectors)
names(theta_vec) <- sectors

regression_data <- prepare_cdk_regression_data(cleaned_data$trade_flows_array, 
                                              cleaned_data$absorption_matrix,
                                              countries, sectors)

tech_estimation <- estimate_cdk_technology_params(regression_data, theta_vec = theta_vec)
  
baseline_results <- calibrate_cdk_baseline(cleaned_data$observed_trade_shares,
                                          tech_estimation$technology_composite,
                                          cleaned_data$labor_matrix,
                                          theta_vec,
                                          cleaned_data$beta_vec,
                                          countries, sectors)
  
observed_data <- list(
  trade_shares = cleaned_data$observed_trade_shares,
  gdp_per_capita = NULL
)

fit_diagnostics <- check_model_fit(baseline_results, observed_data)
  
trump_tariffs <- create_trump_tariff_scenario_cdk(countries, sectors)

counterfactual_results <- compute_counterfactual_equilibrium_cdk(baseline_results, trump_tariffs)

welfare_analysis <- analyze_welfare_effects_cdk(baseline_results, counterfactual_results, countries)
  
robustness_results <- run_robustness_analysis(baseline_results, countries, sectors, theta_vec)

main_results_table <- format_results_table(baseline_results, counterfactual_results, countries)

scenario_comparison <- compare_scenarios(welfare_analysis, robustness_results, countries)

summary_tables <- generate_summary_tables(welfare_analysis, countries)
  
write_xlsx(main_results_table, "Tables/trump_tariff_results.xlsx")

write_xlsx(welfare_analysis$welfare_table, "Tables/welfare_analysis.xlsx")

write_xlsx(scenario_comparison, "Tables/scenario_comparison.xlsx")
