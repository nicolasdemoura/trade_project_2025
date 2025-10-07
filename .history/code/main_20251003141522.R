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
              "writexl", "modelsummary","lmtest", "progress", "softImpute", "stats")
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
source("data_cleaning.R")
source("estimation.R")
source("plotting.R")
source("counterfactual.R")

###############################################################################
# Load the data
###############################################################################

# Define file paths and target year
wiot_file_path <- "data/wiot_full.dta"
socioeco_file_path <- "data/Socio_Economic_Accounts_July14.xlsx"
exchange_rates_file_path <- "data/Exchange_Rates.xlsx"
tariff_file_path <- "data/tariffs.csv" 
target_year <- 2009


# Define country groups
country_groups <- list(
  "EU" = c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", 
                  "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", 
                  "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", 
                  "SVN", "ESP", "SWE"),
  "RoW" = c("AUS","IDN","KOR","RUS","TUR","TWN","RoW")
)
# Define sector groups - mapping from WIOD codes to aggregated sectors
sector_groups <- list(
  "Food" = c("AtB", "c1", "15t16", "c3"),  # Agriculture, hunting, forestry and fishing + Food products and beverages
  "Textiles" = c("19", "c5", "17t18", "c4"),  # Textiles and textile products + Leather, leather products and footwear
  "Paper" = c("21t22", "c7", "20", "c6"),  # Pulp, paper, paper products, printing and publishing + Wood and products of wood and cork
  "Chemical" = c("24", "c9", "25", "c10"),  # Chemicals and chemical products + Rubber and plastics products
  "Metal" = c("26", "c11", "27t28", "c12"),  # Other non-metallic mineral products + Basic metals and fabricated metal products
  "Manufacture" = c("29", "c13", "30t33", "c14", "34t35", "c15", "36t37", "c16"),  # Machinery, equipment, motor vehicles, and other manufacturing
  "Mining" = c("C", "c2"),  # Mining and quarrying
  "Energy" = c("23", "c8", "E", "c17"),  # Coke, refined petroleum products and nuclear fuel + Electricity, gas and water supply
  "Construction" = c("F", "c18", "70", "c29"),  # Construction + Real estate activities
  "Retail and Wholesale" = c("50", "c19", "51", "c20", "52", "c21"),  # Sale, maintenance and repair of motor vehicles + Wholesale trade + Retail trade
  "Transport" = c("60", "c23", "61", "c24", "62", "c25", "63", "c26"),  # Inland transport + Water transport + Air transport + Other supporting transport activities
  "Services" = c("H", "c22", "J", "c28", "M", "c32", "N", "c33", "O", "c34", "64", "c27", "P", "c35", "L", "c31", "71t74", "c30")  # Hotels and restaurants + Financial intermediation + Public admin + Health and social work + Other community services + Post and telecommunications + Education + Renting of machinery and other business activities
)

# Load and clean the data with country and sector aggregation 
cleaned_data <- clean_all_data_cdk(wiot_file_path, socioeco_file_path, exchange_rates_file_path, tariff_file_path,
                                   target_year, country_groups, sector_groups)

beta_matrix <- cleaned_data$beta_matrix
gamma_matrix <- cleaned_data$gamma_matrix
expenditure_matrix <- cleaned_data$expenditure_matrix
labor_matrix <- cleaned_data$labor_matrix
trade_flows_array <- cleaned_data$trade_flows_array
gdp_data <- cleaned_data$gdp_data

# Extract key components from cleaned data
countries <- cleaned_data$countries
sectors <- cleaned_data$sectors
n_countries <- length(countries)
n_sectors <- length(sectors)

###############################################################################
# Estimate the model 
###############################################################################

# Set trade elasticity parameter (theta)
# Using a common value from the literature (e.g., 6.53 from Costinot, Donaldson, and Komunjer, 2012)
theta <- 6.53

# Prepare data for regression and estimation
regression_data <- prepare_cdk_regression_data(cleaned_data$trade_flows_array, 
                                              cleaned_data$expenditure_matrix,
                                              countries, sectors)

# Estimate technology parameters
tech_estimation <- estimate_cdk_technology_params(regression_data, theta = theta)
T_matrix <- tech_estimation$technology_composite


# BASELINE CALIBRATION WITH TARIFFS:
# The model now incorporates baseline tariffs from cleaned_data$tariffs (if available)
# Trade costs are: tau_ij * (1 + tariff_rate_ij)
# Welfare includes both basic real income and tariff revenue transfers
baseline_results <- calibrate_multisector_baseline(T_matrix = T_matrix, 
                                                   cleaned_data = cleaned_data,
                                                   theta = theta,
                                                   labor_mobility = FALSE)  # Change to FALSE for immobile labor

# Save baseline results  
if (!dir.exists("output")) dir.create("output")
saveRDS(baseline_results, "output/baseline_results.rds")

# Create scatter plot of observed vs predicted trade flows
trade_shares_plot_data <- data.frame(
  observed_trade_shares = as.vector(cleaned_data$observed_trade_shares),
  predicted_trade_shares = as.vector(baseline_results$equilibrium$trade_shares)
)

gg_scatterplot(data = trade_shares_plot_data,
         x_var = "observed_trade_shares",
         y_var = "predicted_trade_shares", 
         title = "Observed vs Predicted Trade Flows",
         subtitle = "Model Fit Assessment",
         x_label = "Log Observed Trade Flows",
         y_label = "Log Predicted Trade Flows",
         out_dir = "figures",
         out_name = "trade_flows_fit",
         width = 9,
         height = 9,
         xlim = c(0,1),
         ylim = c(0,1),
         point_color = "#CC0000",
         point_size = 2.5,
         point_alpha = 0.7,
         show_trend = FALSE,
         show_diagonal = TRUE,
         trend_color = "#2c3e50",
         legend = TRUE,
         overwrite = TRUE,
         verbose = TRUE)

# Create scatter plot for GDP analysis
gdp_plot_data <- data.frame(
  observed_log_gdp = log(cleaned_data$gdp_data[["_2009"]], base = 10),
  predicted_log_gdp = log(baseline_results$equilibrium$gdp, base = 10)
)

gg_scatterplot(data = gdp_plot_data,
         x_var = "observed_log_gdp",
         y_var = "predicted_log_gdp",
         title = "Observed vs Predicted GDP",
         subtitle = "Model Fit Assessment",
         x_label = "Log Observed GDP",
         y_label = "Log Predicted GDP",
         out_dir = "figures",
         out_name = "gdp_fit",
         width = 9,
         height = 9,
         xlim = c(3,8),
         ylim = c(3,8),
         point_color = "#CC0000",
         point_size = 2.5,
         point_alpha = 0.7,
         show_trend = FALSE,
         show_diagonal = TRUE,
         trend_color = "#2c3e50",
         legend = TRUE,
         overwrite = TRUE,
         verbose = TRUE)

###############################################################################
# Run counterfactuals and welfare analysis
###############################################################################

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
