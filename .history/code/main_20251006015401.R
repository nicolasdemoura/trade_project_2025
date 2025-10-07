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
source("data.R")        # Task 1: Data cleaning
source("parameter.R")   # Task 2: Parameter extraction functions  
source("calibration.R")      # Task 3: Model calibration with proper equilibrium solver
source("counterfactual.R")         # Task 4: Counterfactual analysis
source("new_tariff_processing.R")  # New: Process HTS tariff data
source("plotting.R")               # Plotting functions

###############################################################################
# TASK 1: DATA PROCESSING
###############################################################################

cat("=== TASK 1: DATA PROCESSING ===\n")

# Define file paths and target year
wiot_file_path <- "data/wiot_full.dta"
socioeco_file_path <- "data/Socio_Economic_Accounts_July14.xlsx"
exchange_rates_file_path <- "data/Exchange_Rates.xlsx"
tariff_file_path <- "data/tariffs.csv" 
new_tariff_file_path <- "data/new_tariff_data.csv"  # Path to your new HTS tariff dataset
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

# # View all parameters
# for(parameter in names(model_parameters)) {
#     View(model_parameters[[parameter]])
# }

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

# Solve the CDK equilibrium system with joint iceberg cost estimation
equilibrium_solution <- calibrate_model(
    database = processed_database,
    parameters = model_parameters,
    labor_mobility = TRUE
)

# Check convergence and display results
if (equilibrium_solution$summary$convergence_achieved) {
    cat("✓ Equilibrium converged in", equilibrium_solution$summary$iterations, "iterations\n")
    
    # Display solution structure
    cat("Equilibrium Solution:\n")
    cat("  - Trade shares π:", ifelse(is.null(dim(equilibrium_solution$model_solution$trade_shares)), 
                                     length(equilibrium_solution$model_solution$trade_shares), 
                                     paste(dim(equilibrium_solution$model_solution$trade_shares), collapse = " x ")), "\n")
    cat("  - Unit costs c:", ifelse(is.null(dim(equilibrium_solution$model_solution$unit_costs)), 
                                   length(equilibrium_solution$model_solution$unit_costs), 
                                   paste(dim(equilibrium_solution$model_solution$unit_costs), collapse = " x ")), "\n")
    cat("  - Price indices P:", ifelse(is.null(dim(equilibrium_solution$model_solution$prices)), 
                                      length(equilibrium_solution$model_solution$prices), 
                                      paste(dim(equilibrium_solution$model_solution$prices), collapse = " x ")), "\n")
    cat("  - Expenditures X:", ifelse(is.null(dim(equilibrium_solution$model_solution$expenditures)), 
                                     length(equilibrium_solution$model_solution$expenditures), 
                                     paste(dim(equilibrium_solution$model_solution$expenditures), collapse = " x ")), "\n")
    cat("  - Income Y:", length(equilibrium_solution$model_solution$total_income), "countries\n")
    cat("  - Wages w:", ifelse(is.null(dim(equilibrium_solution$model_solution$wages)), 
                              length(equilibrium_solution$model_solution$wages), 
                              paste(dim(equilibrium_solution$model_solution$wages), collapse = " x ")), "\n")
    cat("  - Iceberg costs d:", ifelse(is.null(dim(equilibrium_solution$calibrated_parameters$iceberg_costs)), 
                                      length(equilibrium_solution$calibrated_parameters$iceberg_costs), 
                                      paste(dim(equilibrium_solution$calibrated_parameters$iceberg_costs), collapse = " x ")), "\n")
} else {
    warning("Equilibrium did not converge after ", equilibrium_solution$summary$iterations, " iterations")
}

# Plot trade shares against observed
trade_shares_plot_data <- data.frame(
  observed_trade_shares = as.vector(processed_database$bilateral_trade_shares),
  predicted_trade_shares = as.vector(equilibrium_solution$model_solution$trade_shares)
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

# Plot GDP against observed
gdp_plot_data <- data.frame(
  observed_gdp = log(processed_database$gdp_data[["_2009"]]),
  predicted_gdp = log(as.vector(equilibrium_solution$model_solution$total_income))
)

gg_scatterplot(data = gdp_plot_data,
               x_var = "observed_gdp",
               y_var = "predicted_gdp",
               title = "Observed vs Predicted GDP",
               subtitle = "Model Fit Assessment",
               x_label = "Log Observed GDP",
               y_label = "Log Predicted GDP",
               out_dir = "figures",
               out_name = "gdp_fit",
               width = 9,
               height = 9,
               xlim = c(6,18),
               ylim = c(6,18),
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
# TASK 4: NEW TARIFF DATA PROCESSING
###############################################################################

cat("\n=== TASK 4A: NEW TARIFF DATA PROCESSING ===\n")

# Process new HTS tariff dataset with multiple time windows
if (file.exists(new_tariff_file_path)) {
    
    new_tariff_results <- process_new_tariff_pipeline(
        tariff_file_path = new_tariff_file_path,
        cdk_countries = processed_database$countries,
        cdk_sectors = processed_database$sectors,
        current_date = as.Date("2025-10-06")  # Adjust to current date
    )
    
    # Display processed tariff data summary
    cat("New Tariff Data Summary:\n")
    print(head(new_tariff_results$tariff_rates_df, 10))
    
    # Save clean dataset as requested
    write.csv(new_tariff_results$tariff_rates_df, "output/clean_tariff_rates.csv", row.names = FALSE)
    cat("✓ Clean tariff dataset saved to: output/clean_tariff_rates.csv\n")
    
} else {
    cat("⚠ New tariff file not found. Using original tariff scenario...\n")
    new_tariff_results <- NULL
}

###############################################################################
# TASK 4B: COUNTERFACTUAL ANALYSIS WITH NEW TARIFFS
###############################################################################

cat("\n=== TASK 4B: COUNTERFACTUAL ANALYSIS ===\n")

if (!is.null(new_tariff_results)) {
    
    # Use 2024 tariff rates as baseline (replace original model tariffs)
    cat("Using 2024 tariff rates as baseline...\n")
    baseline_parameters <- model_parameters
    baseline_parameters$tariffs <- new_tariff_results$tariff_arrays$tariff_2024
    
    # Re-calibrate model with 2024 tariff baseline
    cat("Re-calibrating model with 2024 tariff baseline...\n")
    baseline_equilibrium_2024 <- calibrate_model(
        database = processed_database,
        parameters = baseline_parameters,
        labor_mobility = TRUE
    )
    
    if (baseline_equilibrium_2024$summary$convergence_achieved) {
        cat("✓ 2024 baseline calibration converged\n")
        
        # Run multiple counterfactual scenarios
        multiple_counterfactuals <- run_multiple_tariff_counterfactuals(
            baseline_equilibrium = baseline_equilibrium_2024,
            new_tariff_results = new_tariff_results,
            database = processed_database,
            parameters = baseline_parameters,
            labor_mobility = TRUE
        )
        
        # Analyze welfare effects across scenarios
        multiple_welfare_analysis <- analyze_multiple_scenario_welfare(
            baseline_outcomes = baseline_equilibrium_2024$outcome_variables,
            multiple_counterfactual_results = multiple_counterfactuals,
            countries = processed_database$countries
        )
        
        # Display comparative welfare results
        cat("Comparative Welfare Analysis Results:\n")
        print(multiple_welfare_analysis$comparative_summary)
        
        # Save detailed results
        saveRDS(multiple_welfare_analysis, "output/multiple_scenario_welfare_analysis.rds")
        write.csv(multiple_welfare_analysis$comparative_summary, "output/welfare_comparison_summary.csv", row.names = FALSE)
        
    } else {
        cat("✗ 2024 baseline calibration failed to converge\n")
        multiple_counterfactuals <- NULL
        multiple_welfare_analysis <- NULL
    }
    
} else {
    
    # Fallback to original Trump tariff scenario
    cat("Running original Trump tariff scenario...\n")
    
    # Create Trump tariff scenario (default: +10pp US tariffs)
    counterfactual_tariffs <- create_tariff_scenario(
        baseline_tariffs = model_parameters$tariffs,
        scenario = "trump_10pp",
        countries = processed_database$countries,
        sectors = processed_database$sectors
    )
    
    # Run counterfactual analysis with fixed iceberg costs from baseline
    counterfactual_solution <- run_counterfactual(
        baseline_equilibrium = equilibrium_solution,
        new_tariffs = counterfactual_tariffs,
        database = processed_database,
        parameters = model_parameters,
        labor_mobility = TRUE
    )
    
    if (counterfactual_solution$converged) {
        cat("✓ Counterfactual equilibrium converged in", counterfactual_solution$iterations, "iterations\n")
    } else {
        warning("Counterfactual equilibrium did not converge")
    }
    
    # Analyze welfare effects using new equilibrium solutions
    welfare_analysis <- analyze_welfare_effects_cdk(
        baseline_outcomes = equilibrium_solution$outcome_variables,
        counterfactual_outcomes = counterfactual_solution,
        countries = processed_database$countries
    )
    
    # Display welfare results
    cat("Welfare Analysis Results:\n")
    print(welfare_analysis$welfare_effects)
    cat("\nWelfare Summary:\n")
    print(welfare_analysis$welfare_summary)
    
    multiple_counterfactuals <- NULL
    multiple_welfare_analysis <- welfare_analysis
}

###############################################################################
# SAVE RESULTS
###############################################################################

if (!dir.exists("output")) dir.create("output")

# Save all results
if (!is.null(new_tariff_results)) {
    # Save results with new tariff analysis
    saveRDS(list(
        raw_database = raw_database,
        processed_database = processed_database,
        parameters = model_parameters,
        equilibrium_solution = equilibrium_solution,
        new_tariff_results = new_tariff_results,
        baseline_equilibrium_2024 = if(exists("baseline_equilibrium_2024")) baseline_equilibrium_2024 else NULL,
        multiple_counterfactuals = if(exists("multiple_counterfactuals")) multiple_counterfactuals else NULL,
        multiple_welfare_analysis = if(exists("multiple_welfare_analysis")) multiple_welfare_analysis else NULL
    ), "output/cdk_results_new_tariffs.rds")
} else {
    # Save original results
    saveRDS(list(
        raw_database = raw_database,
        processed_database = processed_database,
        parameters = model_parameters,
        equilibrium_solution = equilibrium_solution,
        counterfactual_solution = if(exists("counterfactual_solution")) counterfactual_solution else NULL,
        welfare_analysis = multiple_welfare_analysis
    ), "output/cdk_results_mobile.rds")
}