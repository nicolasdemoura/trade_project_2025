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
              "progress", "softImpute", "stats", "xtable")
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
us_tariff_file_path <- "data/tariffs2025.xlsx"
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
    "Retail" = c("50", "c19", "51", "c20", "52", "c21"),
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
    us_tariff_file_path = us_tariff_file_path,
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

# Export these to tables
alpha_xtable <- xtable(model_parameters$alpha, caption = "Expenditure Shares ($\\alpha$)", label = "tab:alpha")
print(alpha_xtable, file = "tables/alpha_table.tex", include.rownames = TRUE, caption.placement = "top", sanitize.text.function = identity, # allows LaTeX symbols
    align = c("r", rep("c", ncol(model_parameters$alpha))),
    add.to.row = list(
      pos = list(nrow(model_parameters$alpha)),
      command = paste0("\\hline \\multicolumn{",
              ncol(model_parameters$alpha) + 1,
              "}{l}{\\footnotesize This table presents the expenditure shares ($\\alpha$) for each country-sector pair.} \\\\")
    )
)

beta_xtable <- xtable(model_parameters$beta, caption = "Labor Shares ($\\beta$)", label = "tab:beta")
print(beta_xtable, file = "tables/beta_table.tex", include.rownames = TRUE, caption.placement = "top", sanitize.text.function = identity, # allows LaTeX symbols
    align = c("l", rep("c", ncol(model_parameters$beta))),
    add.to.row = list(
      pos = list(nrow(model_parameters$beta)),
      command = paste0("\\hline \\multicolumn{",
              ncol(model_parameters$beta) + 1,
              "}{l}{\\footnotesize This table presents the labor shares ($\\beta$) for each country-sector pair.} \\\\")
    )
)

# Export gamma tables for each country using xtable
for(j in 1:dim(model_parameters$gamma)[1]) {
    country <- rownames(model_parameters$gamma)[j]
    gamma_matrix <- model_parameters$gamma[j,,]
    colnames(gamma_matrix) <- colnames(model_parameters$gamma)
    rownames(gamma_matrix) <- colnames(model_parameters$gamma)
    
    gamma_xtable <- xtable(gamma_matrix, 
                           caption = paste0("Intermediate Input Shares ($\\gamma$) - ", country), 
                           label = paste0("tab:gamma_", country))
    print(gamma_xtable, 
          file = paste0("tables/gamma_table_", country, ".tex"), 
          include.rownames = TRUE, 
          caption.placement = "top", 
          sanitize.text.function = identity, # allows LaTeX symbols
          align = c("l", rep("c", ncol(gamma_matrix))),
          add.to.row = list(
            pos = list(nrow(gamma_matrix)),
            command = paste0("\\hline \\multicolumn{",
                    ncol(gamma_matrix) + 1,
                    "}{l}{\\footnotesize This table presents the intermediate input shares ($\\gamma$) for ", 
                    country, " across all sectors.} \\\\")
          )
    )
}

modelsummary::modelsummary(model_parameters$technology, output = "tables/technology_table.tex", 
                           title = "Technology Parameters", 
                           note = "This table presents the technology parameters for each country-sector pair.", 
                           escape = FALSE)

for(i in 1:dim(model_parameters$tariffs)[1]) {
    country <- rownames(model_parameters$tariffs)[i]
    tariff_vector <- model_parameters$tariffs[i,]
    names(tariff_vector) <- colnames(model_parameters$tariffs)
    tariff_df <- as.data.frame(t(tariff_vector))
    colnames(tariff_df) <- colnames(model_parameters$tariffs)
    modelsummary::modelsummary(tariff_df, output = paste0("tables/tariff_table_", country, ".tex"), 
                               title = paste("Tariff Rates -", country), 
                               note = paste("This table presents the tariff rates for", country, "across all sectors."), 
                               escape = FALSE)
}

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
# TASK 4: COUNTERFACTUAL ANALYSIS
###############################################################################

cat("\n=== TASK 4: COUNTERFACTUAL ANALYSIS ===\n")
welfare_analysis <- run_multiple_counterfactual_scenario(baseline_equilibrium = equilibrium_solution,  
                                                         database = processed_database,
                                                         parameters = model_parameters,
                                                         scenarios = c("tariff_rate24", "tariff_rate25_YTD", "tariff_rate25_LTM", "tariff_rate25_3M"), 
                                                         labor_mobility = TRUE)

# Display welfare results
cat("Welfare Analysis Results:\n")
for (scenario in names(welfare_analysis$welfare_effects)) {
    cat("  - Scenario:", scenario, "\n")
    print(welfare_analysis$welfare_effects[[scenario]])
}

###############################################################################
# SAVE RESULTS
###############################################################################

if (!dir.exists("output")) dir.create("output")

# Save all results
saveRDS(list(
    raw_database = raw_database,
    processed_database = processed_database,
    model_parameters = model_parameters,
    equilibrium_solution = equilibrium_solution,
    welfare_analysis = welfare_analysis
), file = "output/cdk_model_results.rds")