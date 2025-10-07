###############################################################################
# Example Usage: New Tariff Dataset Integration with CDK Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-06
###############################################################################

# This script demonstrates how to use the new tariff dataset with the existing CDK model workflow

# Load required libraries and source files
source("data.R")
source("parameter.R")
source("calibration.R")
source("counterfactual.R")
source("new_tariff_processor.R")

# ============================================================================
# STEP 1: PROCESS NEW TARIFF DATASET (Example usage)
# ============================================================================

# If you want to process the new tariff dataset separately first:
# new_tariff_file <- "path/to/your/new_tariff_data.csv"
# processed_tariffs <- process_new_tariff_dataset(new_tariff_file)
# export_processed_tariffs(processed_tariffs, "output/")

# ============================================================================
# STEP 2: LOAD AND PROCESS ALL DATA (Including New Tariffs)
# ============================================================================

# Define your aggregation groups (same as before)
country_groups <- list(
    "USA" = c("USA"),
    "CHN" = c("CHN"), 
    "JPN" = c("JPN"),
    "EU" = c("AUT", "BEL", "BGR", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", 
            "GBR", "GRC", "HRV", "HUN", "IRL", "ITA", "LTU", "LUX", "LVA", "MLT", "NLD", 
            "POL", "PRT", "ROU", "SVK", "SVN", "SWE"),
    "CAN" = c("CAN"),
    "MEX" = c("MEX"),
    "BRA" = c("BRA"),
    "IND" = c("IND"),
    "RoW" = c("AUS", "IDN", "KOR", "RUS", "TUR", "TWN", "RoW")
)

sector_groups <- list(
    "Chemical" = paste0("c", c(35, 33, 36, 31, 28, 13, 38, 29, 30, 34, 32, 39)),
    "Construction" = paste0("c", c(68, 25)),
    "Energy" = paste0("c", c(27, 84)),
    "Food" = paste0("c", c(15, 22, 10, 18, 9, 4, 8, 7, 3, 1, 6, 2, 21, 12, 19, 16, 20, 5, 11, 23, 17, 24)),
    "Manufacture" = paste0("c", c(88, 93, 42, 69, 91, 45, 85, 64, 94, 43, 70, 65, 46, 83, 96, 92, 71, 90, 37, 67, 49, 86, 41, 40, 89, 82, 95, 66, 14, 87, 97)),
    "Metal" = paste0("c", c(76, 73, 74, 72, 78, 75, 81, 80)),
    "Mining" = paste0("c", c(26)),
    "Paper" = paste0("c", c(44, 48, 47)),
    "Textiles" = paste0("c", c(61, 62, 57, 52, 59, 60, 54, 55, 63, 53, 50, 58, 56, 51))
)

# Load all data (INCLUDING new tariff dataset)
raw_database <- load_and_clean_raw_data(
    wiot_file_path = "data/wiot_full.dta",
    socioeco_file_path = "data/Socio_Economic_Accounts_July14.xlsx", 
    exchange_rates_file_path = "data/Socio_Economic_Accounts_July14.xlsx",
    tariff_file_path = NULL,  # Optional: existing tariff data
    new_tariff_file_path = "path/to/your/new_tariff_data.csv",  # NEW: Path to your HTS tariff CSV
    target_year = 2011,
    country_groups = country_groups,
    sector_groups = sector_groups
)

# ============================================================================
# STEP 3: EXTRACT PARAMETERS WITH NEW TARIFF SCENARIOS
# ============================================================================

# Extract parameters WITH new tariff scenarios
parameter_results <- process_and_extract_all_parameters(
    raw_database = raw_database,
    theta = 6.53,
    use_new_tariffs = TRUE  # IMPORTANT: Set this to TRUE to use new tariff scenarios
)

parameters <- parameter_results$parameters
database <- parameter_results$processed_database

# Check available tariff scenarios
cat("Available tariff scenarios:\n")
print(names(parameters$tariff_scenarios))

# ============================================================================
# STEP 4: CALIBRATE MODEL USING BASELINE TARIFFS (tariff24 or baseline)
# ============================================================================

# Calibrate model using 2024 tariffs as baseline
baseline_parameters <- parameters
baseline_parameters$tariffs <- parameters$tariff_scenarios$tariff_rate24  # Use 2024 tariffs as baseline

calibration_results <- calibrate_model(
    database = database,
    parameters = baseline_parameters,
    labor_mobility = TRUE
)

# ============================================================================
# STEP 5: RUN COUNTERFACTUAL ANALYSIS FOR ALL SCENARIOS
# ============================================================================

# Option 1: Run all scenarios at once
all_scenarios <- c("tariff_rate25_YTD", "tariff_rate25_LTM", "tariff_rate25_3M")

scenario_results <- run_multiple_counterfactual_scenarios(
    baseline_equilibrium = calibration_results,
    database = database, 
    parameters = parameters,
    scenarios = all_scenarios,
    labor_mobility = TRUE
)

# ============================================================================
# STEP 6: COMPARE WELFARE EFFECTS ACROSS SCENARIOS
# ============================================================================

welfare_comparison <- compare_scenario_welfare_effects(
    scenario_results = scenario_results,
    countries = database$countries
)

# Display results
cat("=== WELFARE COMPARISON ACROSS TARIFF SCENARIOS ===\n")
print(welfare_comparison$summary_statistics)

cat("\n=== COUNTRY-LEVEL WELFARE EFFECTS ===\n")
print(welfare_comparison$country_comparison)

# ============================================================================
# STEP 7: RUN INDIVIDUAL SCENARIOS (Alternative approach)
# ============================================================================

# Option 2: Run scenarios individually for more control

# Scenario 1: 2025 YTD vs 2024 baseline
cat("\n--- Running 2025 YTD vs 2024 Baseline ---\n")

ytd_tariffs <- create_tariff_scenario(
    baseline_tariffs = parameters$tariffs,
    scenario = "tariff_rate25_YTD",
    countries = database$countries,
    sectors = database$sectors,
    tariff_scenarios = parameters$tariff_scenarios
)

ytd_results <- run_counterfactual(
    baseline_equilibrium = calibration_results,
    new_tariffs = ytd_tariffs,
    database = database,
    parameters = parameters,
    labor_mobility = TRUE
)

ytd_welfare <- analyze_welfare_effects_cdk(
    baseline_outcomes = calibration_results$outcome_variables,
    counterfactual_outcomes = ytd_results,
    countries = database$countries
)

# Scenario 2: 3-Month vs 2024 baseline
cat("\n--- Running 3-Month vs 2024 Baseline ---\n")

three_m_tariffs <- create_tariff_scenario(
    baseline_tariffs = parameters$tariffs,
    scenario = "tariff_rate25_3M",
    countries = database$countries,
    sectors = database$sectors,
    tariff_scenarios = parameters$tariff_scenarios
)

three_m_results <- run_counterfactual(
    baseline_equilibrium = calibration_results,
    new_tariffs = three_m_tariffs,
    database = database,
    parameters = parameters,
    labor_mobility = TRUE
)

three_m_welfare <- analyze_welfare_effects_cdk(
    baseline_outcomes = calibration_results$outcome_variables,
    counterfactual_outcomes = three_m_results,
    countries = database$countries
)

# ============================================================================
# STEP 8: SAVE RESULTS
# ============================================================================

# Save all results
save(
    scenario_results,
    welfare_comparison,
    ytd_results, ytd_welfare,
    three_m_results, three_m_welfare,
    file = "output/tariff_scenario_analysis_results.RData"
)

cat("✓ Analysis completed and results saved!\n")

# ============================================================================
# ADDITIONAL FUNCTIONALITY: Custom Scenario Creation
# ============================================================================

# You can also create custom scenarios by combining different elements:

# Example: Create a scenario that uses 2025 YTD tariffs but only for specific countries
custom_tariffs <- parameters$tariff_scenarios$baseline  # Start with baseline
usa_idx <- which(database$countries == "USA")

# Apply 2025 YTD rates only for China -> USA trade
chn_idx <- which(database$countries == "CHN")
if (length(usa_idx) > 0 && length(chn_idx) > 0) {
    ytd_rates <- parameters$tariff_scenarios$tariff_rate25_YTD
    custom_tariffs[chn_idx, usa_idx, ] <- ytd_rates[chn_idx, usa_idx, ]
}

# Run custom scenario
custom_results <- run_counterfactual(
    baseline_equilibrium = calibration_results,
    new_tariffs = custom_tariffs,
    database = database,
    parameters = parameters,
    labor_mobility = TRUE
)

cat("✓ Custom scenario analysis completed!\n")