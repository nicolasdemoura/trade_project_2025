###############################################################################
# Demonstration: Using New HTS Tariff Data for CDK Model Analysis
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-06
###############################################################################

# This script demonstrates how to use your new HTS tariff dataset
# Save this as "demo_new_tariffs.R" and run after setting up your data

###############################################################################
# SETUP AND DATA PREPARATION
###############################################################################

# First, place your HTS tariff data file in the data/ directory
# The file should be named "new_tariff_data.csv" (or update the path in main.R)
# 
# Your file should have these columns:
# - HTS Number (2-digit codes like 35, 76, 88, etc.)
# - Year (2024, 2025)
# - Month (1-12)
# - Description (product description)
# - Country (country names)
# - General Customs Value (in dollars)
# - General Import Charges (tariff charges in dollars)

# Example of expected data structure:
cat("Expected data structure:\n")
example_data <- data.frame(
    HTS_Number = c(35, 76, 88, 35),
    Year = c(2024, 2024, 2024, 2024),
    Month = c(1, 1, 1, 1),
    Description = c("ALBUMINOIDAL SUBSTANCES", "ALUMINUM AND ARTICLES", 
                   "AIRCRAFT, SPACECRAFT", "ALBUMINOIDAL SUBSTANCES"),
    Country = c("European Union", "European Union", "European Union", "Brazil"),
    General_Customs_Value = c(127935104.00, 191618939.00, 1035549864.00, 19475183.00),
    General_Import_Charges = c(4164219.00, 7185475.00, 5910381.00, 562975.00)
)
print(example_data)

###############################################################################
# RUNNING THE ANALYSIS
###############################################################################

# Step 1: Run the main analysis
# This will automatically:
# - Load and process your HTS data
# - Map HTS codes to CDK sectors
# - Map countries to CDK country groups
# - Calculate tariff rates for different time windows
# - Run multiple counterfactual scenarios

source("main.R")

###############################################################################
# EXAMINING THE RESULTS
###############################################################################

# If the analysis completed successfully, you'll have these outputs:

# 1. Clean tariff dataset (as requested)
if (file.exists("output/clean_tariff_rates.csv")) {
    cat("\n=== CLEAN TARIFF DATASET ===\n")
    clean_tariffs <- read.csv("output/clean_tariff_rates.csv")
    print(head(clean_tariffs, 10))
    
    cat("\nColumns in clean dataset:\n")
    print(names(clean_tariffs))
    
    cat("\nSummary statistics:\n")
    print(summary(clean_tariffs[, c("tariff_rate24", "tariff_rate25_YTD", 
                                   "tariff_rate25_LTM", "tariff_rate25_3M")]))
}

# 2. Welfare comparison across scenarios
if (file.exists("output/welfare_comparison_summary.csv")) {
    cat("\n=== WELFARE COMPARISON ACROSS SCENARIOS ===\n")
    welfare_comparison <- read.csv("output/welfare_comparison_summary.csv")
    print(welfare_comparison)
}

# 3. Detailed results
if (file.exists("output/multiple_scenario_welfare_analysis.rds")) {
    cat("\n=== DETAILED WELFARE ANALYSIS ===\n")
    detailed_results <- readRDS("output/multiple_scenario_welfare_analysis.rds")
    
    # Show individual country effects for each scenario
    for (scenario in names(detailed_results$individual_results)) {
        cat(sprintf("\n--- %s Results ---\n", scenario))
        scenario_effects <- detailed_results$individual_results[[scenario]]$welfare_effects
        print(head(scenario_effects[order(scenario_effects$welfare_change_pct, decreasing = TRUE), ], 5))
    }
}

###############################################################################
# CREATING CUSTOM VISUALIZATIONS
###############################################################################

# You can create custom plots using the plotting functions
if (exists("new_tariff_results") && !is.null(new_tariff_results)) {
    
    # Plot tariff rates by sector
    library(ggplot2)
    
    tariff_data_plot <- new_tariff_results$tariff_rates_df %>%
        tidyr::pivot_longer(cols = starts_with("tariff_rate"), 
                           names_to = "Period", values_to = "Tariff_Rate") %>%
        mutate(Period = case_when(
            Period == "tariff_rate24" ~ "2024",
            Period == "tariff_rate25_YTD" ~ "2025 YTD",
            Period == "tariff_rate25_LTM" ~ "2025 LTM",
            Period == "tariff_rate25_3M" ~ "2025 3M"
        ))
    
    # Average tariff by sector and period
    sector_tariffs <- tariff_data_plot %>%
        group_by(Sector, Period) %>%
        summarise(Avg_Tariff = mean(Tariff_Rate, na.rm = TRUE), .groups = "drop")
    
    p1 <- ggplot(sector_tariffs, aes(x = Sector, y = Avg_Tariff, fill = Period)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_minimal() +
        labs(title = "Average Tariff Rates by Sector and Time Period",
             x = "Sector", y = "Average Tariff Rate") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggsave("output/tariff_rates_by_sector.png", p1, width = 12, height = 8)
    cat("✓ Tariff rate plot saved to: output/tariff_rates_by_sector.png\n")
}

###############################################################################
# INTERPRETATION GUIDE
###############################################################################

cat("\n=== INTERPRETATION GUIDE ===\n")
cat("1. tariff_rate24: Baseline tariff rates for 2024 (used as baseline in model)\n")
cat("2. tariff_rate25_YTD: Year-to-date 2025 rates (counterfactual scenario)\n") 
cat("3. tariff_rate25_LTM: Last 12 months rates (counterfactual scenario)\n")
cat("4. tariff_rate25_3M: Last 3 months rates (most recent counterfactual)\n")
cat("\nWelfare effects show the change from 2024 baseline to each counterfactual scenario.\n")
cat("Positive welfare changes indicate countries that benefit from the tariff changes.\n")
cat("Negative welfare changes indicate countries that are harmed by the tariff changes.\n")

###############################################################################
# TROUBLESHOOTING
###############################################################################

cat("\n=== TROUBLESHOOTING ===\n")
cat("If you encounter issues:\n")
cat("1. Check that your HTS data file exists at: ", new_tariff_file_path, "\n")
cat("2. Verify column names match expected format\n")
cat("3. Ensure HTS codes are 2-digit numbers\n")
cat("4. Check that countries in your data can be mapped to CDK groups\n")
cat("5. Verify General Customs Value > 0 to avoid division by zero\n")

# Show mapping tables for reference
cat("\n=== SECTOR MAPPINGS ===\n")
hts_mapping <- create_hts_to_sector_mapping()
for (sector in unique(hts_mapping)) {
    hts_codes <- names(hts_mapping)[hts_mapping == sector]
    cat(sprintf("%s: %s\n", sector, paste(hts_codes, collapse = ", ")))
}

cat("\n=== COUNTRY MAPPINGS ===\n")
country_mapping <- create_country_mapping()
for (cdk_country in unique(country_mapping)) {
    original_countries <- names(country_mapping)[country_mapping == cdk_country]
    cat(sprintf("%s: %s\n", cdk_country, paste(original_countries, collapse = ", ")))
}