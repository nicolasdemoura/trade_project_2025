###############################################################################
# Data Cleaning for CDK (2012) Model - Reorganized
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

# Source the original data cleaning functions
source("data_cleaning.R")

#' Clean and organize all data for CDK analysis
#' @param wiot_file_path Path to WIOD data file
#' @param socioeco_file_path Path to socioeconomic Excel file  
#' @param exchange_rates_file_path Path to exchange rates Excel file
#' @param tariff_file_path Optional path to tariff data file
#' @param target_year Analysis year
#' @param country_groups Named list of country groups for aggregation
#' @param sector_groups Named list of sector groups for aggregation
#' @return Cleaned database containing only relevant data for WIOD and socioeconomic analysis
clean_database <- function(wiot_file_path, socioeco_file_path, exchange_rates_file_path, 
                          tariff_file_path = NULL, target_year = 2011, 
                          country_groups = NULL, sector_groups = NULL) {
    
    cat("Starting data cleaning process...\n")
    cat("Target year:", target_year, "\n")
    
    # Use existing comprehensive data cleaning function (keeping all logic intact)
    raw_cleaned_data <- clean_all_data_cdk(
        wiot_file_path = wiot_file_path,
        socioeco_file_path = socioeco_file_path, 
        exchange_rates_file_path = exchange_rates_file_path,
        tariff_file_path = tariff_file_path,
        target_year = target_year,
        country_groups = country_groups,
        sector_groups = sector_groups
    )
    
    # Extract and organize only the relevant components for analysis
    cleaned_database <- list(
        # Dimension information
        countries = raw_cleaned_data$countries,
        sectors = raw_cleaned_data$sectors,
        
        # Trade and expenditure data (core WIOD data)
        trade_flows_array = raw_cleaned_data$trade_flows_array,
        expenditure_matrix = raw_cleaned_data$expenditure_matrix,
        observed_trade_shares = raw_cleaned_data$observed_trade_shares,
        
        # Input-output structure (WIOD derived)
        beta_matrix = raw_cleaned_data$beta_matrix,           # Labor shares
        gamma_matrix = raw_cleaned_data$gamma_matrix,         # Intermediate input shares
        
        # Labor market data (socioeconomic data)
        labor_matrix = raw_cleaned_data$labor_matrix,
        
        # Policy data
        tariffs = raw_cleaned_data$tariffs,
        
        # Additional reference data
        gdp_data = raw_cleaned_data$gdp_data,
        
        # Data provenance
        target_year = target_year,
        aggregation_applied = list(
            countries = !is.null(country_groups),
            sectors = !is.null(sector_groups)
        )
    )
    
    # Validate cleaned database
    n_countries <- length(cleaned_database$countries)
    n_sectors <- length(cleaned_database$sectors)
    
    # Data quality checks
    quality_checks <- list(
        trade_flows_dimensions = all(dim(cleaned_database$trade_flows_array) == c(n_countries, n_countries, n_sectors)),
        expenditure_dimensions = all(dim(cleaned_database$expenditure_matrix) == c(n_countries, n_sectors)),
        beta_dimensions = all(dim(cleaned_database$beta_matrix) == c(n_countries, n_sectors)),
        gamma_dimensions = all(dim(cleaned_database$gamma_matrix) == c(n_countries, n_sectors, n_sectors)),
        labor_dimensions = all(dim(cleaned_database$labor_matrix) == c(n_countries, n_sectors)),
        tariff_availability = !is.null(cleaned_database$tariffs),
        positive_trade_flows = all(cleaned_database$trade_flows_array >= 0, na.rm = TRUE),
        positive_expenditure = all(cleaned_database$expenditure_matrix >= 0, na.rm = TRUE)
    )
    
    failed_checks <- names(quality_checks)[!unlist(quality_checks)]
    if (length(failed_checks) > 0) {
        warning("Data quality issues detected: ", paste(failed_checks, collapse = ", "))
    }
    
    cat("✓ Data cleaning completed successfully\n")
    cat("  - Countries:", n_countries, "\n")
    cat("  - Sectors:", n_sectors, "\n") 
    cat("  - Tariff data available:", !is.null(cleaned_database$tariffs), "\n")
    cat("  - Quality checks passed:", sum(unlist(quality_checks)), "/", length(quality_checks), "\n")
    
    return(cleaned_database)
}