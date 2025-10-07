###############################################################################
# New Tariff Dataset Processing for CDK (2012) Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-06
###############################################################################

# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lubridate)

#' Create HTS Number to CDK Sector Mapping
#' @return Named vector mapping HTS numbers to CDK sector categories
create_hts_to_sector_mapping <- function() {
    
    # Define sector mappings based on HTS 2-digit codes
    sector_mapping <- list(
        "Chemical" = c(35, 33, 36, 31, 28, 13, 38, 29, 30, 34, 32, 39),
        "Construction" = c(68, 25),
        "Energy" = c(27, 84),
        "Food" = c(15, 22, 10, 18, 9, 4, 8, 7, 3, 1, 6, 2, 21, 12, 19, 16, 20, 5, 11, 23, 17, 24),
        "Manufacture" = c(88, 93, 42, 69, 91, 45, 85, 64, 94, 43, 70, 65, 46, 83, 96, 92, 71, 90, 37, 67, 49, 86, 41, 40, 89, 82, 95, 66, 14, 87, 97),
        "Metal" = c(76, 73, 74, 72, 78, 75, 81, 80),
        "Mining" = c(26),
        "Paper" = c(44, 48, 47),
        "Textiles" = c(61, 62, 57, 52, 59, 60, 54, 55, 63, 53, 50, 58, 56, 51)
    )
    
    # Create reverse mapping: HTS number -> sector
    hts_to_sector <- c()
    for (sector_name in names(sector_mapping)) {
        for (hts_code in sector_mapping[[sector_name]]) {
            hts_to_sector[as.character(hts_code)] <- sector_name
        }
    }
    
    return(hts_to_sector)
}

#' Create Country Mapping for CDK Model
#' @return Named vector mapping tariff dataset countries to CDK country groups
create_country_mapping <- function() {
    
    # Define country mapping based on your CDK model structure
    country_mapping <- c(
        # RoW aggregation
        "Australia" = "RoW",
        "Indonesia" = "RoW", 
        "South Korea" = "RoW",
        "Russia" = "RoW",
        "Turkey" = "RoW",
        "Taiwan" = "RoW",
        
        # EU aggregation (keep major EU countries as "EU")
        "European Union" = "EU",
        "Germany" = "EU",
        "France" = "EU",
        "Italy" = "EU",
        "Spain" = "EU",
        "Netherlands" = "EU",
        "Belgium" = "EU",
        "Austria" = "EU",
        "Poland" = "EU",
        "Sweden" = "EU",
        "Denmark" = "EU",
        "Finland" = "EU",
        "Ireland" = "EU",
        "Portugal" = "EU",
        "Czech Republic" = "EU",
        "Hungary" = "EU",
        "Slovakia" = "EU",
        "Slovenia" = "EU",
        "Estonia" = "EU",
        "Latvia" = "EU",
        "Lithuania" = "EU",
        "Luxembourg" = "EU",
        "Malta" = "EU",
        "Cyprus" = "EU",
        "Bulgaria" = "EU",
        "Romania" = "EU",
        "Croatia" = "EU",
        "Greece" = "EU",
        
        # Individual countries
        "United States" = "USA",
        "China" = "CHN", 
        "Japan" = "JPN",
        "India" = "IND",
        "Brazil" = "BRA",
        "Canada" = "CAN",
        "Mexico" = "MEX",
        "United Kingdom" = "GBR"
    )
    
    return(country_mapping)
}

#' Load and process new tariff dataset
#' @param file_path Path to the new tariff dataset (CSV or Excel)
#' @return Cleaned and processed tariff data frame
load_new_tariff_data <- function(file_path) {
    
    cat("Loading new tariff dataset...\n")
    
    # Load data based on file extension
    if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        tariff_data <- read_csv(file_path)
    } else if (grepl("\\.(xlsx|xls)$", file_path, ignore.case = TRUE)) {
        tariff_data <- read_excel(file_path)
    } else {
        stop("Unsupported file format. Please provide CSV or Excel file.")
    }
    
    # Clean column names
    names(tariff_data) <- c("HTS_Number", "Year", "Month", "Description", "Country", 
                           "General_Customs_Value", "General_Import_Charges")
    
    # Convert data types
    tariff_data <- tariff_data %>%
        mutate(
            HTS_Number = as.numeric(HTS_Number),
            Year = as.numeric(Year),
            Month = as.numeric(Month),
            General_Customs_Value = as.numeric(General_Customs_Value),
            General_Import_Charges = as.numeric(General_Import_Charges)
        ) %>%
        # Remove rows with missing key data
        filter(
            !is.na(HTS_Number),
            !is.na(Year),
            !is.na(Month),
            !is.na(General_Customs_Value),
            !is.na(General_Import_Charges),
            General_Customs_Value > 0  # Avoid division by zero
        )
    
    cat("✓ Tariff data loaded successfully\n")
    cat("  - Records:", nrow(tariff_data), "\n")
    cat("  - Years:", paste(sort(unique(tariff_data$Year)), collapse = ", "), "\n")
    cat("  - Countries:", length(unique(tariff_data$Country)), "\n")
    cat("  - HTS codes:", length(unique(tariff_data$HTS_Number)), "\n")
    
    return(tariff_data)
}

#' Apply sector and country mappings to tariff data
#' @param tariff_data Raw tariff data from load_new_tariff_data()
#' @return Tariff data with CDK sector and country classifications
apply_mappings_to_tariff_data <- function(tariff_data) {
    
    cat("Applying sector and country mappings...\n")
    
    # Get mapping functions
    hts_to_sector <- create_hts_to_sector_mapping()
    country_mapping <- create_country_mapping()
    
    # Apply mappings
    mapped_data <- tariff_data %>%
        mutate(
            # Map HTS numbers to CDK sectors
            CDK_Sector = hts_to_sector[as.character(HTS_Number)],
            # Map countries to CDK country groups
            CDK_Country = country_mapping[Country]
        ) %>%
        # Keep only successfully mapped records
        filter(
            !is.na(CDK_Sector),
            !is.na(CDK_Country)
        )
    
    # Handle unmapped countries by assigning to RoW
    unmapped_countries <- tariff_data %>%
        filter(!(Country %in% names(country_mapping))) %>%
        pull(Country) %>%
        unique()
    
    if (length(unmapped_countries) > 0) {
        cat("  - Unmapped countries assigned to RoW:", paste(unmapped_countries, collapse = ", "), "\n")
        
        # Add unmapped countries to RoW
        tariff_data_updated <- tariff_data %>%
            mutate(
                CDK_Sector = hts_to_sector[as.character(HTS_Number)],
                CDK_Country = case_when(
                    Country %in% names(country_mapping) ~ country_mapping[Country],
                    TRUE ~ "RoW"
                )
            ) %>%
            filter(!is.na(CDK_Sector))
        
        mapped_data <- tariff_data_updated
    }
    
    cat("✓ Mappings applied successfully\n")
    cat("  - Records after mapping:", nrow(mapped_data), "\n")
    cat("  - CDK sectors:", paste(sort(unique(mapped_data$CDK_Sector)), collapse = ", "), "\n")
    cat("  - CDK countries:", paste(sort(unique(mapped_data$CDK_Country)), collapse = ", "), "\n")
    
    return(mapped_data)
}

#' Calculate tariff rates across different time windows
#' @param mapped_tariff_data Tariff data with CDK mappings
#' @param current_date Current date for calculating windows (default: today)
#' @return Data frame with tariff rates for different time windows
calculate_tariff_rates_by_window <- function(mapped_tariff_data, current_date = Sys.Date()) {
    
    cat("Calculating tariff rates across time windows...\n")
    
    # Create date column for easier filtering
    tariff_with_date <- mapped_tariff_data %>%
        mutate(
            Date = ymd(paste(Year, Month, "01", sep = "-")),
            tariff_rate = General_Import_Charges / General_Customs_Value
        )
    
    # Define time windows
    current_year <- year(current_date)
    current_month <- month(current_date)
    
    # Calculate windows
    tariff_windows <- list()
    
    # 2024 full year
    tariff_2024 <- tariff_with_date %>%
        filter(Year == 2024) %>%
        group_by(CDK_Country, CDK_Sector) %>%
        summarise(
            total_customs_value = sum(General_Customs_Value, na.rm = TRUE),
            total_import_charges = sum(General_Import_Charges, na.rm = TRUE),
            tariff_rate24 = total_import_charges / total_customs_value,
            .groups = "drop"
        ) %>%
        select(CDK_Country, CDK_Sector, tariff_rate24)
    
    # 2025 Year-to-Date (YTD)
    tariff_2025_ytd <- tariff_with_date %>%
        filter(Year == 2025, Month <= current_month) %>%
        group_by(CDK_Country, CDK_Sector) %>%
        summarise(
            total_customs_value = sum(General_Customs_Value, na.rm = TRUE),
            total_import_charges = sum(General_Import_Charges, na.rm = TRUE),
            tariff_rate25_YTD = ifelse(total_customs_value > 0, 
                                      total_import_charges / total_customs_value, 
                                      NA_real_),
            .groups = "drop"
        ) %>%
        select(CDK_Country, CDK_Sector, tariff_rate25_YTD)
    
    # Last 12 Months (LTM) - from current date back 12 months
    ltm_start_date <- current_date %m-% months(12)
    tariff_ltm <- tariff_with_date %>%
        filter(Date >= ltm_start_date, Date <= current_date) %>%
        group_by(CDK_Country, CDK_Sector) %>%
        summarise(
            total_customs_value = sum(General_Customs_Value, na.rm = TRUE),
            total_import_charges = sum(General_Import_Charges, na.rm = TRUE),
            tariff_rate25_LTM = ifelse(total_customs_value > 0, 
                                      total_import_charges / total_customs_value, 
                                      NA_real_),
            .groups = "drop"
        ) %>%
        select(CDK_Country, CDK_Sector, tariff_rate25_LTM)
    
    # Last 3 Months
    three_months_start <- current_date %m-% months(3)
    tariff_3m <- tariff_with_date %>%
        filter(Date >= three_months_start, Date <= current_date) %>%
        group_by(CDK_Country, CDK_Sector) %>%
        summarise(
            total_customs_value = sum(General_Customs_Value, na.rm = TRUE),
            total_import_charges = sum(General_Import_Charges, na.rm = TRUE),
            tariff_rate25_3M = ifelse(total_customs_value > 0, 
                                     total_import_charges / total_customs_value, 
                                     NA_real_),
            .groups = "drop"
        ) %>%
        select(CDK_Country, CDK_Sector, tariff_rate25_3M)
    
    # Create complete country-sector grid for merging
    all_countries <- unique(mapped_tariff_data$CDK_Country)
    all_sectors <- unique(mapped_tariff_data$CDK_Sector)
    
    complete_grid <- expand_grid(
        CDK_Country = all_countries,
        CDK_Sector = all_sectors
    )
    
    # Merge all time windows
    final_tariff_rates <- complete_grid %>%
        left_join(tariff_2024, by = c("CDK_Country", "CDK_Sector")) %>%
        left_join(tariff_2025_ytd, by = c("CDK_Country", "CDK_Sector")) %>%
        left_join(tariff_ltm, by = c("CDK_Country", "CDK_Sector")) %>%
        left_join(tariff_3m, by = c("CDK_Country", "CDK_Sector")) %>%
        # Replace NA with 0 for missing tariff rates
        mutate(
            tariff_rate24 = ifelse(is.na(tariff_rate24), 0, tariff_rate24),
            tariff_rate25_YTD = ifelse(is.na(tariff_rate25_YTD), 0, tariff_rate25_YTD),
            tariff_rate25_LTM = ifelse(is.na(tariff_rate25_LTM), 0, tariff_rate25_LTM),
            tariff_rate25_3M = ifelse(is.na(tariff_rate25_3M), 0, tariff_rate25_3M)
        ) %>%
        # Rename columns for consistency
        rename(
            Country = CDK_Country,
            Sector = CDK_Sector
        )
    
    cat("✓ Tariff rates calculated successfully\n")
    cat("  - Country-Sector combinations:", nrow(final_tariff_rates), "\n")
    cat("  - Average 2024 tariff rate:", round(mean(final_tariff_rates$tariff_rate24, na.rm = TRUE), 4), "\n")
    cat("  - Average 2025 YTD tariff rate:", round(mean(final_tariff_rates$tariff_rate25_YTD, na.rm = TRUE), 4), "\n")
    
    return(final_tariff_rates)
}

#' Convert tariff rates to CDK model format (3D array)
#' @param tariff_rates_df Data frame with tariff rates by country-sector
#' @param countries Vector of CDK country names
#' @param sectors Vector of CDK sector names
#' @param tariff_column Name of tariff rate column to use
#' @return 3D array (origin x destination x sector) for CDK model
convert_tariff_rates_to_cdk_format <- function(tariff_rates_df, countries, sectors, tariff_column = "tariff_rate24") {
    
    cat("Converting tariff rates to CDK 3D array format...\n")
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Initialize tariff array (origin x destination x sector)
    tariff_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                         dimnames = list(countries, countries, sectors))
    
    # Fill tariff array with rates (imports to each destination)
    for (i in 1:nrow(tariff_rates_df)) {
        row_data <- tariff_rates_df[i, ]
        
        dest_country <- row_data$Country
        sector <- row_data$Sector
        tariff_rate <- row_data[[tariff_column]]
        
        # Find indices
        dest_idx <- which(countries == dest_country)
        sector_idx <- which(sectors == sector)
        
        if (length(dest_idx) > 0 && length(sector_idx) > 0) {
            # Apply tariff to all origins importing to this destination-sector
            tariff_array[, dest_idx, sector_idx] <- tariff_rate
        }
    }
    
    # Set domestic trade to zero tariffs
    for (i in 1:n_countries) {
        tariff_array[i, i, ] <- 0
    }
    
    cat("✓ Tariff array created successfully\n")
    cat("  - Dimensions:", paste(dim(tariff_array), collapse = " x "), "\n")
    cat("  - Non-zero tariff rate entries:", sum(tariff_array > 0), "\n")
    cat("  - Average tariff rate:", round(mean(tariff_array[tariff_array > 0], na.rm = TRUE), 4), "\n")
    
    return(tariff_array)
}

#' Process complete pipeline for new tariff data
#' @param tariff_file_path Path to new tariff dataset
#' @param cdk_countries Vector of CDK country names
#' @param cdk_sectors Vector of CDK sector names  
#' @param current_date Current date for time window calculations
#' @return List containing processed tariff data and arrays for each time window
process_new_tariff_pipeline <- function(tariff_file_path, cdk_countries, cdk_sectors, current_date = Sys.Date()) {
    
    cat("=== PROCESSING NEW TARIFF DATASET ===\n")
    
    # Step 1: Load raw tariff data
    raw_tariff_data <- load_new_tariff_data(tariff_file_path)
    
    # Step 2: Apply mappings
    mapped_tariff_data <- apply_mappings_to_tariff_data(raw_tariff_data)
    
    # Step 3: Calculate tariff rates by time windows
    tariff_rates_df <- calculate_tariff_rates_by_window(mapped_tariff_data, current_date)
    
    # Step 4: Convert to CDK model format for each time window
    tariff_arrays <- list(
        tariff_2024 = convert_tariff_rates_to_cdk_format(
            tariff_rates_df, cdk_countries, cdk_sectors, "tariff_rate24"
        ),
        tariff_2025_ytd = convert_tariff_rates_to_cdk_format(
            tariff_rates_df, cdk_countries, cdk_sectors, "tariff_rate25_YTD"
        ),
        tariff_2025_ltm = convert_tariff_rates_to_cdk_format(
            tariff_rates_df, cdk_countries, cdk_sectors, "tariff_rate25_LTM"
        ),
        tariff_2025_3m = convert_tariff_rates_to_cdk_format(
            tariff_rates_df, cdk_countries, cdk_sectors, "tariff_rate25_3M"
        )
    )
    
    cat("✓ New tariff processing pipeline completed successfully\n")
    
    return(list(
        raw_data = raw_tariff_data,
        mapped_data = mapped_tariff_data,
        tariff_rates_df = tariff_rates_df,
        tariff_arrays = tariff_arrays
    ))
}

#' Run multiple counterfactual scenarios using different tariff time windows
#' @param baseline_equilibrium Baseline equilibrium from calibration
#' @param new_tariff_results Results from process_new_tariff_pipeline()
#' @param database Processed database for CDK model
#' @param parameters CDK model parameters
#' @param labor_mobility Labor mobility assumption
#' @return List of counterfactual results for each time window
run_multiple_tariff_counterfactuals <- function(baseline_equilibrium, new_tariff_results, 
                                               database, parameters, labor_mobility = TRUE) {
    
    cat("=== RUNNING MULTIPLE TARIFF COUNTERFACTUALS ===\n")
    
    # Get tariff arrays
    tariff_arrays <- new_tariff_results$tariff_arrays
    
    # List to store results
    counterfactual_results <- list()
    
    # Run counterfactual for each time window
    for (scenario_name in names(tariff_arrays)) {
        
        cat(sprintf("Running counterfactual for %s...\n", scenario_name))
        
        # Create counterfactual parameters with new tariffs
        counterfactual_params <- parameters
        counterfactual_params$tariffs <- tariff_arrays[[scenario_name]]
        
        # Run counterfactual analysis
        counterfactual_solution <- run_counterfactual(
            baseline_equilibrium = baseline_equilibrium,
            new_tariffs = tariff_arrays[[scenario_name]],
            database = database,
            parameters = parameters,
            labor_mobility = labor_mobility
        )
        
        # Store results
        counterfactual_results[[scenario_name]] <- counterfactual_solution
        
        if (counterfactual_solution$converged) {
            cat(sprintf("✓ %s counterfactual converged in %d iterations\n", 
                       scenario_name, counterfactual_solution$iterations))
        } else {
            cat(sprintf("✗ %s counterfactual failed to converge\n", scenario_name))
        }
    }
    
    cat("✓ Multiple counterfactual analysis completed\n")
    
    return(counterfactual_results)
}

#' Analyze welfare effects across multiple tariff scenarios
#' @param baseline_outcomes Baseline model outcomes
#' @param multiple_counterfactual_results Results from run_multiple_tariff_counterfactuals()
#' @param countries Vector of country names
#' @return Comprehensive welfare analysis across scenarios
analyze_multiple_scenario_welfare <- function(baseline_outcomes, multiple_counterfactual_results, countries) {
    
    cat("=== ANALYZING WELFARE ACROSS MULTIPLE SCENARIOS ===\n")
    
    welfare_results <- list()
    
    for (scenario_name in names(multiple_counterfactual_results)) {
        
        cat(sprintf("Analyzing welfare effects for %s...\n", scenario_name))
        
        counterfactual_outcomes <- multiple_counterfactual_results[[scenario_name]]
        
        welfare_analysis <- analyze_welfare_effects_cdk(
            baseline_outcomes = baseline_outcomes,
            counterfactual_outcomes = counterfactual_outcomes,
            countries = countries
        )
        
        welfare_results[[scenario_name]] <- welfare_analysis
    }
    
    # Create comparative summary
    comparative_summary <- data.frame()
    
    for (scenario_name in names(welfare_results)) {
        scenario_summary <- welfare_results[[scenario_name]]$welfare_summary
        scenario_row <- data.frame(
            scenario = scenario_name,
            countries_better_off = scenario_summary$countries_better_off,
            countries_worse_off = scenario_summary$countries_worse_off,
            avg_welfare_change_pct = scenario_summary$average_welfare_change,
            median_welfare_change_pct = scenario_summary$median_welfare_change,
            max_welfare_gain_pct = scenario_summary$max_welfare_gain,
            max_welfare_loss_pct = scenario_summary$max_welfare_loss
        )
        comparative_summary <- rbind(comparative_summary, scenario_row)
    }
    
    cat("✓ Multiple scenario welfare analysis completed\n")
    
    return(list(
        individual_results = welfare_results,
        comparative_summary = comparative_summary
    ))
}