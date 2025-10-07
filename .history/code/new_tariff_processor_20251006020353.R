###############################################################################
# New Tariff Dataset Processor for CDK Model Integration
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-06
###############################################################################

library(dplyr)
library(tidyr)

#' Load and process new HTS tariff dataset
#' @param file_path Path to the new tariff CSV file
#' @return Processed tariff data frame
load_new_tariff_data <- function(file_path) {
    
    cat("Loading new HTS tariff dataset...\n")
    
    # Load the data
    tariff_data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    # Clean column names
    names(tariff_data) <- c("HTS_Number", "Year", "Month", "Description", "Country", 
                           "General_Customs_Value", "General_Import_Charges")
    
    # Convert numeric columns
    tariff_data$HTS_Number <- as.numeric(tariff_data$HTS_Number)
    tariff_data$Year <- as.numeric(tariff_data$Year)
    tariff_data$Month <- as.numeric(tariff_data$Month)
    tariff_data$General_Customs_Value <- as.numeric(tariff_data$General_Customs_Value)
    tariff_data$General_Import_Charges <- as.numeric(tariff_data$General_Import_Charges)
    
    # Remove invalid rows
    tariff_data <- tariff_data %>%
        filter(!is.na(HTS_Number), !is.na(General_Customs_Value), !is.na(General_Import_Charges)) %>%
        filter(General_Customs_Value > 0)  # Avoid division by zero
    
    cat("  - Raw records loaded:", nrow(tariff_data), "\n")
    cat("  - Years available:", paste(sort(unique(tariff_data$Year)), collapse = ", "), "\n")
    cat("  - Countries:", length(unique(tariff_data$Country)), "\n")
    
    return(tariff_data)
}

#' Map HTS codes to model sectors using user-defined mapping
#' @param tariff_data Processed tariff data frame
#' @return Data frame with sector mapping added
map_hts_to_sectors <- function(tariff_data) {
    
    cat("Mapping HTS codes to model sectors...\n")
    
    # Define HS-2 to sector mapping
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
    
    # Create reverse mapping: HTS -> Sector
    hts_to_sector <- list()
    for (sector in names(sector_mapping)) {
        for (hts in sector_mapping[[sector]]) {
            hts_to_sector[[as.character(hts)]] <- sector
        }
    }
    
    # Apply mapping
    tariff_data$Sector <- sapply(tariff_data$HTS_Number, function(x) {
        hts_str <- as.character(x)
        if (hts_str %in% names(hts_to_sector)) {
            return(hts_to_sector[[hts_str]])
        } else {
            return("Other")  # Unmapped sectors
        }
    })
    
    # Report mapping results
    mapping_summary <- tariff_data %>%
        group_by(Sector) %>%
        summarise(Records = n(), .groups = 'drop') %>%
        arrange(desc(Records))
    
    cat("  - Sector mapping summary:\n")
    print(mapping_summary)
    
    # Filter out "Other" sectors to keep only mapped sectors
    tariff_data <- tariff_data %>%
        filter(Sector != "Other")
    
    cat("  - Records after filtering:", nrow(tariff_data), "\n")
    
    return(tariff_data)
}

#' Map countries to model country groups
#' @param tariff_data Tariff data with sector mapping
#' @return Data frame with country mapping added
map_countries_to_groups <- function(tariff_data) {
    
    cat("Mapping countries to model groups...\n")
    
    # Define country mapping based on your model structure
    country_mapping <- list(
        "USA" = c("United States"),
        "CHN" = c("China"),
        "JPN" = c("Japan"), 
        "EU" = c("European Union"),
        "CAN" = c("Canada"),
        "MEX" = c("Mexico"),
        "BRA" = c("Brazil"),
        "IND" = c("India"),
        "GBR" = c("United Kingdom"),
        "RoW" = c("Australia", "Indonesia", "South Korea", "Russia", "Turkey", "Taiwan", "RoW", 
                 "Korea", "Taiwan", "Turkey")  # These will all map to RoW
    )
    
    # Create reverse mapping: Original Country -> Model Country
    original_to_model <- list()
    for (model_country in names(country_mapping)) {
        for (original in country_mapping[[model_country]]) {
            original_to_model[[original]] <- model_country
        }
    }
    
    # Apply mapping
    tariff_data$Model_Country <- sapply(tariff_data$Country, function(x) {
        if (x %in% names(original_to_model)) {
            return(original_to_model[[x]])
        } else {
            return("RoW")  # Default to RoW for unmapped countries
        }
    })
    
    # Report country mapping results
    country_summary <- tariff_data %>%
        group_by(Country, Model_Country) %>%
        summarise(Records = n(), .groups = 'drop') %>%
        arrange(Model_Country, desc(Records))
    
    cat("  - Country mapping summary:\n")
    print(country_summary)
    
    return(tariff_data)
}

#' Calculate implied tariff rates for different time windows
#' @param tariff_data Mapped tariff data
#' @return Data frame with calculated tariff rates for each scenario
calculate_tariff_rates <- function(tariff_data) {
    
    cat("Calculating implied tariff rates for different scenarios...\n")
    
    # Aggregate by country-sector-year-month to handle multiple HTS codes per sector
    monthly_data <- tariff_data %>%
        group_by(Model_Country, Sector, Year, Month) %>%
        summarise(
            Total_Customs_Value = sum(General_Customs_Value, na.rm = TRUE),
            Total_Import_Charges = sum(General_Import_Charges, na.rm = TRUE),
            .groups = 'drop'
        ) %>%
        mutate(
            Monthly_Tariff_Rate = Total_Import_Charges / Total_Customs_Value
        ) %>%
        filter(Total_Customs_Value > 0)  # Avoid division by zero
    
    # Create date column for easier filtering
    monthly_data$Date <- as.Date(paste(monthly_data$Year, monthly_data$Month, "01", sep = "-"))
    
    # Calculate different scenarios
    tariff_scenarios <- monthly_data %>%
        group_by(Model_Country, Sector) %>%
        summarise(
            # 2024 full year average
            tariff_rate24 = weighted.mean(
                Monthly_Tariff_Rate[Year == 2024], 
                Total_Customs_Value[Year == 2024], 
                na.rm = TRUE
            ),
            
            # 2025 YTD (January to current available month)
            tariff_rate25_YTD = weighted.mean(
                Monthly_Tariff_Rate[Year == 2025], 
                Total_Customs_Value[Year == 2025], 
                na.rm = TRUE
            ),
            
            # LTM (Last 12 months from latest available date)
            tariff_rate25_LTM = {
                latest_date <- max(Date, na.rm = TRUE)
                ltm_start <- latest_date - 365
                ltm_data <- Monthly_Tariff_Rate[Date >= ltm_start & Date <= latest_date]
                ltm_weights <- Total_Customs_Value[Date >= ltm_start & Date <= latest_date]
                weighted.mean(ltm_data, ltm_weights, na.rm = TRUE)
            },
            
            # Last 3 months
            tariff_rate25_3M = {
                latest_date <- max(Date, na.rm = TRUE)
                three_m_start <- latest_date - 90
                three_m_data <- Monthly_Tariff_Rate[Date >= three_m_start & Date <= latest_date]
                three_m_weights <- Total_Customs_Value[Date >= three_m_start & Date <= latest_date]
                weighted.mean(three_m_data, three_m_weights, na.rm = TRUE)
            },
            
            .groups = 'drop'
        ) %>%
        # Replace NaN with 0 (happens when no data available for time period)
        mutate(across(starts_with("tariff_rate"), ~ ifelse(is.nan(.x), 0, .x)))
    
    # Rename country column to match expected format
    tariff_scenarios <- tariff_scenarios %>%
        rename(Country = Model_Country)
    
    # Report summary statistics
    cat("  - Tariff rate scenarios calculated:\n")
    for (col in grep("tariff_rate", names(tariff_scenarios), value = TRUE)) {
        avg_rate <- mean(tariff_scenarios[[col]], na.rm = TRUE)
        cat("    *", col, "- Average:", round(avg_rate * 100, 2), "%\n")
    }
    
    return(tariff_scenarios)
}

#' Master function to process new tariff dataset
#' @param file_path Path to new tariff CSV file
#' @return Clean dataset with all tariff scenarios
process_new_tariff_dataset <- function(file_path) {
    
    cat("=== Processing New Tariff Dataset ===\n")
    
    # Step 1: Load raw data
    raw_data <- load_new_tariff_data(file_path)
    
    # Step 2: Map HTS codes to sectors
    sector_mapped <- map_hts_to_sectors(raw_data)
    
    # Step 3: Map countries to model groups
    country_mapped <- map_countries_to_groups(sector_mapped)
    
    # Step 4: Calculate tariff rates for different scenarios
    final_dataset <- calculate_tariff_rates(country_mapped)
    
    cat("✓ New tariff dataset processing completed\n")
    cat("  - Final records:", nrow(final_dataset), "\n")
    cat("  - Countries:", length(unique(final_dataset$Country)), "\n")
    cat("  - Sectors:", length(unique(final_dataset$Sector)), "\n")
    
    return(final_dataset)
}

#' Export processed tariff data for use in main workflow
#' @param processed_data Output from process_new_tariff_dataset()
#' @param output_path Path to save processed data
export_processed_tariffs <- function(processed_data, output_path) {
    
    # Save as RData for R workflow
    save(processed_data, file = paste0(output_path, "/processed_tariff_scenarios.RData"))
    
    # Save as CSV for inspection
    write.csv(processed_data, paste0(output_path, "/processed_tariff_scenarios.csv"), row.names = FALSE)
    
    cat("✓ Processed tariff data exported to:\n")
    cat("  - RData:", paste0(output_path, "/processed_tariff_scenarios.RData"), "\n")
    cat("  - CSV:", paste0(output_path, "/processed_tariff_scenarios.csv"), "\n")
}