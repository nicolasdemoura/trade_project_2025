###############################################################################
# Data Processing Functions for CDK (2012) Model - Raw Data Loading and Cleaning
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

# Load required libraries
library(dplyr)
library(haven)
library(readxl)

#' Aggregate countries according to user-defined groups BEFORE data cleaning
#' @param raw_data Raw WIOD data frame
#' @param country_groups Named list where names are new country names and values are vectors of original countries
#' @return Modified data frame with aggregated countries
aggregate_wiod_countries_before_cleaning <- function(wiot_target, country_groups = NULL) {
  
  if (is.null(country_groups)) {
    return(wiot_target)  # No aggregation requested
  }
  
  # Create mapping from original countries to new aggregated countries
  country_mapping <- list()
  
  # Add all grouped countries
  for (group_name in names(country_groups)) {
    for (country in country_groups[[group_name]]) {
      country_mapping[[country]] <- group_name
    }
  }
  
  # Find all unique countries in the data
  all_countries <- unique(c(wiot_target$row_country, wiot_target$col_country))
  
  # Add ungrouped countries (map to themselves)
  for (country in all_countries) {
    if (!country %in% names(country_mapping)) {
      country_mapping[[country]] <- country
    }
  }
  
  # Apply mapping to the data
  modified_data <- wiot_target
  modified_data$row_country <- sapply(wiot_target$row_country, function(x) country_mapping[[x]])
  modified_data$col_country <- sapply(wiot_target$col_country, function(x) country_mapping[[x]])
  
  # Aggregate trade flows for countries that are now the same
  aggregated_data <- modified_data %>%
    group_by(year, row_country, col_country, row_item, col_item) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = 'drop')
  
  return(aggregated_data)
}

#' Aggregate sectors according to user-defined groups BEFORE data cleaning
#' @param wiot_target WIOD data frame
#' @param sector_groups Named list where names are new sector names and values are vectors of original sectors
#' @return Modified data frame with aggregated sectors
aggregate_wiod_sectors_before_cleaning <- function(wiot_target, sector_groups = NULL) {
  
  if (is.null(sector_groups)) {
    return(wiot_target)  # No aggregation requested
  }
  
  # Create mapping from original sectors to new aggregated sectors
  sector_mapping <- list()
  
  # Add all grouped sectors
  for (group_name in names(sector_groups)) {
    for (sector in sector_groups[[group_name]]) {
      sector_mapping[[sector]] <- group_name
    }
  }
  
  # Find all unique sectors in the data
  all_sectors <- unique(c(wiot_target$row_item, wiot_target$col_item))
  
  # Add ungrouped sectors (map to themselves)
  for (sector in all_sectors) {
    if (!sector %in% names(sector_mapping)) {
      sector_mapping[[sector]] <- sector
    }
  }
  
  # Apply mapping to the data
  modified_data <- wiot_target
  modified_data$row_item <- sapply(wiot_target$row_item, function(x) {
    if (x %in% names(sector_mapping)) {
      return(sector_mapping[[x]])
    } else {
      return(x)
    }
  })
  modified_data$col_item <- sapply(wiot_target$col_item, function(x) {
    if (x %in% names(sector_mapping)) {
      return(sector_mapping[[x]])
    } else {
      return(x)
    }
  })
  
  # Aggregate trade flows for sectors that are now the same
  aggregated_data <- modified_data %>%
    group_by(year, row_country, col_country, row_item, col_item) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = 'drop')
  
  return(aggregated_data)
}

#' Load and clean WIOD input-output data
#' @param wiot_file_path Path to the WIOD data file
#' @param target_year Target year for analysis
#' @param country_groups Named list of country groups for aggregation (applied before cleaning)
#' @param sector_groups Named list of sector groups for aggregation (applied before cleaning)
#' @return Raw cleaned WIOD data with basic filtering and aggregation
load_and_clean_wiod_data <- function(wiot_file_path, target_year = 2011, country_groups = NULL, sector_groups = NULL) {
    
    # Load data from cache or file
    if(!file.exists(paste0(wiot_file_path, "_", target_year, ".RData"))) {
      wiot_raw <- read_dta(wiot_file_path)
      wiot_target <- wiot_raw[wiot_raw$year == target_year, ]
      save(wiot_target, file = paste0(wiot_file_path, "_", target_year, ".RData"))
    } else {
      load(paste0(wiot_file_path, "_", target_year, ".RData"))
    }

    # Filter out non-country entities
    wiot_target <- wiot_target[!wiot_target$row_country %in% c("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA") &
                             !wiot_target$col_country %in% c("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA"), ]

    # Filter sectors (keep manageable size)
    wiot_target <- wiot_target[wiot_target$row_item %in% as.character(1:35) & 
                                 wiot_target$col_item %in% as.character(1:35), ]
    wiot_target$row_item <- paste0("c", wiot_target$row_item)
    wiot_target$col_item <- paste0("c", wiot_target$col_item)

    # Apply aggregations
    if (!is.null(country_groups)) {
      wiot_target <- aggregate_wiod_countries_before_cleaning(wiot_target, country_groups)
    }
    
    if (!is.null(sector_groups)) {
      wiot_target <- aggregate_wiod_sectors_before_cleaning(wiot_target, sector_groups)
    }
    
    cat("✓ WIOD data loaded and cleaned\n")
    cat("  - Target year:", target_year, "\n")
    cat("  - Records:", nrow(wiot_target), "\n")
    
    return(wiot_target)
}

#' Aggregate socioeconomic data according to country groups
#' @param socioeco_data Raw socioeconomic data frame  
#' @param country_groups Named list of country groups
#' @return Aggregated socioeconomic data
aggregate_socioeco_countries_before_cleaning <- function(socioeco_data, country_groups = NULL) {
  
  if (is.null(country_groups)) {
    return(socioeco_data)
  }
  
  # Create mapping
  country_mapping <- list()
  for (group_name in names(country_groups)) {
    for (country in country_groups[[group_name]]) {
      country_mapping[[country]] <- group_name
    }
  }
  
  # Add ungrouped countries
  all_countries <- unique(socioeco_data$Country)
  for (country in all_countries) {
    if (!country %in% names(country_mapping)) {
      country_mapping[[country]] <- country
    }
  }
  
  # Apply mapping and aggregate
  modified_data <- socioeco_data
  modified_data$Country <- sapply(socioeco_data$Country, function(x) {
    if (x %in% names(country_mapping)) {
      return(country_mapping[[x]])
    } else {
      return(x)
    }
  })
  
  # Aggregate by new country names
  year_cols <- names(socioeco_data)[grepl("^_\\d{4}$", names(socioeco_data))]
  
  aggregated_data <- modified_data %>%
    group_by(Country, Variable, Code) %>%
    summarise(across(all_of(year_cols), ~ sum(as.numeric(.x), na.rm = TRUE)), .groups = 'drop')
  
  return(aggregated_data)
}

#' Aggregate socioeconomic sectors according to sector groups
#' @param socioeco_data Raw socioeconomic data frame  
#' @param sector_groups Named list of sector groups
#' @return Aggregated socioeconomic data
aggregate_socioeco_sectors_before_cleaning <- function(socioeco_data, sector_groups = NULL) {
  
  if (is.null(sector_groups)) {
    return(socioeco_data)
  }
  
  # Create mapping
  sector_mapping <- list()
  for (group_name in names(sector_groups)) {
    for (sector in sector_groups[[group_name]]) {
      sector_mapping[[sector]] <- group_name
    }
  }
  
  # Add ungrouped sectors
  all_sectors <- unique(socioeco_data$Code)
  for (sector in all_sectors) {
    if (!sector %in% names(sector_mapping)) {
      sector_mapping[[sector]] <- sector
    }
  }
  
  # Apply mapping and aggregate
  modified_data <- socioeco_data
  modified_data$Code <- sapply(socioeco_data$Code, function(x) {
    if (x %in% names(sector_mapping)) {
      return(sector_mapping[[x]])
    } else {
      return(x)
    }
  })
  
  # Aggregate by new sector names
  year_cols <- names(socioeco_data)[grepl("^_\\d{4}$", names(socioeco_data))]
  
  aggregated_data <- modified_data %>%
    group_by(Country, Variable, Code) %>%
    summarise(across(all_of(year_cols), ~ sum(as.numeric(.x), na.rm = TRUE)), .groups = 'drop')
  
  return(aggregated_data)
}

#' Load and clean socioeconomic data
#' @param socioeco_file_path Path to the Excel file with socio-economic data
#' @param exchange_rates_file_path Path to exchange rates Excel file
#' @param target_year Target year for analysis (default: 2011)
#' @param country_groups Named list of country groups for aggregation (applied before cleaning)
#' @param sector_groups Named list of sector groups for aggregation (applied before cleaning)
#' @return Raw cleaned socioeconomic data
load_and_clean_socioeco_data <- function(socioeco_file_path, exchange_rates_file_path, target_year = 2011, country_groups = NULL, sector_groups = NULL) {
    
    key_variables <- c("EMP", "LAB", "COMP", "GO", "VA")
    year_col <- paste0("_", target_year)

    # Load socio-economic data
    socioeco_data <- readxl::read_excel(socioeco_file_path, sheet = 2)
    socioeco_data <- as.data.frame(socioeco_data)
    socioeco_data <- socioeco_data %>%
        mutate(across(starts_with("_"), as.numeric)) %>% 
        filter(Variable %in% key_variables) %>%
        select(Country, Variable, Description, Code, year_col)

    # Load exchange rates data
    exchange_rates <- readxl::read_excel(exchange_rates_file_path, sheet = 2, skip = 3)
    exchange_rates <- as.data.frame(exchange_rates)
    exchange_rates <- exchange_rates %>%
        mutate(across(starts_with("_"), as.numeric)) %>%
      select(Acronym, all_of(year_col)) %>%
      rename(Country = Acronym, exchange_rate = all_of(year_col))

    # Apply exchange rate conversion
    socioeco_data <- socioeco_data %>%
      left_join(exchange_rates, by = "Country") %>%
      mutate(across(starts_with("_"), ~ ifelse(Variable %in% c("LAB", "COMP", "GO", "VA"), .x * exchange_rate, .x))) %>%
      select(-exchange_rate)

    # Apply aggregations BEFORE cleaning
    if (!is.null(country_groups)) {
      socioeco_data <- aggregate_socioeco_countries_before_cleaning(socioeco_data, country_groups)
    }
    
    # Round values to avoid floating point issues
    socioeco_data <- socioeco_data %>%
      mutate(across(starts_with("_"), ~ round(.x, 2)))
    
    # Extract GDP data
    gdp_data <- socioeco_data %>%
      filter(Variable == "GO" & Code == "TOT") %>%
      select(Country, Code, year_col)

    # Remove totals and apply sector aggregation
    socioeco_data <- socioeco_data %>%
      filter(Code != "TOT")

    if (!is.null(sector_groups)) {
      socioeco_data <- aggregate_socioeco_sectors_before_cleaning(socioeco_data, sector_groups)
    }
    
    cat("✓ Socioeconomic data loaded and cleaned\n")
    cat("  - Target year:", target_year, "\n")
    cat("  - Countries with data:", length(unique(socioeco_data$Country)), "\n")
    
    return(list(
        socioeco_data = socioeco_data,
        gdp_data = gdp_data,
        target_year = target_year
    ))
}

#' Load raw tariff data
#' @param tariff_file_path Path to tariff data file (CSV or Excel)
#' @param countries Vector of country names (after aggregation)
#' @param sectors Vector of sector names (after aggregation)
#' @return Raw tariff data frame
load_raw_tariff_data <- function(tariff_file_path = NULL, countries = NULL, sectors = NULL) {
    
    if (is.null(tariff_file_path) || !file.exists(tariff_file_path)) {
        cat("✓ No tariff data file - will use zero baseline\n")
        return(NULL)
    }
    
    # Load tariff data
    tariff_data <- read.csv(tariff_file_path)
    tariff_data <- as.data.frame(tariff_data)
    
    # Basic cleaning
    tariff_data <- tariff_data %>% 
        select(`Reporter.Name`, `Product.Name`, `Partner.Name`, `Weighted.Average`) %>%
        rename(importer = `Reporter.Name`, sector = `Product.Name`, exporter = `Partner.Name`, tariff_rate = `Weighted.Average`)
    
    cat("✓ Raw tariff data loaded\n")
    cat("  - Records:", nrow(tariff_data), "\n")
    
    return(tariff_data)
}

#' Master function to load and clean all raw data for CDK analysis
#' @param wiot_file_path Path to WIOD data file
#' @param socioeco_file_path Path to socioeconomic Excel file  
#' @param exchange_rates_file_path Path to exchange rates Excel file
#' @param tariff_file_path Optional path to tariff data file
#' @param us_tariff_file_path Optional path to new HTS tariff CSV file
#' @param target_year Analysis year
#' @param country_groups Named list of country groups for aggregation
#' @param sector_groups Named list of sector groups for aggregation
#' @return Raw cleaned database containing basic cleaned data
load_and_clean_raw_data <- function(wiot_file_path, socioeco_file_path, exchange_rates_file_path, 
                                   tariff_file_path, us_tariff_file_path, target_year = 2011, 
                                   country_groups = NULL, sector_groups = NULL) {
    
    cat("Starting raw data loading and cleaning process...\n")
    cat("Target year:", target_year, "\n")
    
    # Load WIOD data
    wiod_data <- load_and_clean_wiod_data(wiot_file_path, target_year, country_groups, sector_groups)
    
    # Load socioeconomic data
    socioeco_results <- load_and_clean_socioeco_data(socioeco_file_path, exchange_rates_file_path, target_year, country_groups, sector_groups)
    
    # Extract dimensions from cleaned data
    countries <- sort(unique(c(wiod_data$row_country, wiod_data$col_country)))
    sectors <- sort(unique(socioeco_results$socioeco_data$Code))
    
    # Load tariff data
    tariff_data <- load_raw_tariff_data(tariff_file_path, countries, sectors)
    
    # Load and process new tariff data if available
    us_tariff_scenarios <- process_us_tariff_dataset(us_tariff_file_path)
    
    # Organize raw cleaned data
    raw_database <- list(
        # Raw data components
        wiod_data = wiod_data,
        socioeco_data = socioeco_results$socioeco_data,
        gdp_data = socioeco_results$gdp_data,
        tariff_data = tariff_data,
        us_tariff_scenarios = us_tariff_scenarios,
        
        # Dimensions
        countries = countries,
        sectors = sectors,
        
        # Metadata
        target_year = target_year,
        aggregation_applied = list(
            countries = !is.null(country_groups),
            sectors = !is.null(sector_groups)
        )
    )
    
    cat("✓ Raw data loading completed successfully\n")
    cat("  - Countries:", length(countries), "\n")
    cat("  - Sectors:", length(sectors), "\n") 
    cat("  - Tariff data available:", !is.null(tariff_data), "\n")
    cat("  - US tariff scenarios available:", !is.null(us_tariff_scenarios), "\n")
    
    return(raw_database)
}


#' Load and process new HTS tariff dataset
#' @param file_path Path to the new tariff CSV file
#' @return Processed tariff data frame
load_us_tariff_data <- function(file_path) {
    
    cat("Loading new HTS tariff dataset...\n")
    
    # Load the data
    tariff_data <- read_excel(file_path)
    tariff_data <- as.data.frame(tariff_data)
    
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
        rename(country = Model_Country, sector = Sector)
    
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
process_us_tariff_dataset <- function(file_path) {
    
    cat("=== Processing New Tariff Dataset ===\n")
    
    # Step 1: Load raw data
    raw_data <- load_us_tariff_data(file_path)
    
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

#' Helper function to create and save LaTeX tables using xtable
#' @param data Matrix or data frame to export
#' @param filename Output filename (without path or extension)
#' @param caption Table caption
#' @param label Table label for LaTeX referencing
#' @param output_dir Output directory path
#' @param labor_mobility Logical indicating if labor mobility is considered (affects table formatting)
create_latex_table <- function(data, filename, caption, label, output_dir = "tables", labor_mobility = "NULL") {
  
  # Load xtable library
  if (!require(xtable, quietly = TRUE)) {
  install.packages("xtable")
  library(xtable)
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
  }
  
  # Convert data to numeric matrix if not already
  if (!is.matrix(data)) {
  data <- as.matrix(data)
  }
  
  # Apply color coding to numeric values
  data_colored <- data
  if (is.numeric(data)) {
  # Get all non-NA values
  all_values <- as.vector(data[!is.na(data)])
  
  # Calculate percentiles for inverse cumulative frequency
  percentiles <- ecdf(all_values)
  
  # Apply color gradient based on inverse cumulative frequency
  for (i in 1:nrow(data)) {
  for (j in 1:ncol(data)) {
  if (!is.na(data[i, j])) {
  # Get percentile (inverse cumulative frequency)
  percentile <- percentiles(data[i, j])
  
  # Special handling for true zero values
  if (data[i, j] == 0) {
    # Make true zeros orange
    color_code <- "\\textcolor[RGB]{255,165,0}"
  } else {
    # Create continuous gradient from orange (low) to blue (high)
    # Orange: RGB(255, 165, 0)
    # Blue: RGB(0, 0, 255)
    red_component <- round(255 * (1 - percentile))
    green_component <- round(165 * (1 - percentile))
    blue_component <- round(255 * percentile)
    
    color_code <- paste0("\\textcolor[RGB]{", 
      red_component, ",", 
      green_component, ",", 
      blue_component, "}")
  }
  
  data_colored[i, j] <- paste0(color_code, "{", 
      sprintf("%.3f", data[i, j]), "}")
  }
  }
  }
  }
  
  # Create xtable object
  xt <- xtable(data_colored, 
   caption = paste0(caption, ifelse(labor_mobility == "NULL", "", ifelse(labor_mobility, " (with Labor Mobility)", " (without Labor Mobility)"))), 
   label = label,
   align = paste0("l", paste(rep("c", ncol(data_colored)), collapse = "")))
  
  # Generate LaTeX output
  full_path <- file.path(output_dir, paste0(filename, ifelse(labor_mobility == "NULL", ".tex", ifelse(labor_mobility, "_mobile.tex", "_immobile.tex"))))
  
  # Print to file with LaTeX formatting
  print(xt, 
  file = full_path,
  type = "latex",
  sanitize.text.function = function(x) x,  # Don't escape LaTeX commands
  include.rownames = TRUE,
  include.colnames = TRUE,
  caption.placement = "top",
  table.placement = "htbp")
  
  cat("✓ LaTeX table exported to:", full_path, "\n")
}

export_parameter_tables <- function(processed_database, model_parameters, output_dir = "tables") {
  
  # Export alpha table
  create_latex_table(model_parameters$alpha, 
      "alpha_table", 
      "Expenditure Shares ($\\alpha$)", 
      "tab:alpha", 
      paste0(output_dir, "/alpha"))

  # Export beta table
  create_latex_table(model_parameters$beta, 
      "beta_table", 
      "Labor Shares ($\\beta$)", 
      "tab:beta", 
      paste0(output_dir, "/beta"))

  # Export gamma tables for each country
  for(j in 1:dim(model_parameters$gamma)[1]) {
  country <- rownames(model_parameters$gamma)[j]
  gamma_matrix <- model_parameters$gamma[j,,]
  colnames(gamma_matrix) <- colnames(model_parameters$gamma)
  rownames(gamma_matrix) <- colnames(model_parameters$gamma)
  
  create_latex_table(gamma_matrix,
      paste0("gamma_table_", country),
      paste0("Intermediate Input Shares ($\\gamma$) - ", country),
      paste0("tab:gamma_", country),
      paste0(output_dir, "/gamma"))
  }

  # Export technology table
  create_latex_table(model_parameters$technology, 
      "technology_table", 
      "Technology Parameters", 
      "tab:technology", 
      paste0(output_dir, "/technology"))

  # Export tariff tables for each country
  for(i in 1:dim(processed_database$tariffs)[1]) {
  country <- rownames(processed_database$tariffs)[i]
  tariff_vector <- processed_database$tariffs[i,,]

  create_latex_table(tariff_vector,
      paste0("tariff_table_", country),
      paste0("Tariff Rates - ", country),
      paste0("tab:tariffs_", country),
      paste0(output_dir, "/tariffs"))
  }
}
