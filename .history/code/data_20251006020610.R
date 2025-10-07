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
    cat("  - New tariff scenarios available:", !is.null(us_tariff_scenarios), "\n")
    
    return(raw_database)
}