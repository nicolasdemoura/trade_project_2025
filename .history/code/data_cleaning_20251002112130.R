###############################################################################
# Data Cleaning for CDK (2012) Multi-Sector Ricardian Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

#' Aggregate countries according to user-defined groups BEFORE data cleaning
#' @param raw_data Raw WIOD data frame
#' @param country_groups Named list where names are new country names and values are vectors of original countries
#' @return Modified data frame with aggregated countries
aggregate_countries_before_cleaning <- function(wiot_target, country_groups = NULL) {
  
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

#' Aggregate socioeconomic data according to country groups
#' @param socioeco_data Raw socioeconomic data frame  
#' @param country_groups Named list of country groups
#' @return Aggregated socioeconomic data
aggregate_socioeco_before_cleaning <- function(socioeco_data, country_groups = NULL) {
  
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
  # Get all year columns (assuming format _YYYY)
  year_cols <- names(socioeco_data)[grepl("^_\\d{4}$", names(socioeco_data))]
  
  aggregated_data <- modified_data %>%
    group_by(Country, Variable, Description, Code) %>%
    summarise(across(all_of(year_cols), ~ sum(as.numeric(.x), na.rm = TRUE)), .groups = 'drop')
  
  return(aggregated_data)
}

#' Clean WIOD input-output data for CDK (2012) analysis
#' @param wiot_file_path Path to the WIOD data file
#' @param country_groups Named list of country groups for aggregation (applied before cleaning)
#' @return List with cleaned bilateral trade flows and country/sector info
clean_wiod_data_cdk <- function(wiot_file_path, target_year = 2011, country_groups = NULL) {
  
  if(!file.exists(paste0(wiot_file_path, "_", target_year, ".RData"))) {
    # Load WIOD data from file
    wiot_raw <- read_dta(wiot_file_path)
    
    wiot_target <- wiot_raw[wiot_raw$year == target_year, ]
    
    # Export target year data for RData
    save(wiot_target, file = paste0(wiot_file_path, "_", target_year, ".RData"))
  } else {
    # Load WIOD data from pre-saved RData
    load(paste0(wiot_file_path, "_", target_year, ".RData"))
  }
  
  # APPLY COUNTRY AGGREGATION BEFORE CLEANING
  if (!is.null(country_groups)) {
    wiot_target <- aggregate_countries_before_cleaning(wiot_target, country_groups)
  }
  
  # Extract unique countries from row_country and col_country (after aggregation)
  countries <- sort(unique(c(wiot_target$row_country, wiot_target$col_country)))
  countries <- countries[!countries %in% c("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA")]
  
  # Extract unique sectors from row_item and col_item (after aggregation)
  sectors <- sort(unique(c(wiot_target$row_item, wiot_target$col_item)))

  if (length(sectors) > 35) {
    sectors <- sectors[1:35]
  }
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Create trade flows array from actual data
  trade_flows_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                           dimnames = list(countries, countries, sectors))
  
  for (i in 1:nrow(wiot_target)) {
    row_country <- wiot_target$row_country[i]
    col_country <- wiot_target$col_country[i] 
    row_sector <- wiot_target$row_item[i]
    col_sector <- wiot_target$col_item[i]
    flow_value <- wiot_target$value[i]
    
    # Check if this is an intermediate flow (sector to sector)
    if (row_country %in% countries && 
        col_country %in% countries && 
        row_sector %in% sectors &&
        col_sector %in% sectors &&
        !is.na(flow_value)) {
      
      # This represents flow from row_country producing row_sector
      # to col_country for use in col_sector production
      # For CDK model, we need flows by origin-destination-sector
      
      country_i <- which(countries == row_country)
      country_j <- which(countries == col_country)
      sector_s <- which(sectors == row_sector)
      
      # Aggregate across destination sectors (sum over col_item)
      trade_flows_array[country_i, country_j, sector_s] <- 
        trade_flows_array[country_i, country_j, sector_s] + flow_value
    }
  }
  
  return(list(
    trade_flows_array = trade_flows_array,
    countries = countries,
    sectors = sectors
  ))
}

#' Clean socio-economic accounts data for CDK (2012) analysis
#' @param socioeco_file_path Path to the Excel file with socio-economic data
#' @param target_year Target year for analysis (default: 2011)
#' @param country_groups Named list of country groups for aggregation (applied before cleaning)
#' @return List with cleaned labor data and countries with complete data
clean_socioeconomic_data_cdk <- function(socioeco_file_path, target_year = 2011, country_groups = NULL) {
    
    socioeco_data <- readxl::read_excel(socioeco_file_path, sheet = 2)
    socioeco_data <- as.data.frame(socioeco_data)
    socioeco_data <- socioeco_data %>%
        filter(Code != "TOT") # Remove total sector
    
    # APPLY COUNTRY AGGREGATION BEFORE CLEANING
    if (!is.null(country_groups)) {
        socioeco_data <- aggregate_socioeco_before_cleaning(socioeco_data, country_groups)
    }

    year_col <- paste0("_", target_year)
    
    # Filter countries with complete 2011 data
    countries <- sort(unique(socioeco_data$Country))    
    sectors <- sort(unique(c(socioeco_data$Code)))
    n_countries <- length(countries)
    n_sectors <- length(sectors)

    # EMP: Number of persons engaged (thousands)
    # LAB: Labour compensation (millions of national currency)
    # COMP: Compensation of employees (millions of national currency)
    
    key_variables <- c("EMP", "EMPE", "LAB", "COMP", "GO", "VA")
    
    employment_data <- socioeco_data %>%
        filter(Variable %in% key_variables,
            Code %in% sectors,
            !is.na(.data[[year_col]])) %>%
        select(Country, Variable, Code, all_of(year_col)) %>%
        rename(value_2011 = all_of(year_col))
    employment_data$value_2011 <- as.numeric(employment_data$value_2011)
    
    # Create labor matrix from actual employment data (EMP)
    emp_data <- employment_data %>%
        filter(Variable == "EMP") %>%
        select(Country, Code, value_2011) 
    
    # Ensure countries and sectors match
    available_countries <- intersect(countries, emp_data$Country)
    available_sectors <- intersect(sectors, emp_data$Code)
    
    # Create labor matrix with actual employment shares
    labor_matrix <- emp_data %>%
        filter(Country %in% available_countries, Code %in% available_sectors) %>%
        group_by(Country, Code) %>%
        summarise(value_2011 = sum(value_2011, na.rm = TRUE), .groups = 'drop') %>%
        pivot_wider(names_from = Code, values_from = value_2011, values_fill = 0) %>%
        column_to_rownames(var = "Country") %>%
        as.matrix()

    # Create expenditure shares (β_s) from labor compensation data
    # Use LAB (Labour compensation) to calculate sectoral expenditure shares
    lab_data <- employment_data %>%
        filter(Variable == "LAB") %>%
        dplyr::select(Country, Code, value_2011) %>%
        group_by(Code) %>%
        summarise(total_sector_compensation = sum(value_2011, na.rm = TRUE), .groups = 'drop')
    
    # Calculate beta_vec as share of total labor compensation by sector
    total_compensation <- sum(lab_data$total_sector_compensation, na.rm = TRUE)
    beta_vec <- lab_data$total_sector_compensation / total_compensation
    names(beta_vec) <- lab_data$Code       
    
    # Update countries and sectors to match available data
    countries <- available_countries
    sectors <- available_sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)

    # Calculate GDP from Gross Output (GO) by country
    gdp_data <- socioeco_data %>%
        filter(Variable == "GO",  # Use Gross Output as GDP measure
            Country %in% countries,
            Code %in% sectors,
            !is.na(.data[[year_col]])) %>%
        select(Country, Code, all_of(year_col)) %>%
        rename(value_2011 = all_of(year_col)) %>%
        group_by(Country) %>%
        summarise(gdp = sum(as.numeric(value_2011), na.rm = TRUE), .groups = 'drop')
    
    # Create GDP vector matching countries order
    gdp_vector <- numeric(n_countries)
    names(gdp_vector) <- countries
    for (i in seq_along(countries)) {
        country_gdp <- gdp_data[gdp_data$Country == countries[i], "gdp"]
        if (nrow(country_gdp) > 0 && !is.na(country_gdp$gdp[1])) {
            gdp_vector[i] <- country_gdp$gdp[1]
        }
    }
    
    return(list(
        labor_matrix = labor_matrix,
        beta_vec = beta_vec,
        countries = countries,
        sectors = sectors,
        employment_data = employment_data,
        gdp_vector = gdp_vector
    ))
}

###############################################################################
# Main data cleaning workflow
###############################################################################

#' Master function to clean all data for CDK (2012) analysis
#' @param wiot_file_path Path to the WIOD data file
#' @param socioeco_file_path Path to socio-economic Excel file
#' @param target_year Analysis year
#' @param country_groups Named list of country groups for aggregation (applied before cleaning)
#' @return List with all cleaned and processed data for CDK methodology
clean_all_data_cdk <- function(wiot_file_path, socioeco_file_path, target_year = 2011, country_groups = NULL) {

    wiod_cleaned <- clean_wiod_data_cdk(wiot_file_path, target_year, country_groups)
    socio_cleaned <- clean_socioeconomic_data_cdk(socioeco_file_path, target_year, country_groups)

    # Country aggregation already applied in individual cleaning functions
    # Use intersection of countries with both WIOD and socioeco data
    common_countries <- intersect(wiod_cleaned$countries, socio_cleaned$countries)
    
    # Filter data to common countries
    wiod_indices <- which(wiod_cleaned$countries %in% common_countries)  
    socio_indices <- which(socio_cleaned$countries %in% common_countries)
    
    trade_flows_array <- wiod_cleaned$trade_flows_array[wiod_indices, wiod_indices, ]
    labor_matrix <- socio_cleaned$labor_matrix
    gdp_vector <- socio_cleaned$gdp_vector[socio_indices]
    
    countries <- common_countries
    sectors <- socio_cleaned$sectors
    beta_vec <- socio_cleaned$beta_vec
    
    # Calculate absorption (total imports by destination-sector) with new dimensions
    n_countries <- length(countries)
    
    # Use sectors from WIOD trade data to ensure dimension compatibility
    wiod_sectors <- dimnames(trade_flows_array)[[3]]
    n_sectors <- length(wiod_sectors)
    sectors <- wiod_sectors

    absorption_matrix <- matrix(0, n_countries, n_sectors,
                                dimnames = list(countries, sectors))

    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            absorption_matrix[j, s] <- sum(trade_flows_array[, j, s])
        }
    }
    
    # Calculate trade shares with new dimensions
    observed_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors),
                                    dimnames = list(countries, countries, sectors))

    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            if (absorption_matrix[j, s] > 0) {
                observed_trade_shares[, j, s] <- trade_flows_array[, j, s] / absorption_matrix[j, s]
            }
        }
    }

    # Create balanced data structure for trade balancing
    balanced_data <- list(
        trade_flows_array = trade_flows_array,
        observed_trade_shares = observed_trade_shares,
        absorption_matrix = absorption_matrix
    )
    
    
    final_data <- list(
        # WIOD data (EU-aggregated and filtered)
        trade_flows_array = trade_flows_array,
        observed_trade_shares = observed_trade_shares,
        absorption_matrix = absorption_matrix,
        
        # Country and sector info (EU-aggregated)
        countries = countries,  # EU + non-EU countries with complete data
        sectors = sectors,
        
        # Socio-economic data (EU-aggregated)
        labor_matrix = labor_matrix,
        beta_vec = beta_vec,
        employment_data = socio_cleaned$employment_data,
        gdp_vector = gdp_vector,  # Proper GDP from gross output
        
        # Additional metadata
        eu_countries_included = EU_COUNTRIES,
        aggregation_method = "EU_countries_aggregated"
    )
    
    return(final_data)
}