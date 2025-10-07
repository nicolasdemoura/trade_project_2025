###############################################################################
# Data Cleaning for CDK (2012) Multi-Sector Ricardian Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################


#' Aggregate countries according to user-defined groups BEFORE data cleaning
#' @param raw_data Raw WIOD data frame
#' @param country_groups Named list where names are new country names and values are vectors of original countries
#' @return Modified data frame with aggregated countries
aggregate_countries_before_cleaning <- function(raw_data, country_groups = NULL) {
  
  if (is.null(country_groups)) {
    return(raw_data)  # No aggregation requested
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
  all_countries <- unique(c(raw_data$row_country, raw_data$col_country))
  
  # Add ungrouped countries (map to themselves)
  for (country in all_countries) {
    if (!country %in% names(country_mapping)) {
      country_mapping[[country]] <- country
    }
  }
  
  # Apply mapping to the data
  modified_data <- raw_data
  modified_data$row_country <- sapply(raw_data$row_country, function(x) country_mapping[[x]])
  modified_data$col_country <- sapply(raw_data$col_country, function(x) country_mapping[[x]])
  
  # Aggregate trade flows for countries that are now the same
  aggregated_data <- modified_data %>%
    group_by(year, row_country, col_country, row_item, col_item) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = 'drop')
  
  return(aggregated_data)
}

#' Aggregate socioeconomic data according to country groups
#' @param socio_data Raw socioeconomic data frame  
#' @param country_groups Named list of country groups
#' @return Aggregated socioeconomic data
aggregate_socioeco_before_cleaning <- function(socio_data, country_groups = NULL) {
  
  if (is.null(country_groups)) {
    return(socio_data)
  }
  
  # Create mapping
  country_mapping <- list()
  for (group_name in names(country_groups)) {
    for (country in country_groups[[group_name]]) {
      country_mapping[[country]] <- group_name
    }
  }
  
  # Add ungrouped countries
  all_countries <- unique(socio_data$Country)
  for (country in all_countries) {
    if (!country %in% names(country_mapping)) {
      country_mapping[[country]] <- country
    }
  }
  
  # Apply mapping and aggregate
  modified_data <- socio_data
  modified_data$Country <- sapply(socio_data$Country, function(x) {
    if (x %in% names(country_mapping)) {
      return(country_mapping[[x]])
    } else {
      return(x)
    }
  })
  
  # Aggregate by new country names
  # Get all year columns (assuming format _YYYY)
  year_cols <- names(socio_data)[grepl("^_\\d{4}$", names(socio_data))]
  
  aggregated_data <- modified_data %>%
    group_by(Country, Variable, Code) %>%
    summarise(across(all_of(year_cols), ~sum(.x, na.rm = TRUE)), .groups = 'drop')
  
  return(aggregated_data)
}

#' Ensure trade balance: total exports = total imports
#' @param trade_data Cleaned trade data with trade_flows_array
#' @return Trade data with balanced trade flows
balance_trade_flows <- function(trade_data) {
  
  trade_flows <- trade_data$trade_flows_array
  countries <- dimnames(trade_flows)[[1]]
  sectors <- dimnames(trade_flows)[[3]]
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Calculate total exports and imports by country-sector
  total_exports <- apply(trade_flows, c(1, 3), sum)  # Sum over destinations
  total_imports <- apply(trade_flows, c(2, 3), sum)  # Sum over origins
  
  # Global totals
  global_exports <- sum(total_exports)
  global_imports <- sum(total_imports)
  
  cat("Before balancing:\n")
  cat("Global exports:", global_exports, "\n")
  cat("Global imports:", global_imports, "\n")
  cat("Trade imbalance:", global_exports - global_imports, "\n")
  
  # Method: Proportional adjustment to ensure global balance
  # Adjust all trade flows proportionally so that global exports = global imports
  if (global_exports != global_imports && global_exports > 0) {
    target_total <- (global_exports + global_imports) / 2
    adjustment_factor <- target_total / global_exports
    
    # Apply adjustment
    balanced_flows <- trade_flows * adjustment_factor
    
    # Verify balance
    new_exports <- sum(apply(balanced_flows, c(1, 3), sum))
    new_imports <- sum(apply(balanced_flows, c(2, 3), sum))
    
    cat("After balancing:\n")
    cat("Global exports:", new_exports, "\n")
    cat("Global imports:", new_imports, "\n")
    cat("Trade imbalance:", new_exports - new_imports, "\n")
    
    # Update the trade data
    trade_data$trade_flows_array <- balanced_flows
    
    # Recalculate absorption matrix and trade shares
    absorption_matrix <- matrix(0, n_countries, n_sectors,
                               dimnames = list(countries, sectors))
    
    for (s in 1:n_sectors) {
      for (j in 1:n_countries) {
        absorption_matrix[j, s] <- sum(balanced_flows[, j, s])
      }
    }
    
    # Recalculate trade shares
    observed_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors),
                                   dimnames = list(countries, countries, sectors))
    
    for (s in 1:n_sectors) {
      for (j in 1:n_countries) {
        if (absorption_matrix[j, s] > 0) {
          observed_trade_shares[, j, s] <- balanced_flows[, j, s] / absorption_matrix[j, s]
        }
      }
    }
    
    trade_data$absorption_matrix <- absorption_matrix
    trade_data$observed_trade_shares <- observed_trade_shares
  }
  
  return(trade_data)
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
    if (target_year == 2011) {
      wiot_target <- wiot_2011
    } else {
      # Handle other years if needed
      wiot_target <- get(paste0("wiot_", target_year))
    }
  }
  
  # APPLY COUNTRY AGGREGATION BEFORE CLEANING
  if (!is.null(country_groups)) {
    wiot_target <- aggregate_countries_before_cleaning(wiot_target, country_groups)
  }
  
  # Extract unique countries from row_country and col_country (after aggregation)
  countries <- sort(unique(c(wiot_target$row_country, wiot_target$col_country)))
  countries <- countries[!countries %in% c("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA", "RoW")]
  
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
        select(Country, Code, value_2011) %>%
        spread(Code, value_2011, fill = 0)
    
    # Ensure countries and sectors match
    available_countries <- intersect(countries, emp_data$Country)
    available_sectors <- intersect(sectors, names(emp_data)[-1])
    
    # Create labor matrix with actual employment shares
    labor_matrix <- matrix(0, length(available_countries), length(available_sectors),
                          dimnames = list(available_countries, available_sectors))
    
    for (i in seq_along(available_countries)) {
        country <- available_countries[i]
        country_data <- emp_data[emp_data$Country == country, available_sectors, drop = FALSE]
        country_emp <- as.numeric(country_data[1, ])
        country_emp[is.na(country_emp)] <- 0
        labor_matrix[i, ] <- country_emp / sum(country_emp)
    }
    
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
    labor_matrix <- socio_cleaned$labor_matrix[socio_indices, , drop = FALSE]
    gdp_vector <- socio_cleaned$gdp_vector[socio_indices]
    
    countries <- common_countries
    sectors <- socio_cleaned$sectors
    beta_vec <- socio_cleaned$beta_vec
    
    # Calculate absorption (total imports by destination-sector) with new dimensions
    n_countries <- length(countries)
    n_sectors <- length(sectors)

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
    
    # Create preliminary data structure for trade balancing
    preliminary_data <- list(
        trade_flows_array = trade_flows_array,
        observed_trade_shares = observed_trade_shares,
        absorption_matrix = absorption_matrix
    )
    
    # BALANCE TRADE FLOWS: Ensure total exports = total imports
    balanced_data <- balance_trade_flows(preliminary_data)
    
    # Extract balanced results
    trade_flows_array <- balanced_data$trade_flows_array
    observed_trade_shares <- balanced_data$observed_trade_shares  
    absorption_matrix <- balanced_data$absorption_matrix
    
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