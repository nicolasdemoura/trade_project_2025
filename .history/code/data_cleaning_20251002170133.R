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

#' Aggregate sectors according to user-defined groups BEFORE data cleaning
#' @param wiot_target WIOD data frame
#' @param sector_groups Named list where names are new sector names and values are vectors of original sectors
#' @return Modified data frame with aggregated sectors
aggregate_sectors_before_cleaning <- function(wiot_target, sector_groups = NULL) {
  
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
  # Get all year columns (assuming format _YYYY)
  year_cols <- names(socioeco_data)[grepl("^_\\d{4}$", names(socioeco_data))]
  
  aggregated_data <- modified_data %>%
    group_by(Country, Variable, Code) %>%
    summarise(across(all_of(year_cols), ~ sum(as.numeric(.x), na.rm = TRUE)), .groups = 'drop')
  
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
    group_by(Country, Variable, Code) %>%
    summarise(across(all_of(year_cols), ~ sum(as.numeric(.x), na.rm = TRUE)), .groups = 'drop')
  
  return(aggregated_data)
}

#' Clean WIOD input-output data for CDK (2012) analysis
#' @param wiot_file_path Path to the WIOD data file
#' @param country_groups Named list of country groups for aggregation (applied before cleaning)
#' @param sector_groups Named list of sector groups for aggregation (applied before cleaning)
#' @return List with cleaned bilateral trade flows and country/sector info
clean_wiod_data_cdk <- function(wiot_file_path, target_year = 2011, country_groups = NULL, sector_groups = NULL) {
    
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
    # Filter out countries that are  ("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA")]
    wiot_target <- wiot_target[!wiot_target$row_country %in% c("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA") &
                             !wiot_target$col_country %in% c("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA"), ]

    # Filter out sectors whose codes are larger than 35 (to keep manageable size)
    wiot_target <- wiot_target[wiot_target$row_item %in% as.character(1:35) & 
                                 wiot_target$col_item %in% as.character(1:35), ]
    wiot_target$row_item <- paste0("c", wiot_target$row_item)
    wiot_target$col_item <- paste0("c", wiot_target$col_item)

    # Apply country aggregation before cleaning
    if (!is.null(country_groups)) {
      wiot_target <- aggregate_countries_before_cleaning(wiot_target, country_groups)
    }
    
    # Apply sector aggregation before cleaning
    if (!is.null(sector_groups)) {
      wiot_target <- aggregate_sectors_before_cleaning(wiot_target, sector_groups)
    }
    
    # Extract unique countries from row_country and col_country (after aggregation)
    countries <- sort(unique(c(wiot_target$row_country, wiot_target$col_country)))
    sectors <- sort(unique(c(wiot_target$row_item, wiot_target$col_item)))

    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Create trade flows array from actual data
    trade_flows_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                            dimnames = list(countries, countries, sectors))
    
    # Create gamma_{ikk'} matrix: share of sector k' in country i sector k's total intermediate input use
    gamma_matrix <- array(0, dim = c(n_countries, n_sectors, n_sectors),
                          dimnames = list(countries, sectors, sectors))

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
          sector_t <- which(sectors == col_sector)
          
          # Aggregate across destination sectors (sum over col_item)
          trade_flows_array[country_i, country_j, sector_s] <- 
            trade_flows_array[country_i, country_j, sector_s] + flow_value

          # Update gamma matrix
          gamma_matrix[country_i, sector_s, sector_t] <- gamma_matrix[country_i, sector_s, sector_t] + flow_value
      }
    }

    # Reshape, and group by to normalize such that for each country and sector, the shares sum to 1
    gamma_matrix <- as.data.frame.table(gamma_matrix)
    gamma_matrix <- gamma_matrix %>%
      group_by(Var1, Var2) %>%
      mutate(Freq = Freq / sum(Freq, na.rm = TRUE)) %>%
      ungroup()
    gamma_matrix$Freq[is.na(gamma_matrix$Freq)] <- 0
      
    # Convert back to array
    gamma_matrix <- array(gamma_matrix$Freq, dim = c(n_countries, n_sectors, n_sectors),
                         dimnames = list(countries, sectors, sectors))

    return(list(
      trade_flows_array = trade_flows_array,
      gamma_matrix =  gamma_matrix,
      countries = countries,
      sectors = sectors
    ))
}

#' Clean socio-economic accounts data for CDK (2012) analysis
#' @param socioeco_file_path Path to the Excel file with socio-economic data
#' @param target_year Target year for analysis (default: 2011)
#' @param country_groups Named list of country groups for aggregation (applied before cleaning)
#' @param sector_groups Named list of sector groups for aggregation (applied before cleaning)
#' @return List with cleaned labor data and countries with complete data
clean_socioeconomic_data_cdk <- function(socioeco_file_path, exchange_rates_file_path, target_year = 2011, country_groups = NULL, sector_groups = NULL) {
    # EMPE: Number of persons employed (thousands)
    # LAB: Labour compensation (millions of national currency)
    # COMP: Compensation of employees (millions of national currency)
    # VA: Value added (millions of national currency)
    # GO: Gross output (millions of national currency)
    key_variables <- c("EMPE", "LAB", "COMP", "GO", "VA")
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

    socioeco_data <- socioeco_data %>%
      left_join(exchange_rates, by = "Country") %>%
      mutate(across(starts_with("_"), ~ ifelse(Variable %in% c("LAB", "COMP", "GO", "VA"), .x / exchange_rate, .x))) %>%
      select(-exchange_rate)

    # Apply Country Aggregation BEFORE CLEANING
    if (!is.null(country_groups)) {
      socioeco_data <- aggregate_socioeco_before_cleaning(socioeco_data, country_groups)
    }
    
    gdp_data <- socioeco_data %>%
      filter(Variable == "GO" & Code == "TOT") %>%
      select(Country, Code, all_of(year_col))

    socioeco_data <- socioeco_data %>%
      filter(Code != "TOT") # Remove total sector

    # APPLY SECTOR AGGREGATION BEFORE CLEANING
    if (!is.null(sector_groups)) {
      socioeco_data <- aggregate_socioeco_sectors_before_cleaning(socioeco_data, sector_groups)
    }

    
    # Filter countries with complete 2011 data
    countries <- sort(unique(socioeco_data$Country))    
    sectors <- sort(unique(c(socioeco_data$Code)))
    

    data <- socioeco_data %>%
      filter(Code %in% sectors,
             !is.na(.data[[year_col]])) %>%
      select(Country, Variable, Code, all_of(year_col)) %>%
      rename(value = all_of(year_col))
    data$value <- as.numeric(data$value)
    
    # Create labor matrix from actual employment data (EMP)
    emp_data <- data %>%
      filter(Variable == "EMPE") %>%
      select(Country, Code, value) 
    
    # Ensure countries and sectors match
    # Create labor matrix with actual employment shares
    labor_matrix <- emp_data %>%
      filter(Country %in% countries, Code %in% sectors) %>%
      group_by(Country, Code) %>%
      summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = Code, values_from = value, values_fill = 0) %>%
      column_to_rownames(var = "Country") %>%
      as.matrix()

    # Create beta_ik matrix: labor income share of gross value  by country-sector
    lab_data <- data %>%
      filter(Variable == "LAB") %>%
      select(Country, Code, value)
    
    go_data <- data %>%
      filter(Variable == "GO") %>%
      select(Country, Code, value) %>%
      rename(go = value)

    # Merge labor compensation and gross output data
    beta_data <- lab_data %>%
      left_join(go_data, by = c("Country", "Code")) %>%
      mutate(beta = value / go)
    beta_data$beta[is.na(beta_data$beta) | is.infinite(beta_data$beta)] <- 0

    # Create beta matrix (countries x sectors)
    beta_matrix <- beta_data %>%
      group_by(Country, Code) %>%
      summarise(beta = mean(beta, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = Code, values_from = beta) %>%
      column_to_rownames(var = "Country")

    # Remove NaN or Inf values

    return(list(
      gdp_data = gdp_data,
      labor_matrix = labor_matrix,
      beta_matrix = beta_matrix,  
      data = data,
      countries = countries,
      sectors = sectors
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
#' @param sector_groups Named list of sector groups for aggregation (applied before cleaning)
#' @return List with all cleaned and processed data for CDK methodology
clean_all_data_cdk <- function(wiot_file_path, socioeco_file_path, target_year = 2011, country_groups = NULL, sector_groups = NULL) {
  
    # Load and clean WIOD data
    wiod_cleaned <- clean_wiod_data_cdk(wiot_file_path, target_year, country_groups, sector_groups)
    socio_cleaned <- clean_socioeconomic_data_cdk(socioeco_file_path, target_year, country_groups, sector_groups)

    # Get the countires
    countries <- wiod_cleaned$countries
    sectors <- socio_cleaned$sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    

    trade_flows_array <- wiod_cleaned$trade_flows_array
    labor_matrix <- socio_cleaned$labor_matrix
    
    gamma_matrix <- wiod_cleaned$gamma_matrix
    beta_matrix <- socio_cleaned$beta_matrix
    
    # Calculate expenditure (total imports by destination-sector) with new dimensions
    expenditure_matrix <- matrix(0, n_countries, n_sectors,
                                dimnames = list(countries, sectors))

    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            expenditure_matrix[j, s] <- sum(trade_flows_array[, j, s])
        }
    }
    
    # Calculate trade shares with new dimensions
    observed_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors),
                                    dimnames = list(countries, countries, sectors))

    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            if (expenditure_matrix[j, s] > 0) {
                observed_trade_shares[, j, s] <- trade_flows_array[, j, s] / expenditure_matrix[j, s]
            }
        }
    }

    
    
    final_data <- list(
        # Country and sector info 
        countries = countries,  # EU + non-EU countries with complete data
        sectors = sectors,

        # WIOD data 
        trade_flows_array = trade_flows_array,
        observed_trade_shares = observed_trade_shares,
        expenditure_matrix = expenditure_matrix,
        
        # Socio-economic data
        labor_matrix = labor_matrix,
        beta_matrix = beta_matrix,
        gamma_matrix = gamma_matrix,
        data = socio_cleaned$data
    )
    
    return(final_data)
}