###############################################################################
# Data Cleaning for CDK (2012) Multi-Sector Ricardian Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

###############################################################################
# Clean WIOD 2011 data for CDK (2012) methodology
###############################################################################

#' Clean WIOD input-output data for CDK (2012) analysis
#' @param wiot_file_path Path to the WIOD data file
#' @return List with cleaned bilateral trade flows and country/sector info
clean_wiod_data_cdk <- function(wiot_file_path) {
  
  # Load WIOD data from file
  wiot_raw <- read_dta(wiot_file_path)
  
  wiot_2011 <- wiot_raw[wiot_raw$year == 2011, ]
  
  # Extract unique countries from row_country and col_country
  countries <- sort(unique(c(wiot_2011$row_country, wiot_2011$col_country)))
  countries <- countries[countries != c("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA")]
  
  # Extract unique sectors from row_item and col_item
  sectors <- sort(unique(c(wiot_2011$row_item, wiot_2011$col_item)))

  if (length(sectors) > 35) {
    sectors <- sectors[1:35]
  }
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Create trade flows array from actual data
  trade_flows_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                           dimnames = list(countries, countries, sectors))
  
  for (i in 1:nrow(wiot_2011)) {
    row_country <- wiot_2011$row_country[i]
    col_country <- wiot_2011$col_country[i] 
    row_sector <- wiot_2011$row_item[i]
    col_sector <- wiot_2011$col_item[i]
    flow_value <- wiot_2011$value[i]
    
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
    sectors = sectors,
    years = unique(wiot_raw$year)
  ))
}

#' Clean socio-economic accounts data for CDK (2012) analysis
#' @param socioeco_file_path Path to the Excel file with socio-economic data
#' @param countries Vector of countries from WIOD
#' @param sectors Vector of sectors from WIOD
#' @param target_year Target year for analysis (default: 2011)
#' @return List with cleaned labor data and countries with complete data
clean_socioeconomic_data_cdk <- function(socioeco_file_path, countries, sectors, target_year = 2011) {
  
  socioeco_data <- readxl::read_excel(socioeco_file_path, sheet = 2)
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  year_col <- paste0("_", target_year)
  
  # Filter countries with complete 2011 data
  countries_with_2011_data <- socioeco_data %>%
    filter(!is.na(.data[[year_col]])) %>%
    pull(Country) %>%
    unique()
  
  common_countries <- intersect(countries, countries_with_2011_data)
  
  # Extract key variables for 2011
  # EMP: Number of persons engaged (thousands)
  # LAB: Labour compensation (millions of national currency)
  # COMP: Compensation of employees (millions of national currency)
  
  key_variables <- c("EMP", "EMPE", "LAB", "COMP", "GO", "VA")
  
  employment_data <- socioeco_data %>%
    filter(Country %in% common_countries, 
           Variable %in% key_variables,
           !is.na(.data[[year_col]])) %>%
    select(Country, Variable, Description, all_of(year_col)) %>%
    rename(value_2011 = all_of(year_col))
  
  # Create labor matrix (countries x sectors)
  # For now, assume equal allocation across sectors (can be refined)
  n_common_countries <- length(common_countries)
  labor_matrix <- matrix(1/n_sectors, n_common_countries, n_sectors,
                        dimnames = list(common_countries, sectors))
  
  # Create expenditure shares (β_s) - Cobb-Douglas preferences
  beta_vec <- rep(1/n_sectors, n_sectors)
  names(beta_vec) <- sectors
  
  gdp_data <- socioeco_data %>%
    filter(Country %in% common_countries,
           Variable %in% c("GO", "VA"),  # Gross Output, Value Added
           !is.na(.data[[year_col]])) %>%
    select(Country, Variable, all_of(year_col)) %>%
    rename(value_2011 = all_of(year_col))
  
  return(list(
    labor_matrix = labor_matrix,
    beta_vec = beta_vec,
    countries = common_countries,  # Updated to filtered countries
    sectors = sectors,
    employment_data = employment_data,
    gdp_data = gdp_data,
    countries_with_data = common_countries,
    target_year = target_year
  ))
}

#' Create bilateral trade flow matrices by sector
#' @param cleaned_wiod Cleaned WIOD data
#' @param year Specific year to extract (default: 2011)
#' @return Array of trade flow matrices (countries x countries x sectors)
create_trade_matrices <- function(cleaned_wiod, year = 2011) {
  
  # Extract bilateral trade flows for specified year
  # Create matrices where element (i,j,s) represents trade from country i to j in sector s
  
  # Placeholder implementation
  n_countries <- length(cleaned_wiod$countries)
  n_sectors <- length(cleaned_wiod$sectors)
  
  trade_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                      dimnames = list(cleaned_wiod$countries, 
                                    cleaned_wiod$countries,
                                    cleaned_wiod$sectors))
  
  return(trade_array)
}

#' Calculate trade shares from trade flows
#' @param trade_flows Trade flow matrices
#' @param total_absorption Total absorption by destination country and sector
#' @return Trade share matrices
calculate_observed_trade_shares <- function(trade_flows, total_absorption) {
  
  # Calculate pi_ij^s = X_ij^s / X_j^s where X_j^s is total absorption in country j, sector s
  
  n_countries <- dim(trade_flows)[1]
  n_sectors <- dim(trade_flows)[3]
  
  trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors))
  
  for (s in 1:n_sectors) {
    for (j in 1:n_countries) {
      if (total_absorption[j, s] > 0) {
        trade_shares[, j, s] <- trade_flows[, j, s] / total_absorption[j, s]
      }
    }
  }
  
  return(trade_shares)
}

#' Aggregate data for single-sector analysis (if needed)
#' @param trade_flows Multi-sector trade flows
#' @param labor_data Labor input data
#' @param aggregation_weights Weights for aggregation
#' @return Aggregated single-sector data
aggregate_to_single_sector <- function(trade_flows, labor_data, aggregation_weights = NULL) {
  
  # If we want to start with single-sector analysis first
  n_countries <- dim(trade_flows)[1]
  
  if (is.null(aggregation_weights)) {
    # Simple sum across sectors
    aggregated_flows <- apply(trade_flows, c(1, 2), sum)
  } else {
    # Weighted aggregation
    aggregated_flows <- matrix(0, n_countries, n_countries)
    for (s in 1:dim(trade_flows)[3]) {
      aggregated_flows <- aggregated_flows + trade_flows[, , s] * aggregation_weights[s]
    }
  }
  
  # Aggregate labor data
  aggregated_labor <- rowSums(labor_data, na.rm = TRUE)
  
  return(list(
    trade_flows = aggregated_flows,
    labor_inputs = aggregated_labor
  ))
}

#' Validate data consistency
#' @param trade_data Cleaned trade data
#' @param labor_data Cleaned labor data
#' @return Logical indicating if data passes validation checks
validate_data_consistency <- function(trade_data, labor_data) {
  
  # Check for missing values, negative values, etc.
  # Ensure countries and sectors match across datasets
  # Check if trade balances are reasonable
  
  checks_passed <- TRUE
  
  # Check for negative values
  if (any(trade_data$trade_flows < 0, na.rm = TRUE)) {
    warning("Negative trade flows detected")
    checks_passed <- FALSE
  }
  
  if (any(labor_data$labor_inputs < 0, na.rm = TRUE)) {
    warning("Negative labor inputs detected")
    checks_passed <- FALSE
  }
  
  # Check for missing values
  if (any(is.na(trade_data$trade_flows))) {
    warning("Missing values in trade flows")
    checks_passed <- FALSE
  }
  
  return(checks_passed)
}

###############################################################################
# Main data cleaning workflow
###############################################################################

#' Master function to clean all data for CDK (2012) analysis
#' @param wiot_file_path Path to the WIOD data file
#' @param socioeco_file_path Path to socio-economic Excel file
#' @param year Analysis year
#' @return List with all cleaned and processed data for CDK methodology
clean_all_data_cdk <- function(wiot_file_path, socioeco_file_path, year = 2011) {
  
  wiod_cleaned_initial <- clean_wiod_data_cdk(wiot_file_path)
  
  initial_countries <- wiod_cleaned_initial$countries
  sectors <- wiod_cleaned_initial$sectors
  
  socio_cleaned <- clean_socioeconomic_data_cdk(socioeco_file_path, initial_countries, sectors, year)
  
  final_countries <- socio_cleaned$countries_with_data
  
  # Re-read and filter WIOD data for final countries
  wiot_raw <- readr::read_csv(wiot_file_path)
  wiot_filtered <- wiot_raw[wiot_raw$row_country %in% final_countries & 
                           wiot_raw$col_country %in% final_countries, ]
  
  # Temporarily save filtered data and clean again
  temp_file <- tempfile(fileext = ".csv")
  readr::write_csv(wiot_filtered, temp_file)
  
  wiod_cleaned_final <- clean_wiod_data_cdk(temp_file)
  
  # Clean up temp file
  unlink(temp_file)
  
  trade_flows_array <- wiod_cleaned_final$trade_flows_array
  
  # Calculate absorption (total imports by destination-sector)
  n_countries <- length(final_countries)
  n_sectors <- length(sectors)
  
  absorption_matrix <- matrix(0, n_countries, n_sectors,
                             dimnames = list(final_countries, sectors))
  
  for (s in 1:n_sectors) {
    for (j in 1:n_countries) {
      absorption_matrix[j, s] <- sum(trade_flows_array[, j, s])
    }
  }
  
  # Calculate trade shares
  observed_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors),
                                dimnames = list(final_countries, final_countries, sectors))
  
  for (s in 1:n_sectors) {
    for (j in 1:n_countries) {
      if (absorption_matrix[j, s] > 0) {
        observed_trade_shares[, j, s] <- trade_flows_array[, j, s] / absorption_matrix[j, s]
      }
    }
  }
  
  final_data <- list(
    # WIOD data (filtered)
    trade_flows_array = trade_flows_array,
    observed_trade_shares = observed_trade_shares,
    absorption_matrix = absorption_matrix,
    
    # Country and sector info (filtered)
    countries = final_countries,  # Only countries with complete 2011 data
    sectors = sectors,
    
    # Socio-economic data
    labor_matrix = socio_cleaned$labor_matrix,
    beta_vec = socio_cleaned$beta_vec,
    employment_data = socio_cleaned$employment_data,
    gdp_data = socio_cleaned$gdp_data,
    
    # Metadata
    year = year,
    n_countries = n_countries,
    n_sectors = n_sectors,
    initial_countries = initial_countries,
    countries_with_complete_data = final_countries
  )
  
  return(final_data)
}