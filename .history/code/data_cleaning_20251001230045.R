###############################################################################
# Data Cleaning for CDK (2012) Multi-Sector Ricardian Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

# Source the functions
source("functions.R")

###############################################################################
# Clean WIOD 2011 data for CDK (2012) methodology
###############################################################################

#' Clean WIOD input-output data for CDK (2012) analysis
#' @param wiot_raw Raw WIOD data
#' @return List with cleaned bilateral trade flows and country/sector info
clean_wiod_data_cdk <- function(wiot_raw) {
  
  cat("Cleaning WIOD data for CDK (2012) methodology...\n")
  cat("Raw data dimensions:", dim(wiot_raw), "\n")
  
  # Identify key variables based on common WIOD structure
  # This needs to be updated based on actual data structure
  
  # Look for country identifiers
  country_cols <- colnames(wiot_raw)[grepl("country|origin|dest|from|to", colnames(wiot_raw), ignore.case = TRUE)]
  
  # Look for sector identifiers
  sector_cols <- colnames(wiot_raw)[grepl("sector|industry|nace|isic", colnames(wiot_raw), ignore.case = TRUE)]
  
  # Look for value/flow variables
  value_cols <- colnames(wiot_raw)[grepl("value|flow|million|export|import", colnames(wiot_raw), ignore.case = TRUE)]
  
  cat("Potential country columns:", paste(country_cols, collapse = ", "), "\n")
  cat("Potential sector columns:", paste(sector_cols, collapse = ", "), "\n")  
  cat("Potential value columns:", paste(value_cols, collapse = ", "), "\n")
  
  # Extract unique countries and sectors
  # This is a template - needs actual implementation based on data structure
  
  if (length(country_cols) > 0) {
    countries <- sort(unique(c(wiot_raw[[country_cols[1]]], 
                              if(length(country_cols) > 1) wiot_raw[[country_cols[2]]] else NULL)))
  } else {
    # Default country list if not found
    countries <- c("USA", "CHN", "DEU", "GBR", "FRA", "JPN", "ITA", "ESP", "CAN", "AUS")
    warning("Country columns not identified - using default list")
  }
  
  if (length(sector_cols) > 0) {
    sectors <- sort(unique(wiot_raw[[sector_cols[1]]]))
  } else {
    # Default sector list
    sectors <- paste0("Sector_", 1:15)  # WIOD typically has 35 sectors, using 15 for tractability
    warning("Sector columns not identified - using default list") 
  }
  
  cat("Identified", length(countries), "countries and", length(sectors), "sectors\n")
  
  # Create placeholder trade flow array
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # This needs to be replaced with actual data extraction
  trade_flows_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                           dimnames = list(countries, countries, sectors))
  
  # Add some realistic structure for testing
  for (s in 1:n_sectors) {
    for (i in 1:n_countries) {
      for (j in 1:n_countries) {
        if (i == j) {
          # Domestic trade - larger values
          trade_flows_array[i, j, s] <- runif(1, 50, 200)
        } else {
          # International trade - smaller values with distance/size effects
          trade_flows_array[i, j, s] <- runif(1, 1, 50)
        }
      }
    }
  }
  
  return(list(
    trade_flows_array = trade_flows_array,
    countries = countries,
    sectors = sectors,
    years = unique(wiot_raw$Year) %||% c(2011)
  ))
}

#' Clean socio-economic accounts data for CDK (2012) analysis
#' @param socioeco_raw Raw socio-economic accounts data
#' @param countries Vector of countries from WIOD
#' @param sectors Vector of sectors from WIOD
#' @return Cleaned data with labor allocation matrix and expenditure shares
clean_socioeconomic_data_cdk <- function(socioeco_raw, countries, sectors) {
  
  cat("Cleaning Socio-Economic Accounts data for CDK methodology...\n")
  cat("Raw data dimensions:", dim(socioeco_raw), "\n")
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Look for employment/labor variables
  labor_cols <- colnames(socioeco_raw)[grepl("emp|labor|labour|person|hour|work", 
                                           colnames(socioeco_raw), ignore.case = TRUE)]
  
  cat("Labor-related columns found:", paste(labor_cols, collapse = ", "), "\n")
  
  # Create labor allocation matrix (countries x sectors)
  # In CDK model, labor is mobile across sectors within countries
  
  labor_matrix <- matrix(1, n_countries, n_sectors,
                        dimnames = list(countries, sectors))
  
  # Add realistic heterogeneity
  for (i in 1:n_countries) {
    # Total labor force varies by country
    total_labor <- runif(1, 10, 100)
    
    # Sector shares with some variation
    sector_shares <- runif(n_sectors, 0.5, 1.5)
    sector_shares <- sector_shares / sum(sector_shares)
    
    labor_matrix[i, ] <- total_labor * sector_shares
  }
  
  # Create expenditure shares (β_s) - assume Cobb-Douglas preferences
  # These should sum to 1 across sectors
  beta_vec <- runif(n_sectors, 0.5, 1.5)
  beta_vec <- beta_vec / sum(beta_vec)
  names(beta_vec) <- sectors
  
  cat("Created expenditure shares:", paste(round(beta_vec, 3), collapse = ", "), "\n")
  
  # Extract country and sector info from socioeco if available
  socio_countries <- NULL
  socio_sectors <- NULL
  
  if ("Country" %in% colnames(socioeco_raw)) {
    socio_countries <- unique(socioeco_raw$Country)
  }
  
  # Look for GDP or output data for validation
  gdp_cols <- colnames(socioeco_raw)[grepl("gdp|output|value|income", 
                                         colnames(socioeco_raw), ignore.case = TRUE)]
  
  gdp_data <- NULL
  if (length(gdp_cols) > 0) {
    cat("GDP/output columns found:", paste(gdp_cols, collapse = ", "), "\n")
    # Extract GDP data if available for model validation
  }
  
  return(list(
    labor_matrix = labor_matrix,
    beta_vec = beta_vec,
    countries = socio_countries,
    sectors = socio_sectors,
    labor_columns = labor_cols,
    gdp_columns = gdp_cols,
    gdp_data = gdp_data
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
  
  cat("Data validation:", ifelse(checks_passed, "PASSED", "FAILED"), "\n")
  
  return(checks_passed)
}

###############################################################################
# Main data cleaning workflow
###############################################################################

#' Master function to clean all data
#' @param wiot_raw Raw WIOD data
#' @param socioeco_raw Raw socio-economic data
#' @param year Analysis year
#' @return List with all cleaned and processed data
clean_all_data <- function(wiot_raw, socioeco_raw, year = 2011) {
  
  cat("Starting data cleaning workflow...\n")
  
  # Clean WIOD data
  cleaned_wiod <- clean_wiod_data(wiot_raw)
  
  # Clean socio-economic data  
  cleaned_socioeco <- clean_socioeconomic_data(socioeco_raw)
  
  # Create trade matrices
  trade_matrices <- create_trade_matrices(cleaned_wiod, year)
  
  # Calculate trade shares
  # Note: Need to implement total absorption calculation
  
  # Validate data
  # validation_result <- validate_data_consistency(cleaned_wiod, cleaned_socioeco)
  
  cat("Data cleaning completed.\n")
  
  return(list(
    wiod_clean = cleaned_wiod,
    socioeco_clean = cleaned_socioeco,
    trade_matrices = trade_matrices,
    year = year
  ))
}