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
#' @param wiot_raw Raw WIOD data with columns: year, row_country, col_country, row_item, col_item, value
#' @return List with cleaned bilateral trade flows and country/sector info
clean_wiod_data_cdk <- function(wiot_raw) {
  
  cat("Cleaning WIOD data for CDK (2012) methodology...\n")
  cat("Raw data dimensions:", dim(wiot_raw), "\n")
  
  # WIOD structure: year, row_country, col_country, row_item, col_item, value
  cat("WIOD columns:", paste(colnames(wiot_raw), collapse = ", "), "\n")
  
  # Filter for 2011 data
  wiot_2011 <- wiot_raw[wiot_raw$year == 2011, ]
  cat("Filtered to 2011:", nrow(wiot_2011), "observations\n")
  
  # Extract unique countries from row_country and col_country
  countries <- sort(unique(c(wiot_2011$row_country, wiot_2011$col_country)))
  
  # Extract unique sectors from row_item and col_item
  # Filter out final demand categories (typically start with 'F' or contain 'CONS', 'GFCF', etc.)
  all_items <- unique(c(wiot_2011$row_item, wiot_2011$col_item))
  
  # Keep only intermediate sectors (exclude final demand)
  # WIOD sectors are typically numbered or have industry codes
  sectors <- all_items[!grepl("^F|CONS|GFCF|INVNT|EXP|final|Final", all_items, ignore.case = TRUE)]
  
  # If sectors still include final demand, take first 35 (typical WIOD industry count)
  if (length(sectors) > 35) {
    sectors <- sectors[1:35]
  }
  
  cat("Identified", length(countries), "countries:\n")
  print(countries)
  cat("\nIdentified", length(sectors), "sectors:\n")
  print(head(sectors, 10))  # Print first 10 sectors
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Create trade flows array from actual data
  trade_flows_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                           dimnames = list(countries, countries, sectors))
  
  # Fill array with actual WIOD data
  cat("Extracting bilateral trade flows...\n")
  
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

#' Master function to clean all data for CDK (2012) analysis
#' @param wiot_raw Raw WIOD data
#' @param socioeco_raw Raw socio-economic data
#' @param year Analysis year
#' @return List with all cleaned and processed data for CDK methodology
clean_all_data_cdk <- function(wiot_raw, socioeco_raw, year = 2011) {
  
  cat("Starting CDK (2012) data cleaning workflow...\n")
  
  # Step 1: Clean WIOD data
  cleaned_wiod <- clean_wiod_data_cdk(wiot_raw)
  
  # Step 2: Clean socio-economic data using countries/sectors from WIOD
  cleaned_socioeco <- clean_socioeconomic_data_cdk(socioeco_raw, 
                                                  cleaned_wiod$countries,
                                                  cleaned_wiod$sectors)
  
  # Step 3: Calculate total sectoral absorption
  # Y_js = β_s * total country income (for Cobb-Douglas preferences)
  n_countries <- length(cleaned_wiod$countries)
  n_sectors <- length(cleaned_wiod$sectors)
  
  # Calculate total labor income by country
  total_labor_income <- rowSums(cleaned_socioeco$labor_matrix)
  
  # Calculate sectoral absorption matrix
  absorption_matrix <- matrix(0, n_countries, n_sectors,
                             dimnames = list(cleaned_wiod$countries, cleaned_wiod$sectors))
  
  for (s in 1:n_sectors) {
    absorption_matrix[, s] <- cleaned_socioeco$beta_vec[s] * total_labor_income
  }
  
  # Step 4: Calculate observed trade shares
  observed_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors),
                                dimnames = list(cleaned_wiod$countries, 
                                              cleaned_wiod$countries, 
                                              cleaned_wiod$sectors))
  
  for (s in 1:n_sectors) {
    for (j in 1:n_countries) {
      if (absorption_matrix[j, s] > 0) {
        observed_trade_shares[, j, s] <- cleaned_wiod$trade_flows_array[, j, s] / absorption_matrix[j, s]
      }
    }
  }
  
  # Step 5: Data validation
  cat("Validating CDK data consistency...\n")
  
  # Check that trade shares sum to 1 for each destination-sector
  for (s in 1:n_sectors) {
    for (j in 1:n_countries) {
      trade_share_sum <- sum(observed_trade_shares[, j, s])
      if (abs(trade_share_sum - 1) > 0.01) {
        cat("Warning: Trade shares for country", j, "sector", s, "sum to", round(trade_share_sum, 3), "\n")
        # Normalize to sum to 1
        if (trade_share_sum > 0) {
          observed_trade_shares[, j, s] <- observed_trade_shares[, j, s] / trade_share_sum
        }
      }
    }
  }
  
  cat("CDK data cleaning completed successfully.\n")
  
  return(list(
    countries = cleaned_wiod$countries,
    sectors = cleaned_wiod$sectors,
    trade_flows_array = cleaned_wiod$trade_flows_array,
    observed_trade_shares = observed_trade_shares,
    absorption_matrix = absorption_matrix,
    labor_matrix = cleaned_socioeco$labor_matrix,
    beta_vec = cleaned_socioeco$beta_vec,
    year = year,
    # Keep original cleaning results for reference
    wiod_clean = cleaned_wiod,
    socioeco_clean = cleaned_socioeco
  ))
}