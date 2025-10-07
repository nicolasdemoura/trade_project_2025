###############################################################################
# Data Cleaning for CDK (2012) Multi-Sector Ricardian Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

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
  
  cat("Trade flows extraction completed.\n")
  cat("Total non-zero flows:", sum(trade_flows_array > 0), "\n")
  cat("Average flow value:", round(mean(trade_flows_array[trade_flows_array > 0]), 2), "\n")
  
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
  
  cat("Cleaning Socio-Economic Accounts data for CDK methodology...\n")
  
  # Read the DATA sheet (second sheet) - consistent with test_data_loading.R
  socioeco_data <- readxl::read_excel(socioeco_file_path, sheet = 2)
  
  cat("Socio-economic data dimensions:", dim(socioeco_data), "\n")
  cat("Columns:", paste(colnames(socioeco_data), collapse = ", "), "\n")
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Check for 2011 data availability
  year_col <- paste0("_", target_year)
  
  if (!year_col %in% colnames(socioeco_data)) {
    cat("Warning: Target year", target_year, "not found in socio-economic data\n")
    return(NULL)
  }
  
  # Filter countries with complete 2011 data
  countries_with_2011_data <- socioeco_data %>%
    filter(!is.na(.data[[year_col]])) %>%
    pull(Country) %>%
    unique()
  
  cat("Countries with", target_year, "data:", length(countries_with_2011_data), "\n")
  cat("Sample countries:", paste(head(countries_with_2011_data, 10), collapse = ", "), "\n")
  
  # Filter WIOD countries to match those with socio-economic data
  common_countries <- intersect(countries, countries_with_2011_data)
  cat("Countries in both WIOD and socio-economic data:", length(common_countries), "\n")
  
  if (length(common_countries) < 5) {
    cat("Warning: Very few countries with complete data. Analysis may be limited.\n")
  }
  
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
  
  cat("Extracted employment data for", nrow(employment_data), "country-variable pairs\n")
  
  # Create labor matrix (countries x sectors)
  # For now, assume equal allocation across sectors (can be refined)
  n_common_countries <- length(common_countries)
  labor_matrix <- matrix(1/n_sectors, n_common_countries, n_sectors,
                        dimnames = list(common_countries, sectors))
  
  # Create expenditure shares (β_s) - Cobb-Douglas preferences
  beta_vec <- rep(1/n_sectors, n_sectors)
  names(beta_vec) <- sectors
  
  cat("Created labor allocation for", n_common_countries, "countries and", n_sectors, "sectors\n")
  
  cat("Created expenditure shares:", paste(round(beta_vec, 3), collapse = ", "), "\n")
  
  # Extract GDP/output data for validation
  gdp_data <- socioeco_data %>%
    filter(Country %in% common_countries,
           Variable %in% c("GO", "VA"),  # Gross Output, Value Added
           !is.na(.data[[year_col]])) %>%
    select(Country, Variable, all_of(year_col)) %>%
    rename(value_2011 = all_of(year_col))
  
  cat("Extracted GDP data for", nrow(gdp_data), "country-variable pairs\n")
  
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
  
  cat("Data validation:", ifelse(checks_passed, "PASSED", "FAILED"), "\n")
  
  return(checks_passed)
}

###############################################################################
# Main data cleaning workflow
###############################################################################

#' Master function to clean all data for CDK (2012) analysis
#' @param wiot_raw Raw WIOD data
#' @param socioeco_file_path Path to socio-economic Excel file
#' @param year Analysis year
#' @return List with all cleaned and processed data for CDK methodology
clean_all_data_cdk <- function(wiot_raw, socioeco_file_path, year = 2011) {
  
  cat("Starting comprehensive CDK (2012) data cleaning...\n")
  
  # Step 1: Clean WIOD data (initial pass)
  cat("Step 1: Initial WIOD data cleaning...\n")
  wiod_cleaned_initial <- clean_wiod_data_cdk(wiot_raw)
  
  if (is.null(wiod_cleaned_initial)) {
    cat("Error: WIOD data cleaning failed\n")
    return(NULL)
  }
  
  initial_countries <- wiod_cleaned_initial$countries
  sectors <- wiod_cleaned_initial$sectors
  
  # Step 2: Clean socio-economic data and get countries with complete data
  cat("Step 2: Cleaning socio-economic data and identifying countries with", year, "data...\n")
  socio_cleaned <- clean_socioeconomic_data_cdk(socioeco_file_path, initial_countries, sectors, year)
  
  if (is.null(socio_cleaned)) {
    cat("Error: Socio-economic data cleaning failed\n")
    return(NULL)
  }
  
  # Get filtered countries (only those with complete 2011 data)
  final_countries <- socio_cleaned$countries_with_data
  
  # Step 3: Re-clean WIOD data with filtered countries
  cat("Step 3: Re-filtering WIOD data for countries with complete", year, "data...\n")
  
  # Filter WIOD data to only include countries with complete socio-economic data
  wiot_filtered <- wiot_raw[wiot_raw$row_country %in% final_countries & 
                           wiot_raw$col_country %in% final_countries, ]
  
  wiod_cleaned_final <- clean_wiod_data_cdk(wiot_filtered)
  
  if (is.null(wiod_cleaned_final)) {
    cat("Error: Final WIOD filtering failed\n")
    return(NULL)
  }
  
  # Step 4: Create trade shares for regression using filtered data
  cat("Step 4: Creating observed trade shares for", length(final_countries), "countries...\n")
  
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
  
  # Step 5: Compile final results
  cat("Step 5: Compiling final CDK dataset...\n")
  
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
  
  cat("CDK data cleaning completed successfully!\n")
  cat("Final dataset:", n_countries, "countries x", n_sectors, "sectors\n")
  cat("Countries included:", paste(final_countries, collapse = ", "), "\n")
  
  return(final_data)
}