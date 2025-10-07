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
clean_wiod_data_cdk <- function(wiot_file_path, target_year = 2011) {
  
  if(!file.exists(paste0(wiot_file_path, "_", target_year, ".RData"))) {
    # Load WIOD data from file
    wiot_raw <- read_dta(wiot_file_path)
    
    wiot_2011 <- wiot_raw[wiot_raw$year == 2011, ]
    
    # Export 2011 data for RData
    save(wiot_2011, file = paste0(wiot_file_path, "_", target_year, ".RData"))
  } else {
    # Load WIOD data from pre-saved 2011 RData
    load(paste0(wiot_file_path, "_", target_year, ".RData"))
  }
  
  # Extract unique countries from row_country and col_country
  countries <- sort(unique(c(wiot_2011$row_country, wiot_2011$col_country)))
  countries <- countries[!countries %in% c("CIF", "GO", "ITM", "PUA", "PUF", "TOT", "TXP", "VA", "RoW")]
  
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
    sectors = sectors
  ))
}

#' Clean socio-economic accounts data for CDK (2012) analysis
#' @param socioeco_file_path Path to the Excel file with socio-economic data
#' @param countries Vector of countries from WIOD
#' @param sectors Vector of sectors from WIOD
#' @param target_year Target year for analysis (default: 2011)
#' @return List with cleaned labor data and countries with complete data
clean_socioeconomic_data_cdk <- function(socioeco_file_path, target_year = 2011) {
    
    socioeco_data <- readxl::read_excel(socioeco_file_path, sheet = 2)
    socioeco_data <- as.data.frame(socioeco_data)
    socioeco_data <- socioeco_data %>%
        filter(Code != "TOT") # Remove total sector

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
    
    # Create labor matrix (countries x sectors)
    # For now, assume equal allocation across sectors (can be refined)
    labor_matrix <- matrix(1/n_sectors, n_countries, n_sectors,
                            dimnames = list(countries, sectors))

    # Create expenditure shares (β_s) - Cobb-Douglas preferences
    beta_vec <- rep(1/n_sectors, n_sectors)
    names(beta_vec) <- sectors

    gdp_data <- socioeco_data %>%
        filter(Variable %in% c("GO", "VA"),  # Gross Output, Value Added
            !is.na(.data[[year_col]])) %>%
        select(Country, Variable, all_of(year_col)) %>%
        rename(value_2011 = all_of(year_col))
    
    return(list(
        labor_matrix = labor_matrix,
        beta_vec = beta_vec,
        countries = countries,
        sectors = sectors,
        employment_data = employment_data,
        gdp_data = gdp_data
    ))
}

###############################################################################
# Main data cleaning workflow
###############################################################################

#' Master function to clean all data for CDK (2012) analysis
#' @param wiot_file_path Path to the WIOD data file
#' @param socioeco_file_path Path to socio-economic Excel file
#' @param year Analysis year
#' @return List with all cleaned and processed data for CDK methodology
clean_all_data_cdk <- function(wiot_file_path, socioeco_file_path, target_year = 2011) {

    wiod_cleaned <- clean_wiod_data_cdk(wiot_file_path, target_year)
    socio_cleaned <- clean_socioeconomic_data_cdk(socioeco_file_path, target_year)

    countries <- wiod_cleaned$countries
    sectors <- cleaned$sectors

    trade_flows_array <- wiod_cleaned$trade_flows_array
    
    # Calculate absorption (total imports by destination-sector)
    n_countries <- length(countries)
    n_sectors <- length(sectors)

    absorption_matrix <- matrix(0, n_countries, n_sectors,
                                dimnames = list(countries, sectors))

    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
        absorption_matrix[j, s] <- sum(trade_flows_array[, j, s])
        }
    }
    
    # Calculate trade shares
    observed_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors),
                                    dimnames = list(countries, countries, sectors))

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
        countries = countries,  # Only countries with complete 2011 data
        sectors = sectors,
        
        # Socio-economic data
        labor_matrix = socio_cleaned$labor_matrix,
        beta_vec = socio_cleaned$beta_vec,
        employment_data = socio_cleaned$employment_data,
        gdp_data = socio_cleaned$gdp_data
    )
    
    return(final_data)
}