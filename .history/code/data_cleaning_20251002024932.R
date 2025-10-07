###############################################################################
# Data Cleaning for CDK (2012) Multi-Sector Ricardian Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

###############################################################################
# Clean WIOD 2011 data for CDK (2012) methodology
###############################################################################

# Define EU countries (excluding UK)
EU_COUNTRIES <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", 
                  "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", 
                  "LTU", "LUX", "MLT", "NLD", "POL", "PRT", "ROU", "SVK", 
                  "SVN", "ESP", "SWE")

#' Aggregate EU countries into single entity
#' @param data_array Array with country dimension to aggregate
#' @param countries Vector of country codes
#' @param dimension Which dimension contains countries (1, 2, or 3)
#' @return Array with EU countries aggregated
aggregate_eu_countries <- function(data_array, countries, dimension = 1) {
  
  # Identify EU and non-EU countries
  eu_indices <- which(countries %in% EU_COUNTRIES)
  non_eu_indices <- which(!countries %in% EU_COUNTRIES)
  
  # New country list: EU + non-EU countries
  new_countries <- c("EU", countries[non_eu_indices])
  
  if (length(dim(data_array)) == 2) {
    # Matrix case (e.g., labor_matrix, gdp_data)
    if (dimension == 1) {
      # Aggregate rows (countries)
      eu_aggregated <- colSums(data_array[eu_indices, , drop = FALSE])
      new_array <- rbind(eu_aggregated, data_array[non_eu_indices, , drop = FALSE])
      rownames(new_array) <- new_countries
    } else {
      # Aggregate columns (countries)
      eu_aggregated <- rowSums(data_array[, eu_indices, drop = FALSE])
      new_array <- cbind(eu_aggregated, data_array[, non_eu_indices, drop = FALSE])
      colnames(new_array) <- new_countries
    }
  } else if (length(dim(data_array)) == 3) {
    # 3D array case (e.g., trade_flows_array)
    dims <- dim(data_array)
    
    if (dimension == 1) {
      # Aggregate first dimension (origin countries)
      eu_aggregated <- apply(data_array[eu_indices, , , drop = FALSE], c(2, 3), sum)
      new_array <- array(0, dim = c(length(new_countries), dims[2], dims[3]))
      new_array[1, , ] <- eu_aggregated
      new_array[2:length(new_countries), , ] <- data_array[non_eu_indices, , ]
      dimnames(new_array)[[1]] <- new_countries
      dimnames(new_array)[[2]] <- dimnames(data_array)[[2]]
      dimnames(new_array)[[3]] <- dimnames(data_array)[[3]]
    } else if (dimension == 2) {
      # Aggregate second dimension (destination countries)
      eu_aggregated <- apply(data_array[, eu_indices, , drop = FALSE], c(1, 3), sum)
      new_array <- array(0, dim = c(dims[1], length(new_countries), dims[3]))
      new_array[, 1, ] <- eu_aggregated
      new_array[, 2:length(new_countries), ] <- data_array[, non_eu_indices, ]
      dimnames(new_array)[[1]] <- dimnames(data_array)[[1]]
      dimnames(new_array)[[2]] <- new_countries
      dimnames(new_array)[[3]] <- dimnames(data_array)[[3]]
    }
  }
  
  return(list(aggregated_data = new_array, new_countries = new_countries))
}

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
        if (nrow(country_data) > 0) {
            country_emp <- as.numeric(country_data[1, ])
            country_emp[is.na(country_emp)] <- 0
            # Convert to shares (labor allocation across sectors)
            total_emp <- sum(country_emp)
            if (total_emp > 0) {
                labor_matrix[i, ] <- country_emp / total_emp
            } else {
                labor_matrix[i, ] <- 1/length(available_sectors)  # Equal allocation if no data
            }
        } else {
            labor_matrix[i, ] <- 1/length(available_sectors)
        }
    }
    
    # Create expenditure shares (β_s) from labor compensation data
    # Use LAB (Labour compensation) to calculate sectoral expenditure shares
    lab_data <- employment_data %>%
        filter(Variable == "LAB") %>%
        select(Country, Code, value_2011) %>%
        group_by(Code) %>%
        summarise(total_sector_compensation = sum(value_2011, na.rm = TRUE), .groups = 'drop')
    
    # Calculate beta_vec as share of total labor compensation by sector
    total_compensation <- sum(lab_data$total_sector_compensation, na.rm = TRUE)
    if (total_compensation > 0) {
        beta_vec <- lab_data$total_sector_compensation / total_compensation
        names(beta_vec) <- lab_data$Code
        # Ensure beta_vec matches available_sectors and sums to 1
        beta_vec <- beta_vec[available_sectors]
        beta_vec[is.na(beta_vec)] <- 0
        if (sum(beta_vec) > 0) {
            beta_vec <- beta_vec / sum(beta_vec)
        } else {
            beta_vec <- rep(1/length(available_sectors), length(available_sectors))
            names(beta_vec) <- available_sectors
        }
    } else {
        # Fallback to equal shares
        beta_vec <- rep(1/length(available_sectors), length(available_sectors))
        names(beta_vec) <- available_sectors
    }
    
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
        summarise(gdp = sum(value_2011, na.rm = TRUE), .groups = 'drop')
    
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
    sectors <- clean_sectors

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