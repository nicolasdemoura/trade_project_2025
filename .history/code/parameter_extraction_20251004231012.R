###############################################################################
# Parameter Extraction and Processing Functions for CDK (2012) Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

# Source original estimation functions for technology parameter regression
source("estimation.R")

#' Process WIOD data into trade flows and expenditure arrays
#' @param raw_database Raw c    # Step 4: Extracting model parameters...
    parameters <- list(
        alpha = get_alpha_parameters(processed_wiod, countries, sectors),
        beta = get_beta_parameters(processed_socioeco, countries, sectors),
        gamma = get_gamma_parameters(processed_wiod, countries, sectors),
        technology = get_technology_parameters(processed_wiod, processed_socioeco, countries, sectors, theta),
        theta = get_trade_elasticity(theta),
        tariffs = get_tariff_data(processed_tariffs, countries, sectors)
    )tabase from data_processing.R
#' @return List with processed WIOD arrays and matrices
process_wiod_data <- function(raw_database) {
    
    wiod_data <- raw_database$wiod_data
    countries <- raw_database$countries
    sectors <- raw_database$sectors
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Create trade flows array
    trade_flows_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                            dimnames = list(countries, countries, sectors))
    
    # Create gamma matrix for intermediate input shares
    gamma_matrix <- array(0, dim = c(n_countries, n_sectors, n_sectors),
                          dimnames = list(countries, sectors, sectors))

    # Process WIOD data into arrays
    for (i in 1:nrow(wiod_data)) {
        row_country <- wiod_data$row_country[i]
        col_country <- wiod_data$col_country[i] 
        row_sector <- wiod_data$row_item[i]
        col_sector <- wiod_data$col_item[i]
        flow_value <- wiod_data$value[i]
        
        if (row_country %in% countries && 
            col_country %in% countries && 
            row_sector %in% sectors &&
            col_sector %in% sectors &&
            !is.na(flow_value)) {
            
            # Get indices
            i_idx <- which(countries == row_country)
            j_idx <- which(countries == col_country)
            k_idx <- which(sectors == row_sector)
            l_idx <- which(sectors == col_sector)
            
            # Store trade flow
            trade_flows_array[i_idx, j_idx, k_idx] <- trade_flows_array[i_idx, j_idx, k_idx] + flow_value
            
            # Store intermediate input flow for gamma calculation
            gamma_matrix[j_idx, l_idx, k_idx] <- gamma_matrix[j_idx, l_idx, k_idx] + flow_value
        }
    }

    # Normalize gamma matrix (intermediate input shares)
    for (j in 1:n_countries) {
        for (l in 1:n_sectors) {
            total_intermediate <- sum(gamma_matrix[j, l, ])
            if (total_intermediate > 0) {
                gamma_matrix[j, l, ] <- gamma_matrix[j, l, ] / total_intermediate
            } else {
                gamma_matrix[j, l, ] <- 1 / n_sectors  # Equal shares if no data
            }
        }
    }
    
    # Calculate expenditure matrix (total imports by destination-sector)
    expenditure_matrix <- matrix(0, n_countries, n_sectors,
                                dimnames = list(countries, sectors))
    
    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            expenditure_matrix[j, s] <- sum(trade_flows_array[, j, s])
        }
    }
    
    # Calculate observed trade shares
    observed_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors),
                                    dimnames = list(countries, countries, sectors))
    
    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            total_expenditure <- expenditure_matrix[j, s]
            if (total_expenditure > 0) {
                observed_trade_shares[, j, s] <- trade_flows_array[, j, s] / total_expenditure
            }
        }
    }
    
    cat("✓ WIOD data processed into arrays\n")
    cat("  - Trade flows array:", paste(dim(trade_flows_array), collapse = " x "), "\n")
    cat("  - Gamma matrix:", paste(dim(gamma_matrix), collapse = " x "), "\n")
    
    return(list(
        trade_flows_array = trade_flows_array,
        expenditure_matrix = expenditure_matrix,
        observed_trade_shares = observed_trade_shares,
        gamma_matrix = gamma_matrix
    ))
}

#' Process socioeconomic data into model parameters
#' @param raw_database Raw cleaned database from data_processing.R  
#' @return List with processed socioeconomic matrices
process_socioeco_data <- function(raw_database) {
    
    socioeco_data <- raw_database$socioeco_data
    countries <- raw_database$countries
    sectors <- raw_database$sectors
    target_year <- raw_database$target_year
    year_col <- paste0("_", target_year)
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Process socioeconomic data
    data <- socioeco_data %>%
        filter(Code %in% sectors, !is.na(.data[[year_col]])) %>%
        select(Country, Variable, Code, all_of(year_col)) %>%
        rename(value = all_of(year_col))
    data$value <- as.numeric(data$value)
    
    # Create labor matrix from employment data
    emp_data <- data %>%
        filter(Variable == "EMPE") %>%
        select(Country, Code, value)
    
    labor_matrix <- emp_data %>%
        filter(Country %in% countries, Code %in% sectors) %>%
        group_by(Country, Code) %>%
        summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
        pivot_wider(names_from = Code, values_from = value, values_fill = 0) %>%
        column_to_rownames(var = "Country") %>%
        as.matrix()
    
    # Create beta matrix (labor share of gross output)
    lab_data <- data %>%
        filter(Variable == "LAB") %>%
        select(Country, Code, value)
    
    go_data <- data %>%
        filter(Variable == "GO") %>%
        select(Country, Code, value) %>%
        rename(go = value)
    
    beta_data <- lab_data %>%
        left_join(go_data, by = c("Country", "Code")) %>%
        mutate(beta = value / go)
    beta_data$beta[is.na(beta_data$beta) | is.infinite(beta_data$beta)] <- 0
    
    beta_matrix <- beta_data %>%
        group_by(Country, Code) %>%
        summarise(beta = mean(beta, na.rm = TRUE), .groups = 'drop') %>%
        pivot_wider(names_from = Code, values_from = beta) %>%
        column_to_rownames(var = "Country") %>%
        as.matrix()
    
    cat("✓ Socioeconomic data processed\n")
    cat("  - Labor matrix:", paste(dim(labor_matrix), collapse = " x "), "\n")
    cat("  - Beta matrix:", paste(dim(beta_matrix), collapse = " x "), "\n")
    
    return(list(
        labor_matrix = labor_matrix,
        beta_matrix = beta_matrix
    ))
}

#' Process tariff data into tariff array
#' @param raw_database Raw cleaned database from data_processing.R
#' @return 3D array of tariff rates (origin x destination x sector)
process_tariff_data <- function(raw_database) {
    
    tariff_data <- raw_database$tariff_data
    countries <- raw_database$countries
    sectors <- raw_database$sectors
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    if (is.null(tariff_data)) {
        # Create zero tariff array if no data
        tariff_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                             dimnames = list(countries, countries, sectors))
        cat("✓ No tariff data - using zero baseline\n")
        return(tariff_array)
    }
    
    # Process tariff data
    processed_tariff_data <- tariff_data %>%
        mutate(
            across(c(importer, exporter), ~ case_when(
                .x == "United States of America" ~ "USA",
                .x == "China" ~ "CHN", 
                .x == "Japan" ~ "JPN",
                .x == "Canada" ~ "CAN",
                .x == "Mexico" ~ "MEX",
                .x == "Brazil" ~ "BRA",
                .x == "India" ~ "IND",
                .x == "Indonesia" ~ "IDN",
                .x == "Australia" ~ "AUS",
                .x == "South Korea" ~ "KOR",
                .x == "Turkey" ~ "TUR",
                .x == "Russia" ~ "RUS",
                TRUE ~ .x
            ))
        )
    
    # Create tariff array
    tariff_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                         dimnames = list(countries, countries, sectors))
    
    # Fill in tariff rates
    for (i in 1:nrow(processed_tariff_data)) {
        imp <- processed_tariff_data$importer[i]
        exp <- processed_tariff_data$exporter[i]
        sec <- processed_tariff_data$sector[i]
        rate <- processed_tariff_data$tariff_rate[i] / 100  # Convert percentage to decimal
        
        # Find indices
        imp_idx <- which(countries == imp)
        exp_idx <- which(countries == exp)
        sec_idx <- which(sectors == sec)
        
        # Assign if all indices found
        if (length(imp_idx) == 1 && length(exp_idx) == 1 && length(sec_idx) == 1) {
            tariff_array[exp_idx, imp_idx, sec_idx] <- rate
        }
    }
    
    cat("✓ Tariff data processed\n")
    cat("  - Dimensions:", paste(dim(tariff_array), collapse = " x "), "\n")
    cat("  - Non-zero tariffs:", sum(tariff_array > 0), "\n")
    
    return(tariff_array)
}

#' Extract and process alpha parameters (expenditure shares) from processed WIOD data
#' @param processed_wiod Processed WIOD data from process_wiod_data()
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Matrix of expenditure shares (countries x sectors)
get_alpha_parameters <- function(processed_wiod, countries, sectors) {
    
    expenditure_matrix <- processed_wiod$expenditure_matrix
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Calculate expenditure shares: alpha_nk = X_nk / sum_k(X_nk)
    alpha_matrix <- matrix(0, n_countries, n_sectors)
    
    for (n in 1:n_countries) {
        total_expenditure <- sum(expenditure_matrix[n, ])
        if (total_expenditure > 0) {
            alpha_matrix[n, ] <- expenditure_matrix[n, ] / total_expenditure
        } else {
            # Equal shares if no expenditure data
            alpha_matrix[n, ] <- 1 / n_sectors
        }
    }
    
    # Ensure rows sum to 1
    for (n in 1:n_countries) {
        row_sum <- sum(alpha_matrix[n, ])
        if (row_sum > 0) {
            alpha_matrix[n, ] <- alpha_matrix[n, ] / row_sum
        }
    }
    
    rownames(alpha_matrix) <- countries
    colnames(alpha_matrix) <- sectors
    
    cat("✓ Alpha parameters extracted: expenditure shares\n")
    cat("  - Dimensions:", paste(dim(alpha_matrix), collapse = " x "), "\n")
    cat("  - Average expenditure share:", round(mean(alpha_matrix), 4), "\n")
    
    return(alpha_matrix)
}

#' Extract and process beta parameters (labor shares) from processed socioeconomic data
#' @param processed_socioeco Processed socioeconomic data from process_socioeco_data()
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Matrix of labor shares (countries x sectors)
get_beta_parameters <- function(processed_socioeco, countries, sectors) {
    
    beta_matrix <- processed_socioeco$beta_matrix
    
    # Validate beta parameters
    if (is.null(beta_matrix)) {
        stop("Beta matrix not found in processed socioeconomic data")
    }
    
    # Ensure proper dimensions and names
    rownames(beta_matrix) <- countries
    colnames(beta_matrix) <- sectors
    
    # Validate bounds (labor shares should be between 0 and 1)
    beta_matrix[beta_matrix < 0] <- 0.01
    beta_matrix[beta_matrix > 1] <- 0.99
    
    cat("✓ Beta parameters extracted: labor shares\n")
    cat("  - Dimensions:", paste(dim(beta_matrix), collapse = " x "), "\n")
    cat("  - Average labor share:", round(mean(beta_matrix, na.rm = TRUE), 4), "\n")
    cat("  - Range: [", round(min(beta_matrix, na.rm = TRUE), 4), ", ", 
        round(max(beta_matrix, na.rm = TRUE), 4), "]\n")
    
    return(beta_matrix)
}

#' Extract and process gamma parameters (intermediate input shares) from processed WIOD data
#' @param processed_wiod Processed WIOD data from process_wiod_data()
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Array of intermediate input shares (countries x sectors x sectors) 
get_gamma_parameters <- function(processed_wiod, countries, sectors) {
    
    gamma_matrix <- processed_wiod$gamma_matrix
    
    # Validate gamma parameters
    if (is.null(gamma_matrix)) {
        stop("Gamma matrix not found in processed WIOD data")
    }
    
    # Ensure proper dimensions and names
    dimnames(gamma_matrix) <- list(countries, sectors, sectors)
    
    # Additional validation and normalization (already done in processing, but ensure consistency)
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            # Ensure non-negative
            gamma_matrix[i, k, ][gamma_matrix[i, k, ] < 0] <- 0
            
            # Normalize to sum to 1 (should already be done, but ensure)
            row_sum <- sum(gamma_matrix[i, k, ])
            if (row_sum > 0) {
                gamma_matrix[i, k, ] <- gamma_matrix[i, k, ] / row_sum
            } else {
                # Equal shares if no data
                gamma_matrix[i, k, ] <- 1 / n_sectors
            }
        }
    }
    
    cat("✓ Gamma parameters extracted: intermediate input shares\n")
    cat("  - Dimensions:", paste(dim(gamma_matrix), collapse = " x "), "\n") 
    cat("  - Average intermediate share:", round(mean(gamma_matrix, na.rm = TRUE), 4), "\n")
    
    return(gamma_matrix)
}

#' Extract and calibrate technology parameters from processed WIOD data and socioeconomic data
#' @param processed_wiod Processed WIOD data from process_wiod_data()
#' @param processed_socioeco Processed socioeconomic data from process_socioeco_data()
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @param theta Trade elasticity parameter
#' @return Matrix of technology parameters (countries x sectors)
get_technology_parameters <- function(processed_wiod, processed_socioeco, countries, sectors, theta = 6.53) {
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Method 1: Use productivity measures from socioeconomic data if available
    # Calculate labor productivity as GO/EMPE (Gross Output per Employee)
    T_matrix <- matrix(1, n_countries, n_sectors)
    
    # Try to extract productivity from socioeconomic data
    socioeco_data <- processed_socioeco$socioeco_data
    if (!is.null(socioeco_data)) {
        target_year <- names(socioeco_data)[ncol(socioeco_data)]
        
        # Get GO and EMPE data
        go_data <- socioeco_data %>%
            filter(Variable == "GO") %>%
            select(Country, Code, all_of(target_year)) %>%
            rename(go = all_of(target_year))
        
        empe_data <- socioeco_data %>%
            filter(Variable == "EMPE") %>%
            select(Country, Code, all_of(target_year)) %>%
            rename(empe = all_of(target_year))
        
        # Calculate productivity
        productivity_data <- go_data %>%
            left_join(empe_data, by = c("Country", "Code")) %>%
            mutate(productivity = ifelse(empe > 0, go / empe, 1))
        
        # Fill technology matrix with productivity measures
        for (i in 1:nrow(productivity_data)) {
            country <- productivity_data$Country[i]
            sector <- productivity_data$Code[i]
            prod <- productivity_data$productivity[i]
            
            country_idx <- which(countries == country)
            sector_idx <- which(sectors == sector)
            
            if (length(country_idx) == 1 && length(sector_idx) == 1 && !is.na(prod) && prod > 0) {
                T_matrix[country_idx, sector_idx] <- prod
            }
        }
        
        cat("✓ Technology parameters calibrated from productivity data\n")
    } else {
        # Method 2: Fallback - use simple calibration based on trade patterns
        observed_trade_shares <- processed_wiod$observed_trade_shares
        
        # Extract domestic trade shares as proxy for competitiveness/technology
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                domestic_share <- observed_trade_shares[i, i, k]
                if (!is.na(domestic_share) && domestic_share > 0) {
                    # Higher domestic share suggests higher relative productivity
                    # Convert to technology parameter (this is a simplification)
                    T_matrix[i, k] <- (domestic_share / (1 - domestic_share + 0.01)) ^ (1/theta)
                }
            }
        }
        
        cat("✓ Technology parameters calibrated from trade patterns\n")
    }
    
    # Normalize technology parameters (set geometric mean to 1 for identification)
    geometric_mean <- exp(mean(log(T_matrix[T_matrix > 0])))
    T_matrix <- T_matrix / geometric_mean
    
    # Ensure all values are positive
    T_matrix[T_matrix <= 0] <- 0.1
    
    # Ensure proper dimensions and names
    rownames(T_matrix) <- countries
    colnames(T_matrix) <- sectors
    
    cat("  - Dimensions:", paste(dim(T_matrix), collapse = " x "), "\n")
    cat("  - Average productivity (normalized):", round(mean(T_matrix), 4), "\n")
    cat("  - Range: [", round(min(T_matrix), 4), ", ", round(max(T_matrix), 4), "]\n")
    
    return(T_matrix)
}

#' Get trade elasticity parameter
#' @param value Optional custom value (default: CDK 2012 estimate)
#' @return Scalar trade elasticity
get_trade_elasticity <- function(value = 6.53) {
    
    cat("✓ Trade elasticity parameter set\n")
    cat("  - Value:", value, "(CDK 2012 benchmark)\n")
    
    return(value)
}

#' Extract tariff data from processed tariff array
#' @param processed_tariffs Processed tariff array from process_tariff_data()
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Array of tariff rates (origin x destination x sector)
get_tariff_data <- function(processed_tariffs, countries, sectors) {
    
    if (is.null(processed_tariffs)) {
        # Create zero tariff matrix if no data available
        n_countries <- length(countries)
        n_sectors <- length(sectors)
        tariffs <- array(0, dim = c(n_countries, n_countries, n_sectors))
        dimnames(tariffs) <- list(countries, countries, sectors)
        
        cat("✓ Tariff data: using zero baseline (no tariff data available)\n")
    } else {
        tariffs <- processed_tariffs
        
        # Ensure proper dimensions and names
        dimnames(tariffs) <- list(countries, countries, sectors)
        
        cat("✓ Tariff data extracted\n")
        cat("  - Dimensions:", paste(dim(tariffs), collapse = " x "), "\n")
        cat("  - Non-zero tariffs:", sum(tariffs > 0), "\n")
        if (sum(tariffs > 0) > 0) {
            cat("  - Average tariff rate:", round(mean(tariffs[tariffs > 0]), 4), "\n")
        }
    }
    
    return(tariffs)
}

#' Process all raw data into model-ready arrays and extract all parameters
#' @param raw_database Raw cleaned database from data_processing.R
#' @param theta Trade elasticity (optional)
#' @return List containing processed data and all model parameters
process_and_extract_all_parameters <- function(raw_database, theta = 6.53) {
    
    cat("Processing raw data and extracting all model parameters...\n")
    
    countries <- raw_database$countries
    sectors <- raw_database$sectors
    
    # Step 1: Process raw data into model arrays
    cat("Step 1: Processing WIOD data...\n")
    processed_wiod <- process_wiod_data(raw_database)
    
    cat("Step 2: Processing socioeconomic data...\n")
    processed_socioeco <- process_socioeco_data(raw_database)
    
    cat("Step 3: Processing tariff data...\n")
    processed_tariffs <- process_tariff_data(raw_database)
    
    # Step 2: Extract parameters from processed data
    cat("Step 4: Extracting model parameters...\n")
    parameters <- list(
        alpha = get_alpha_parameters(processed_wiod, countries, sectors),
        beta = get_beta_parameters(processed_socioeco, countries, sectors),
        gamma = get_gamma_parameters(processed_wiod, countries, sectors),
        technology = get_technology_parameters(processed_wiod, countries, sectors, theta),
        theta = get_trade_elasticity(theta),
        tariffs = get_tariff_data(processed_tariffs, countries, sectors)
    )
    
    # Organize processed database (for calibration functions that need it)
    processed_database <- list(
        # Dimensions
        countries = countries,
        sectors = sectors,
        
        # Processed WIOD data
        trade_flows_array = processed_wiod$trade_flows_array,
        expenditure_matrix = processed_wiod$expenditure_matrix,
        observed_trade_shares = processed_wiod$observed_trade_shares,
        
        # Processed IO structure
        beta_matrix = processed_socioeco$beta_matrix,
        gamma_matrix = processed_wiod$gamma_matrix,
        
        # Labor market data
        labor_matrix = processed_socioeco$labor_matrix,
        
        # Policy data
        tariffs = processed_tariffs,
        
        # Reference data
        gdp_data = raw_database$gdp_data,
        
        # Metadata
        target_year = raw_database$target_year
    )
    
    cat("✓ All data processed and parameters extracted successfully\n")
    
    return(list(
        parameters = parameters,
        processed_database = processed_database
    ))
}