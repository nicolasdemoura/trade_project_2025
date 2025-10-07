###############################################################################
# Parameter Extraction and Processing Functions for CDK (2012) Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

#' Process WIOD data into trade flows and expenditure arrays - building blocks for parameter computation
#' @param raw_database Raw database from data_processing.R
#' @return List with processed WIOD building blocks for parameter extraction
process_wiod_data <- function(raw_database) {
    
    wiod_data <- raw_database$wiod_data
    countries <- raw_database$countries
    sectors <- raw_database$sectors
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Create trade flows array
    trade_flows_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                            dimnames = list(countries, countries, sectors))
    
    # Create intermediate flows matrix for gamma calculation (destination x consuming_sector x input_sector)
    intermediate_flows <- array(0, dim = c(n_countries, n_sectors, n_sectors),
                               dimnames = list(countries, sectors, sectors))
    
    # Create final demand matrix for alpha calculation
    final_demand_matrix <- matrix(0, n_countries, n_sectors,
                                 dimnames = list(countries, sectors))
    
    # Create domestic production matrix for technology parameter calculation
    domestic_production <- matrix(0, n_countries, n_sectors,
                                 dimnames = list(countries, sectors))

    # Process WIOD data into building block arrays
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
            
            # Store intermediate input flows for gamma calculation
            intermediate_flows[j_idx, l_idx, k_idx] <- intermediate_flows[j_idx, l_idx, k_idx] + flow_value
            
            # Store domestic production for technology parameters
            if (i_idx == j_idx) {  # Domestic production
                domestic_production[i_idx, k_idx] <- domestic_production[i_idx, k_idx] + flow_value
            }
        }
    }

    # Calculate total expenditures by destination-sector (for alpha calculation)
    total_expenditures <- matrix(0, n_countries, n_sectors,
                                dimnames = list(countries, sectors))
    
    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            total_expenditures[j, s] <- sum(trade_flows_array[, j, s])
        }
    }
    
    # Calculate total intermediate inputs by destination-sector (for gamma calculation)
    total_intermediate_inputs <- matrix(0, n_countries, n_sectors,
                                       dimnames = list(countries, sectors))
    
    for (j in 1:n_countries) {
        for (l in 1:n_sectors) {
            total_intermediate_inputs[j, l] <- sum(intermediate_flows[j, l, ])
        }
    }
    
    # Calculate bilateral trade shares (building block for technology estimation)
    bilateral_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors),
                                   dimnames = list(countries, countries, sectors))
    
    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            if (total_expenditures[j, s] > 0) {
                bilateral_trade_shares[, j, s] <- trade_flows_array[, j, s] / total_expenditures[j, s]
            }
        }
    }
    
    cat("✓ WIOD data processed into building blocks\n")
    cat("  - Trade flows array:", paste(dim(trade_flows_array), collapse = " x "), "\n")
    cat("  - Intermediate flows:", paste(dim(intermediate_flows), collapse = " x "), "\n")
    cat("  - Domestic production:", paste(dim(domestic_production), collapse = " x "), "\n")
    
    return(list(
        # Core building blocks
        trade_flows_array = trade_flows_array,
        intermediate_flows = intermediate_flows,
        domestic_production = domestic_production,
        
        # Aggregated building blocks for parameter computation
        total_expenditures = total_expenditures,
        total_intermediate_inputs = total_intermediate_inputs,
        bilateral_trade_shares = bilateral_trade_shares
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
        filter(Variable == "EMP") %>%
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
                .x == "United States" ~ "USA",
                .x == "EuropeanUnion --- EuropeanUnion  " ~ "EU",
                .x == "China" ~ "CHN", 
                .x == "Japan" ~ "JPN",
                .x == "Canada" ~ "CAN",
                .x == "Mexico" ~ "MEX",
                .x == "Brazil" ~ "BRA",
                .x == "India" ~ "IND",
                .x == "United Kingdom" ~ "GBR",
                .x == "RestOfTheWorld --- RestOfTheWorld  " ~ "RoW",
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
    
    total_expenditures <- processed_wiod$total_expenditures
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Compute expenditure shares: alpha_nk = X_nk / sum_k(X_nk)
    alpha_matrix <- matrix(0, n_countries, n_sectors)
    
    for (n in 1:n_countries) {
        country_total_expenditure <- sum(total_expenditures[n, ])
        if (country_total_expenditure > 0) {
            alpha_matrix[n, ] <- total_expenditures[n, ] / country_total_expenditure
        } else {
            # Equal shares if no expenditure data
            alpha_matrix[n, ] <- 1 / n_sectors
        }
    }
    
    # Ensure rows sum to 1 (normalization)
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
    
    intermediate_flows <- processed_wiod$intermediate_flows
    total_intermediate_inputs <- processed_wiod$total_intermediate_inputs
    
    # Compute intermediate input shares: gamma_jlk = M_jlk / sum_k'(M_jlk')
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    gamma_matrix <- array(0, dim = c(n_countries, n_sectors, n_sectors),
                         dimnames = list(countries, sectors, sectors))
    
    for (j in 1:n_countries) {
        for (l in 1:n_sectors) {
            if (total_intermediate_inputs[j, l] > 0) {
                # Compute shares of each input sector k in production of sector l
                gamma_matrix[j, l, ] <- intermediate_flows[j, l, ] / total_intermediate_inputs[j, l]
            } else {
                # Equal shares if no intermediate input data
                gamma_matrix[j, l, ] <- 1 / n_sectors
            }
            
            # Ensure normalization (shares sum to 1)
            row_sum <- sum(gamma_matrix[j, l, ])
            if (row_sum > 0) {
                gamma_matrix[j, l, ] <- gamma_matrix[j, l, ] / row_sum
            } else {
                gamma_matrix[j, l, ] <- 1 / n_sectors
            }
        }
    }
    
    cat("✓ Gamma parameters extracted: intermediate input shares\n")
    cat("  - Dimensions:", paste(dim(gamma_matrix), collapse = " x "), "\n") 
    cat("  - Average intermediate share:", round(mean(gamma_matrix, na.rm = TRUE), 4), "\n")
    
    return(gamma_matrix)
}

#' Extract and estimate technology parameters using CDK regression methodology
#' @param processed_wiod Processed WIOD data from process_wiod_data()
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @param theta Trade elasticity parameter
#' @return Matrix of technology parameters (countries x sectors)
get_technology_parameters <- function(processed_wiod, countries, sectors, theta = 6.53) {
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    cat("✓ Technology parameters estimated using CDK regression approach\n")
    
    # Extract building blocks
    trade_flows_array <- processed_wiod$trade_flows_array
    total_expenditures <- processed_wiod$total_expenditures
    
    # Step 1: Calculate trade shares π_ijs = X_ijs / Y_js
    trade_shares_array <- array(0, dim = c(n_countries, n_countries, n_sectors))
    
    for (s in 1:n_sectors) {
        for (j in 1:n_countries) {
            if (total_expenditures[j, s] > 0) {
                trade_shares_array[, j, s] <- trade_flows_array[, j, s] / total_expenditures[j, s]
            }
        }
    }
    
    # Step 2: Prepare regression data in long format
    regression_data <- data.frame()
    
    for (s in 1:n_sectors) {
        for (i in 1:n_countries) {
            for (j in 1:n_countries) {
                
                pi_ijs <- trade_shares_array[i, j, s]
                
                # Skip if trade share is zero or missing
                if (is.na(pi_ijs) || pi_ijs <= 0) {
                    next
                }
                
                # CDK regression variables
                row_data <- data.frame(
                    origin_country = countries[i],
                    destination_country = countries[j],
                    sector = sectors[s],
                    origin_id = i,
                    dest_id = j,
                    sector_id = s,
                    log_pi_ijs = log(pi_ijs),
                    # Fixed effects identifiers
                    origin_sector = paste(i, s, sep = "_"),
                    dest_sector = paste(j, s, sep = "_"),
                    # Other controls
                    domestic = as.numeric(i == j),
                    trade_flow = trade_flows_array[i, j, s],
                    expenditure = total_expenditures[j, s]
                )
                
                regression_data <- rbind(regression_data, row_data)
            }
        }
    }
    
    cat("  - Regression observations:", nrow(regression_data), "\n")
    
    # Step 3: Run CDK reduced form regression ln(π_ijs) = α_is + γ_js + ε_ijs
    regression_formula <- log_pi_ijs ~ factor(origin_sector) + factor(dest_sector) - 1
    regression_result <- fixest::feols(regression_formula, data = regression_data)
    
    # Step 4: Extract fixed effects coefficients
    coef_vec <- coef(regression_result)
    coef_names <- names(coef_vec)
    
    # Separate origin-sector effects (contain technology parameters)
    origin_sector_effects <- coef_vec[grepl("factor\\(origin_sector\\)", coef_names)]
    
    # Step 5: Extract technology parameters from origin-sector fixed effects
    T_matrix <- matrix(1, n_countries, n_sectors)
    
    for (effect_name in names(origin_sector_effects)) {
        # Parse "factor(origin_sector)1_1" format
        clean_name <- gsub("factor\\(origin_sector\\)", "", effect_name)
        parts <- strsplit(clean_name, "_")[[1]]
        
        if (length(parts) == 2) {
            country_id <- as.numeric(parts[1])
            sector_id <- as.numeric(parts[2])
            
            if (!is.na(country_id) && !is.na(sector_id) && 
                country_id <= n_countries && sector_id <= n_sectors) {
                # Transform fixed effect to technology parameter: T_ik = exp(α_is / θ)
                T_matrix[country_id, sector_id] <- exp(origin_sector_effects[[effect_name]]/theta)
            }
        }
    }
    
    # Step 6: Handle missing values using softImpute matrix completion
    if (any(is.na(T_matrix)) || any(T_matrix <= 0)) {
        # Convert invalid values to NA for softImpute
        T_matrix[T_matrix <= 0] <- NA
        
        # Apply softImpute for matrix completion
        if (sum(!is.na(T_matrix)) > 0) {
            # softImpute requires sufficient non-missing entries
            soft_result <- softImpute::softImpute(T_matrix, rank.max = min(dim(T_matrix)) - 1)
            T_completed <- softImpute::complete(T_matrix, soft_result)
            
            T_matrix <- T_completed
            
            cat("  - Missing/invalid values imputed using softImpute\n")
        }
    }
    
    # Ensure proper dimensions and names
    rownames(T_matrix) <- countries
    colnames(T_matrix) <- sectors
    
    cat("  - Dimensions:", paste(dim(T_matrix), collapse = " x "), "\n")
    cat("  - Average productivity:", round(mean(T_matrix), 4), "\n")
    
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
        total_expenditures = processed_wiod$total_expenditures,
        bilateral_trade_shares = processed_wiod$bilateral_trade_shares,
        
        # Processed IO structure
        beta_matrix = processed_socioeco$beta_matrix,
        
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