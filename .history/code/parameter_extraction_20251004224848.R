###############################################################################
# Parameter Extraction Functions for CDK (2012) Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

#' Extract alpha parameters (expenditure shares) from WIOD data
#' @param cleaned_data Cleaned WIOD and socioeconomic database
#' @return Matrix of expenditure shares (countries x sectors)
get_alpha_parameters <- function(cleaned_data) {
    
    # Extract expenditure matrix and total expenditure by country
    expenditure_matrix <- cleaned_data$expenditure_matrix
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    
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

#' Extract beta parameters (labor shares) from socioeconomic data
#' @param cleaned_data Cleaned WIOD and socioeconomic database  
#' @return Matrix of labor shares (countries x sectors)
get_beta_parameters <- function(cleaned_data) {
    
    beta_matrix <- cleaned_data$beta_matrix
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    
    # Validate beta parameters
    if (is.null(beta_matrix)) {
        stop("Beta matrix not found in cleaned data")
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

#' Extract gamma parameters (intermediate input shares) from WIOD data
#' @param cleaned_data Cleaned WIOD and socioeconomic database
#' @return Array of intermediate input shares (countries x sectors x sectors) 
get_gamma_parameters <- function(cleaned_data) {
    
    gamma_matrix <- cleaned_data$gamma_matrix
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    
    # Validate gamma parameters
    if (is.null(gamma_matrix)) {
        stop("Gamma matrix not found in cleaned data")
    }
    
    # Ensure proper dimensions and names
    dimnames(gamma_matrix) <- list(countries, sectors, sectors)
    
    # Validate bounds and normalization (shares should sum to 1 across k')
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            # Ensure non-negative
            gamma_matrix[i, k, ][gamma_matrix[i, k, ] < 0] <- 0
            
            # Normalize to sum to 1
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

#' Extract technology parameters from WIOD data and reduced-form regression
#' @param cleaned_data Cleaned WIOD and socioeconomic database
#' @param theta Trade elasticity parameter
#' @return Matrix of technology parameters (countries x sectors)
get_technology_parameters <- function(cleaned_data, theta = 6.53) {
    
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    
    # Prepare regression data (keeping original function intact)
    regression_data <- prepare_cdk_regression_data(
        cleaned_data$trade_flows_array, 
        cleaned_data$expenditure_matrix,
        countries, 
        sectors
    )
    
    # Estimate technology parameters (keeping original function intact)
    tech_estimation <- estimate_cdk_technology_params(regression_data, theta = theta)
    T_matrix <- tech_estimation$technology_composite
    
    # Ensure proper dimensions and names
    rownames(T_matrix) <- countries
    colnames(T_matrix) <- sectors
    
    cat("✓ Technology parameters extracted from regression\n")
    cat("  - Dimensions:", paste(dim(T_matrix), collapse = " x "), "\n")
    cat("  - Average productivity:", round(mean(T_matrix, na.rm = TRUE), 4), "\n")
    cat("  - Trade elasticity used:", theta, "\n")
    
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

#' Extract tariff data from cleaned database
#' @param cleaned_data Cleaned WIOD and socioeconomic database
#' @return Array of tariff rates (origin x destination x sector)
get_tariff_data <- function(cleaned_data) {
    
    tariffs <- cleaned_data$tariffs
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    
    if (is.null(tariffs)) {
        # Create zero tariff matrix if no data available
        n_countries <- length(countries)
        n_sectors <- length(sectors)
        tariffs <- array(0, dim = c(n_countries, n_countries, n_sectors))
        dimnames(tariffs) <- list(countries, countries, sectors)
        
        cat("✓ Tariff data: using zero baseline (no tariff data available)\n")
    } else {
        # Ensure proper dimensions and names
        dimnames(tariffs) <- list(countries, countries, sectors)
        
        cat("✓ Tariff data extracted\n")
        cat("  - Dimensions:", paste(dim(tariffs), collapse = " x "), "\n")
        cat("  - Non-zero tariffs:", sum(tariffs > 0), "\n")
        cat("  - Average tariff rate:", round(mean(tariffs[tariffs > 0]), 4), "\n")
    }
    
    return(tariffs)
}

#' Extract all model parameters in one function
#' @param cleaned_data Cleaned WIOD and socioeconomic database
#' @param theta Trade elasticity (optional)
#' @return List containing all model parameters
extract_all_parameters <- function(cleaned_data, theta = 6.53) {
    
    cat("Extracting all model parameters...\n")
    
    parameters <- list(
        alpha = get_alpha_parameters(cleaned_data),
        beta = get_beta_parameters(cleaned_data),
        gamma = get_gamma_parameters(cleaned_data),
        technology = get_technology_parameters(cleaned_data, theta),
        theta = get_trade_elasticity(theta),
        tariffs = get_tariff_data(cleaned_data)
    )
    
    cat("✓ All parameters extracted successfully\n")
    
    return(parameters)
}