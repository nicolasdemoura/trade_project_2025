###############################################################################
# Model Calibration Functions for CDK (2012) Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

#' Calibrate CDK model baseline equilibrium
#' @param parameters List containing all model parameters (alpha, beta, gamma, technology, theta, tariffs)
#' @param cleaned_data Cleaned WIOD and socioeconomic database (for dimensions and observed data)
#' @param labor_mobility Logical. TRUE for mobile labor, FALSE for immobile labor
#' @return List with three components: parameters, model_solution, outcome_variables
calibrate_model <- function(parameters, cleaned_data, labor_mobility = TRUE) {
    
    cat("Starting CDK model calibration...\n")
    
    # Extract dimensions
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    cat("  - Countries:", n_countries, "| Sectors:", n_sectors, "\n")
    cat("  - Labor mobility:", ifelse(labor_mobility, "Mobile", "Immobile"), "\n")
    
    # Extract parameters
    alpha_matrix <- parameters$alpha
    beta_matrix <- parameters$beta  
    gamma_matrix <- parameters$gamma
    T_matrix <- parameters$technology
    theta <- parameters$theta
    baseline_tariffs <- parameters$tariffs
    
    # Extract additional required data
    labor_matrix <- cleaned_data$labor_matrix
    expenditure_matrix <- cleaned_data$expenditure_matrix
    observed_trade_shares <- cleaned_data$observed_trade_shares
    
    # Calibrate equilibrium using existing function (keeping logic intact)
    equilibrium_results <- calibrate_equilibrium_from_trade_shares(
        theta = theta,
        T_matrix = T_matrix,
        labor_matrix = labor_matrix,
        beta_matrix = beta_matrix,
        gamma_matrix = gamma_matrix,
        expenditure_matrix = expenditure_matrix,
        observed_trade_shares = observed_trade_shares,
        baseline_tariffs = baseline_tariffs,
        labor_mobility = labor_mobility
    )
    
    # Extract calibrated equilibrium components
    equilibrium <- equilibrium_results$equilibrium
    
    # ========================================================================
    # ORGANIZE OUTPUT INTO THREE DISTINCT LISTS
    # ========================================================================
    
    # 1. PARAMETERS: Iceberg trade costs
    calibrated_parameters <- list(
        iceberg_costs = equilibrium$iceberg_costs
    )
    
    # 2. MODEL SOLUTION: All equilibrium variables
    model_solution <- list(
        prices = equilibrium$prices,                    # Baseline sectoral prices
        costs = equilibrium$costs,                      # Baseline unit costs
        wages = equilibrium$wages,                      # Baseline wages
        trade_shares = equilibrium$trade_shares,        # Baseline bilateral trade shares
        sector_shares = alpha_matrix,                   # Sectoral expenditure shares
        total_income = equilibrium$total_income,        # Total income by country
        labor_allocation = equilibrium$labor_allocation # Labor allocation
    )
    
    # 3. OUTCOME VARIABLES: Welfare components
    outcome_variables <- list(
        real_income = equilibrium$real_income,          # Real labor income
        real_tariff_revenue = equilibrium$real_tariff_revenue, # Real tariff revenue  
        total_welfare = equilibrium$total_welfare,      # Total real welfare
        price_indexes = equilibrium$aggregate_prices    # Aggregate price indexes
    )
    
    # Calculate additional outcome statistics
    domestic_shares <- numeric(n_countries)
    for (n in 1:n_countries) {
        domestic_shares[n] <- mean(sapply(1:n_sectors, function(k) {
            equilibrium$trade_shares[n, n, k]
        }))
    }
    
    # Add summary statistics
    calibration_summary <- list(
        countries = countries,
        sectors = sectors,
        labor_mobility = labor_mobility,
        theta = theta,
        domestic_trade_share = mean(domestic_shares),
        convergence_achieved = equilibrium_results$convergence$convergence,
        optimization_value = equilibrium_results$convergence$value,
        parameter_count = equilibrium_results$convergence$counts[1]
    )
    
    cat("✓ Model calibration completed successfully\n")
    cat("  - Average domestic trade share:", round(mean(domestic_shares), 3), "\n")
    cat("  - Optimization convergence:", equilibrium_results$convergence$convergence, "\n")
    cat("  - Final objective value:", round(equilibrium_results$convergence$value, 6), "\n")
    
    # Return organized output
    return(list(
        parameters = calibrated_parameters,
        model_solution = model_solution, 
        outcome_variables = outcome_variables,
        summary = calibration_summary
    ))
}