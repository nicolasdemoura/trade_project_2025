###############################################################################
# Counterfactual Analysis: Trump Tariff Impact using CDK (2012) Methodology
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

#' Create counterfactual tariff scenario
#' @param baseline_tariffs Baseline tariff array (origin x destination x sector)
#' @param scenario Type of scenario ("trump_10pp" for 10pp US increase, "custom" for user-defined)
#' @param custom_tariffs Optional custom tariff array for "custom" scenario
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Modified tariff array for counterfactual analysis
create_tariff_scenario <- function(baseline_tariffs, scenario = "trump_10pp", custom_tariffs = NULL, countries, sectors) {
    
    counterfactual_tariffs <- baseline_tariffs
    
    if (scenario == "trump_10pp") {
        # Default: 10 percentage point homogeneous increase from US to all trade partners
        us_index <- which(countries == "USA")
        if (length(us_index) == 0) {
            stop("USA not found in countries. Available countries: ", paste(countries, collapse = ", "))
        }
        
        # Increase US import tariffs by 10 percentage points across all sectors for the non-US countries
        # Ensure we do not modify domestic tariffs (US to US)
        counterfactual_tariffs[us_index, , ] <- counterfactual_tariffs[us_index, , ] + 0.10
        counterfactual_tariffs[us_index, us_index, ] <- baseline_tariffs[us_index, us_index, ]


        cat("✓ Trump scenario: +10pp US tariffs on all imports\n")
        cat("  - US index:", us_index, "(", countries[us_index], ")\n")
        cat("  - Sectors affected:", length(sectors), "\n")
        
    } else if (scenario == "custom") {
        if (is.null(custom_tariffs)) {
            stop("Custom tariffs must be provided for custom scenario")
        }
        counterfactual_tariffs <- custom_tariffs
        cat("✓ Custom tariff scenario applied\n")
        
    } else {
        stop("Unknown scenario: ", scenario, ". Use 'trump_10pp' or 'custom'")
    }
    
    return(counterfactual_tariffs)
}

#' Run counterfactual analysis with fixed iceberg costs
#' @param baseline_equilibrium Baseline equilibrium solution from calibration
#' @param new_tariffs Counterfactual tariff array (origin x destination x sector)
#' @param database Processed database (for dimensions and data)
#' @param parameters Model parameters (alpha, beta, gamma, technology, theta)
#' @param labor_mobility Logical for labor mobility assumption
#' @return Counterfactual model outcomes
run_counterfactual <- function(baseline_equilibrium, new_tariffs, database, parameters, labor_mobility = TRUE) {
    
    cat("Running counterfactual analysis with fixed iceberg costs...\n")
    
    # Extract dimensions
    countries <- database$countries
    sectors <- database$sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    cat("  - Countries:", n_countries, "| Sectors:", n_sectors, "\n")
    cat("  - Labor mobility:", ifelse(labor_mobility, "Mobile", "Immobile"), "\n")
    cat("  - Using calibrated iceberg costs from baseline\n")
    
    # Use fixed iceberg costs from baseline calibration
    fixed_iceberg_costs <- baseline_equilibrium$iceberg_costs
    
    # Create counterfactual parameters with new tariffs but same iceberg costs
    counterfactual_params <- parameters
    counterfactual_params$tariffs <- new_tariffs
    
    # Solve counterfactual equilibrium using the method of moments solver 
    # but with fixed iceberg costs (no need to match observed trade shares)
    counterfactual_solution <- solve_counterfactual_equilibrium(
        database = database,
        parameters = counterfactual_params,
        fixed_iceberg_costs = fixed_iceberg_costs,
        labor_mobility = labor_mobility
    )
    
    # Organize counterfactual outcomes
    counterfactual_outcomes <- list(
        # Model solution
        prices = equilibrium$prices,
        costs = equilibrium$costs,
        wages = equilibrium$wages,
        trade_shares = equilibrium$trade_shares,
        total_income = equilibrium$total_income,
        labor_allocation = equilibrium$labor_allocation,
        
        # Outcome variables
        real_income = equilibrium$real_income,
        real_tariff_revenue = equilibrium$real_tariff_revenue,
        total_welfare = equilibrium$total_welfare,
        price_indexes = equilibrium$aggregate_prices,
        
        # Convergence info
        convergence_achieved = counterfactual_equilibrium$convergence$convergence,
        optimization_value = counterfactual_equilibrium$convergence$value
    )
    
    cat("✓ Counterfactual analysis completed\n")
    cat("  - Convergence achieved:", counterfactual_equilibrium$convergence$convergence, "\n")
    cat("  - Final objective value:", round(counterfactual_equilibrium$convergence$value, 6), "\n")
    
    return(counterfactual_outcomes)
}

#' Analyze welfare effects between baseline and counterfactual
#' @param baseline_outcomes Baseline model outcomes  
#' @param counterfactual_outcomes Counterfactual model outcomes
#' @param countries Vector of country names
#' @return Welfare comparison analysis
analyze_welfare_effects_cdk <- function(baseline_outcomes, counterfactual_outcomes, countries) {
    
    cat("Analyzing welfare effects...\n")
    
    # Extract welfare components
    baseline_welfare <- baseline_outcomes$total_welfare
    counterfactual_welfare <- counterfactual_outcomes$total_welfare
    
    baseline_real_income <- baseline_outcomes$real_income
    counterfactual_real_income <- counterfactual_outcomes$real_income
    
    baseline_tariff_revenue <- baseline_outcomes$real_tariff_revenue  
    counterfactual_tariff_revenue <- counterfactual_outcomes$real_tariff_revenue
    
    # Calculate welfare changes
    welfare_change <- counterfactual_welfare - baseline_welfare
    welfare_change_pct <- (welfare_change / baseline_welfare) * 100
    
    # Calculate components of welfare change
    income_change <- counterfactual_real_income - baseline_real_income
    tariff_revenue_change <- counterfactual_tariff_revenue - baseline_tariff_revenue
    
    # Create welfare analysis results
    welfare_effects <- data.frame(
        country = countries,
        baseline_welfare = baseline_welfare,
        counterfactual_welfare = counterfactual_welfare, 
        welfare_change = welfare_change,
        welfare_change_pct = welfare_change_pct,
        income_change = income_change,
        tariff_revenue_change = tariff_revenue_change,
        stringsAsFactors = FALSE
    )
    
    # Summary statistics
    welfare_summary <- list(
        total_countries = length(countries),
        countries_better_off = sum(welfare_change > 0),
        countries_worse_off = sum(welfare_change < 0),
        average_welfare_change = mean(welfare_change_pct),
        median_welfare_change = median(welfare_change_pct),
        max_welfare_gain = max(welfare_change_pct),
        max_welfare_loss = min(welfare_change_pct)
    )
    
    cat("✓ Welfare effects analysis completed\n")
    cat("  - Countries better off:", welfare_summary$countries_better_off, "\n")
    cat("  - Countries worse off:", welfare_summary$countries_worse_off, "\n")
    cat("  - Average welfare change:", round(welfare_summary$average_welfare_change, 2), "%\n")
    
    return(list(
        welfare_effects = welfare_effects,
        welfare_summary = welfare_summary
    ))
}