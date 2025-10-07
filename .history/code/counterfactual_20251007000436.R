###############################################################################
# Counterfactual Analysis: Trump Tariff Impact using CDK (2012) Methodology
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

#' Create counterfactual tariff scenario
#' @param baseline_tariffs Baseline tariff array (origin x destination x sector)
#' @param scenario Type of scenario ("trump_10pp", "tariff_rate24", "tariff_rate25_YTD", etc., or "custom")
#' @param custom_tariffs Optional custom tariff array for "custom" scenario
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @param tariff_scenarios Optional list of tariff scenarios from parameters
#' @return Modified tariff array for counterfactual analysis
create_tariff_scenario <- function(baseline_tariffs, us_tariffs = NULL, 
                                  countries, sectors) {
    
    counterfactual_tariffs <- baseline_tariffs
    
    if (!is.null(us_tariffs)) {        
        # Apply US tariffs to all imports into USA
        us_index <- which(countries == "USA")
        for (i in 1:length(countries)) {
            if (i != us_index) {
                for (k in 1:length(sectors)) {
                    country_name <- countries[i]
                    sector_name <- sectors[k]
                    if (sector_name %in% us_tariffs$sector) {
                        tariff_rate <- us_tariffs$tariff_rate[us_tariffs$sector == sector_name & us_tariffs$country == country_name]
                        counterfactual_tariffs[i, us_index, k] <- tariff_rate
                    }
                }
            }
        }
        cat("✓ US tariff scenario applied\n")
    } else {
        cat("No US tariffs provided; using baseline tariffs\n")
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
    fixed_iceberg_costs <- baseline_equilibrium$parameters$iceberg_costs
    
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
    
    cat("✓ Counterfactual analysis completed\n")
    cat("  - Convergence achieved:", counterfactual_solution$converged, "\n")
    cat("  - Final objective value:", round(counterfactual_solution$final_error, 6), "\n")
    
    return(counterfactual_solution)
}

#' Solve counterfactual equilibrium with fixed iceberg costs
#' @param database Processed database containing data
#' @param parameters Model parameters with counterfactual tariffs
#' @param fixed_iceberg_costs Fixed iceberg costs from baseline calibration
#' @param labor_mobility Logical for labor mobility assumption
#' @return Counterfactual equilibrium solution
solve_counterfactual_equilibrium <- function(database, parameters, fixed_iceberg_costs, labor_mobility = TRUE) {
    
    # Extract parameters
    alpha_matrix <- parameters$alpha
    beta_matrix <- parameters$beta
    gamma_matrix <- parameters$gamma
    technology_matrix <- parameters$technology
    theta <- parameters$theta
    tariffs <- parameters$tariffs  # New counterfactual tariffs
    
    # Extract data
    labor_matrix <- database$labor_matrix
    countries <- database$countries
    sectors <- database$sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    cat("Solving counterfactual equilibrium with fixed iceberg costs...\n")
    cat("  - Countries:", n_countries, "| Sectors:", n_sectors, "\n")
    cat("  - Labor mobility:", ifelse(labor_mobility, "Mobile", "Immobile"), "\n")
    
    # Initialize parameter vectors for optimization (wages and prices only)
    # NO iceberg costs - these are fixed from baseline
    if (labor_mobility) {
        n_wages <- n_countries - 1  # Mobile labor: one wage per country
    } else {
        n_wages <- n_countries * n_sectors - 1  # Immobile labor: wage per country-sector
    }
    
    n_prices <- n_countries * n_sectors
    n_params <- n_wages + n_prices
    
    cat("  - Parameters to optimize:", n_params, "(wages:", n_wages, ", prices:", n_prices, ")\n")
    cat("  - Iceberg costs: FIXED from baseline calibration\n")
    
    # Initial parameter values
    initial_params <- numeric(n_params)
    param_idx <- 1
    
    # Initialize wages based on labor mobility (log scale for exp() transformation)
    if (labor_mobility) {
        # Mobile labor: one wage per country (exclude normalization)
        for (i in 2:n_countries) {
            initial_params[param_idx] <- 0.0  # exp(0) = 1
            param_idx <- param_idx + 1
        }
    } else {
        # Immobile labor: wage per country-sector (exclude normalization)
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                if (!(i == 1 && k == 1)) {
                    initial_params[param_idx] <- 0.0  # exp(0) = 1
                    param_idx <- param_idx + 1
                }
            }
        }
    }
    
    # Initialize prices (log scale for exp() transformation)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            initial_params[param_idx] <- 0.0  # exp(0) = 1
            param_idx <- param_idx + 1
        }
    }
    
    # Counterfactual objective function (equilibrium conditions only)
    counterfactual_objective <- function(params) {
        
        param_idx <- 1
        
        # Extract wages based on labor mobility
        wages <- matrix(1.0, n_countries, n_sectors)
        
        if (labor_mobility) {
            # Mobile labor: one wage per country
            wages[1, 1] <- 1.0  # Normalization
            country_wages <- numeric(n_countries)
            country_wages[1] <- 1.0
            
            for (i in 2:n_countries) {
                country_wages[i] <- exp(params[param_idx])
                param_idx <- param_idx + 1
            }
            
            # Apply country wage to all sectors
            for (i in 1:n_countries) {
                for (k in 1:n_sectors) {
                    wages[i, k] <- country_wages[i]
                }
            }
        } else {
            # Immobile labor: sector-specific wages
            wages[1, 1] <- 1.0  # Normalization
            
            for (i in 1:n_countries) {
                for (k in 1:n_sectors) {
                    if (!(i == 1 && k == 1)) {
                        wages[i, k] <- exp(params[param_idx])
                        param_idx <- param_idx + 1
                    }
                }
            }
        }
        
        # Extract prices (with exponential transformation)
        prices <- matrix(0, n_countries, n_sectors)
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                prices[i, k] <- exp(params[param_idx])
                param_idx <- param_idx + 1
            }
        }
        
        # Calculate unit costs using current wages and prices
        unit_costs <- calculate_unit_costs(wages, prices, beta_matrix, gamma_matrix)
        
        # Calculate trade shares using FIXED iceberg costs and NEW tariffs
        trade_shares <- calculate_model_trade_shares(technology_matrix, fixed_iceberg_costs, unit_costs, tariffs, theta)
        
        # Solve income-expenditure system
        labor_income <- numeric(n_countries)
        for (n in 1:n_countries) {
            labor_income[n] <- sum(wages[n, ] * labor_matrix[n, ])
        }
        
        # Calculate tariff revenue coefficient matrix
        tariff_matrix <- matrix(0, n_countries, n_countries)
        for (n in 1:n_countries) {
            tariff_coeff <- 0
            for (k in 1:n_sectors) {
                for (i in 1:n_countries) {
                    tariff_coeff <- tariff_coeff + tariffs[i, n, k] * trade_shares[i, n, k] * alpha_matrix[n, k]
                }
            }
            tariff_matrix[n, n] <- tariff_coeff
        }
        
        # Solve linear system for total income
        income_system_matrix <- diag(n_countries) - tariff_matrix
        total_income <- solve(income_system_matrix, labor_income)
        
        # Calculate expenditures
        expenditures <- matrix(0, n_countries, n_sectors)
        for (n in 1:n_countries) {
            for (k in 1:n_sectors) {
                expenditures[n, k] <- alpha_matrix[n, k] * total_income[n]
            }
        }
        
        # Calculate equilibrium violations (trade balance + price consistency)
        violations <- 0
        
        # Trade balance conditions
        if (labor_mobility) {
            # Mobile labor: export revenues = labor income
            for (i in 1:n_countries) {
                lhs <- 0  # Export revenues
                for (k in 1:n_sectors) {
                    for (n in 1:n_countries) {
                        lhs <- lhs + trade_shares[i, n, k] * expenditures[n, k]
                    }
                }
                
                rhs <- 0  # Labor income
                for (k in 1:n_sectors) {
                    rhs <- rhs + wages[i, k] * labor_matrix[i, k]
                }
                
                violations <- violations + (lhs - rhs)^2 * 1e-9
            }
        } else {
            # Immobile labor: sector export revenues = sector labor income
            for (i in 1:n_countries) {
                for (k in 1:n_sectors) {
                    lhs <- 0  # Sector export revenues
                    for (n in 1:n_countries) {
                        lhs <- lhs + trade_shares[i, n, k] * expenditures[n, k]
                    }
                    
                    rhs <- wages[i, k] * labor_matrix[i, k]  # Sector labor income
                    
                    violations <- violations + (lhs - rhs)^2 * 1e-9
                }
            }
        }
        
        # Price consistency: p_nk = [∑T_ik(c_ik*d_nik*(1+τ_nik))^(-θ)]^(-1/θ)
        for (n in 1:n_countries) {
            for (k in 1:n_sectors) {
                price_index_sum <- 0
                for (i in 1:n_countries) {
                    trade_cost <- unit_costs[i, k] * fixed_iceberg_costs[i, n, k] * (1 + tariffs[i, n, k])
                    price_index_sum <- price_index_sum + technology_matrix[i, k] * (trade_cost^(-theta))
                }

                price_index_sum <- price_index_sum^(-1/theta)
                violations <- violations + (prices[n, k] - price_index_sum)^2
                
            }
        }
        
        return(violations)
    }
    
    # Perform optimization
    cat("  - Starting equilibrium optimization...\n")
    
    optimization_result <- optim(
        par = initial_params,
        fn = counterfactual_objective,
        method = "BFGS",
        control = list(
            maxit = 1000,  
            reltol = 1e-1,   
            trace = 1,
            REPORT = 10
        )
    )
    
    cat("  ✓ Optimization completed\n")
    cat("  - Convergence:", optimization_result$convergence == 0, "\n")
    cat("  - Final objective value:", round(optimization_result$value, 6), "\n")
    
    # Extract final results
    final_params <- optimization_result$par
    param_idx <- 1
    
    # Extract final wages
    wages <- matrix(1.0, n_countries, n_sectors)
    
    if (labor_mobility) {
        wages[1, 1] <- 1.0
        country_wages <- numeric(n_countries)
        country_wages[1] <- 1.0
        
        for (i in 2:n_countries) {
            country_wages[i] <- exp(final_params[param_idx])
            param_idx <- param_idx + 1
        }
        
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                wages[i, k] <- country_wages[i]
            }
        }
    } else {
        wages[1, 1] <- 1.0
        
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                if (!(i == 1 && k == 1)) {
                    wages[i, k] <- exp(final_params[param_idx])
                    param_idx <- param_idx + 1
                }
            }
        }
    }
    
    # Extract final prices (with exponential transformation)
    prices <- matrix(0, n_countries, n_sectors)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            prices[i, k] <- exp(final_params[param_idx])
            param_idx <- param_idx + 1
        }
    }
    
    # Calculate final equilibrium outcomes
    unit_costs <- calculate_unit_costs(wages, prices, beta_matrix, gamma_matrix)
    trade_shares <- calculate_model_trade_shares(technology_matrix, fixed_iceberg_costs, unit_costs, tariffs, theta)
    
    # Calculate final income and welfare
    labor_income <- numeric(n_countries)
    for (n in 1:n_countries) {
        labor_income[n] <- sum(wages[n, ] * labor_matrix[n, ])
    }
    
    tariff_matrix <- matrix(0, n_countries, n_countries)
    for (n in 1:n_countries) {
        tariff_coeff <- 0
        for (k in 1:n_sectors) {
            for (i in 1:n_countries) {
                tariff_coeff <- tariff_coeff + tariffs[i, n, k] * trade_shares[i, n, k] * alpha_matrix[n, k]
            }
        }
        tariff_matrix[n, n] <- tariff_coeff
    }
    
    income_system_matrix <- diag(n_countries) - tariff_matrix
    total_income <- solve(income_system_matrix, labor_income)
    
    expenditures <- matrix(0, n_countries, n_sectors)
    for (n in 1:n_countries) {
        for (k in 1:n_sectors) {
            expenditures[n, k] <- alpha_matrix[n, k] * total_income[n]
        }
    }
    
    # Calculate welfare measures
    real_income <- numeric(n_countries)
    real_tariff_revenue <- numeric(n_countries)
    aggregate_prices <- numeric(n_countries)
    
    for (n in 1:n_countries) {
        country_labor_income <- sum(wages[n, ] * labor_matrix[n, ])
        
        country_tariff_revenue <- 0
        for (k in 1:n_sectors) {
            for (i in 1:n_countries) {
                country_tariff_revenue <- country_tariff_revenue + tariffs[i, n, k] * trade_shares[i, n, k] * expenditures[n, k]
            }
        }
        
        aggregate_price <- prod(prices[n, ]^alpha_matrix[n, ])
        
        real_income[n] <- country_labor_income / aggregate_price
        real_tariff_revenue[n] <- country_tariff_revenue / aggregate_price
        aggregate_prices[n] <- aggregate_price
    }
    
    # Add dimension names
    dimnames(wages) <- list(countries, sectors)
    dimnames(prices) <- list(countries, sectors)
    dimnames(unit_costs) <- list(countries, sectors)
    dimnames(trade_shares) <- list(countries, countries, sectors)
    dimnames(expenditures) <- list(countries, sectors)
    names(total_income) <- countries
    names(real_income) <- countries
    names(real_tariff_revenue) <- countries
    names(aggregate_prices) <- countries
    
    return(list(
        wages = wages,
        prices = prices,
        unit_costs = unit_costs,
        trade_shares = trade_shares,
        expenditures = expenditures,
        total_income = total_income,
        real_income = real_income,
        real_tariff_revenue = real_tariff_revenue,
        total_welfare = real_income + real_tariff_revenue,
        aggregate_prices = aggregate_prices,
        labor_allocation = labor_matrix,
        iceberg_costs = fixed_iceberg_costs,  # Return the fixed iceberg costs
        converged = optimization_result$convergence == 0,
        iterations = optimization_result$counts[1],
        final_error = optimization_result$value,
        labor_mobility = labor_mobility
    ))
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

#' Run multiple counterfactual scenarios and compare results
#' @param baseline_equilibrium Baseline equilibrium solution from calibration
#' @param database Processed database (for dimensions and data)
#' @param parameters Model parameters with tariff scenarios
#' @param scenarios Vector of scenario names to run
#' @param labor_mobility Logical for labor mobility assumption
#' @return List of results for each scenario
run_multiple_counterfactual_scenarios <- function(baseline_equilibrium, database, parameters, 
                                                 scenarios = c("tariff_rate24", "tariff_rate25_YTD", "tariff_rate25_LTM", "tariff_rate25_3M"), 
                                                 labor_mobility = TRUE) {
    
    cat("=== Running Multiple Counterfactual Scenarios ===\n")
    
    countries <- database$countries
    sectors <- database$sectors
    
    results <- list()
    baseline_outcomes <- baseline_equilibrium$outcome_variables

    for (scenario in scenarios) {
        cat("\n--- Running scenario:", scenario, "---\n")
        
        if (!scenario %in% names(database$tariff_scenarios)) {
            cat("  ⚠ Scenario not available, skipping...\n")
            next
        }

    us_tariffs <- database$tariff_scenarios %>%
        dplyr::select(country, sector, scenario) %>%
        rename(tariff_rate = scenario)

        # Create counterfactual tariff array for this scenario
        scenario_tariffs <- create_tariff_scenario(
            baseline_tariffs = database$tariffs,
            us_tariffs = us_tariffs,
            countries = countries,
            sectors = sectors
        )
        
        # Run counterfactual analysis
        counterfactual_result <- run_counterfactual(
            baseline_equilibrium = baseline_equilibrium,
            new_tariffs = scenario_tariffs,
            database = database,
            parameters = parameters,
            labor_mobility = labor_mobility
        )
        
        # Analyze welfare effects
        welfare_analysis <- analyze_welfare_effects_cdk(
            baseline_outcomes = baseline_outcomes,
            counterfactual_outcomes = counterfactual_result,
            countries = countries
        )
        
        # Store results
        results[[scenario]] <- list(
            counterfactual_outcomes = counterfactual_result,
            welfare_analysis = welfare_analysis,
            scenario_tariffs = scenario_tariffs
        )
        
        cat("  ✓ Scenario completed:", scenario, "\n")
    }
    
    cat("\n✓ All counterfactual scenarios completed\n")
    cat("  - Scenarios run:", length(results), "\n")
    
    return(results)
}

export_welfare_tables <- function(welfare_results, baseline_equilibrium, database, output_dir = "results/welfare_tables") {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }
    
    # Get countries and sectors
    countries <- database$countries
    sectors <- database$sectors
    us_index <- which(countries == "USA")
    
    # Calculate baseline trade-weighted average US tariffs
    baseline_tariffs <- database$tariffs
    baseline_trade_shares <- baseline_equilibrium$model_solution$trade_shares
    baseline_expenditures <- baseline_equilibrium$model_solution$expenditures
    
    baseline_us_tariffs <- calculate_trade_weighted_tariffs(
        baseline_tariffs, baseline_trade_shares, baseline_expenditures, 
        us_index, countries, sectors
    )
    
    # Create welfare comparison table for all scenarios
    welfare_table <- data.frame()
    
    for (scenario_name in names(welfare_results)) {
        scenario_result <- welfare_results[[scenario_name]]
        
        # Calculate counterfactual trade-weighted average US tariffs
        counterfactual_tariffs <- scenario_result$scenario_tariffs
        counterfactual_trade_shares <- scenario_result$counterfactual_outcomes$trade_shares
        counterfactual_expenditures <- scenario_result$counterfactual_outcomes$expenditures
        
        counterfactual_us_tariffs <- calculate_trade_weighted_tariffs(
            counterfactual_tariffs, counterfactual_trade_shares, counterfactual_expenditures,
            us_index, countries, sectors
        )
        
        # Get welfare changes for USA
        welfare_effects <- scenario_result$welfare_analysis$welfare_effects
        us_welfare <- welfare_effects[welfare_effects$country == "USA", ]
        
        # Create row for this scenario
        scenario_row <- data.frame(
            Scenario = scenario_name,
            Baseline_US_Tariff = round(baseline_us_tariffs * 100, 2),
            Counterfactual_US_Tariff = round(counterfactual_us_tariffs * 100, 2),
            Welfare_Change = round(us_welfare$welfare_change_pct, 2),
            Income_Change = round((us_welfare$income_change / baseline_equilibrium$outcome_variables$real_income["USA"]) * 100, 2),
            Tariff_Revenue_Change = round((us_welfare$tariff_revenue_change / baseline_equilibrium$outcome_variables$real_tariff_revenue["USA"]) * 100, 2),
            stringsAsFactors = FALSE
        )
        
        welfare_table <- rbind(welfare_table, scenario_row)
    }
    
    # Create LaTeX table
    create_latex_table(welfare_table,
                      "welfare_comparison_table",
                      "Welfare Effects by Scenario - United States",
                      "tab:welfare_comparison",
                      output_dir)
    
    # Also create individual country tables for each scenario
    for (scenario_name in names(welfare_results)) {
        welfare_effects <- welfare_results[[scenario_name]]$welfare_analysis$welfare_effects
        
        create_latex_table(welfare_effects,
                          paste0("welfare_effects_", scenario_name),
                          paste0("Welfare Effects - ", scenario_name),
                          paste0("tab:welfare_", scenario_name),
                          output_dir)
    }
    
    cat("✓ Welfare tables exported to:", output_dir, "\n")
}

# Helper function to calculate trade-weighted average tariffs
calculate_trade_weighted_tariffs <- function(tariffs, trade_shares, expenditures, us_index, countries, sectors) {
    total_us_imports <- 0
    weighted_tariff_sum <- 0
    
    for (i in 1:length(countries)) {
        if (i != us_index) {
            for (k in 1:length(sectors)) {
                import_value <- trade_shares[i, us_index, k] * expenditures[us_index, k]
                tariff_rate <- tariffs[i, us_index, k]
                
                total_us_imports <- total_us_imports + import_value
                weighted_tariff_sum <- weighted_tariff_sum + (tariff_rate * import_value)
            }
        }
    }
    
    return(weighted_tariff_sum / total_us_imports)
}
