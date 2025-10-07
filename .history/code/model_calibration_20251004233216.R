###############################################################################
# Model Calibration Functions for CDK (2012) Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

#' Solve CDK equilibrium system with joint estimation of iceberg costs
#' @param database Processed database containing observed trade data
#' @param parameters List containing all model parameters
#' @param labor_mobility Logical. TRUE for mobile labor, FALSE for immobile labor
#' @param max_iter Maximum iterations for convergence
#' @param tolerance Convergence tolerance
#' @return Solved equilibrium with jointly estimated iceberg costs
solve_cdk_equilibrium <- function(database, parameters, labor_mobility = TRUE, max_iter = 1000, tolerance = 1e-6) {
    
    # Extract parameters
    alpha_matrix <- parameters$alpha
    beta_matrix <- parameters$beta
    gamma_matrix <- parameters$gamma
    technology_matrix <- parameters$technology
    theta <- parameters$theta
    tariffs <- parameters$tariffs
    
    # Extract data
    labor_matrix <- database$labor_matrix
    observed_trade_shares <- database$bilateral_trade_shares
    
    countries <- database$countries
    sectors <- database$sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    cat("Solving CDK equilibrium system with joint iceberg cost estimation...\n")
    cat("  - Countries:", n_countries, "| Sectors:", n_sectors, "\n")
    cat("  - Labor mobility:", ifelse(labor_mobility, "Mobile", "Immobile"), "\n")
    
    # Initialize endogenous variables
    wages <- matrix(1, n_countries, n_sectors)
    prices <- matrix(1, n_countries, n_sectors)
    unit_costs <- matrix(1, n_countries, n_sectors)
    trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors))
    expenditures <- matrix(0, n_countries, n_sectors)
    total_income <- numeric(n_countries)
    
    # Initialize iceberg costs (jointly estimated)
    iceberg_costs <- array(1.5, dim = c(n_countries, n_countries, n_sectors))
    # Set domestic costs to 1 (normalization)
    for (k in 1:n_sectors) {
        for (i in 1:n_countries) {
            iceberg_costs[i, i, k] <- 1.0
        }
    }
    
    # Set wage normalization: first country, first sector = 1
    wages[1, 1] <- 1
    
    # Iterative solution of equilibrium system with joint iceberg cost estimation
    for (iter in 1:max_iter) {
        
        # Store previous iteration values
        wages_old <- wages
        prices_old <- prices
        iceberg_costs_old <- iceberg_costs
        
        # Step 1: Update unit costs using equation: c_ik = w_ik^β_ik * ∏p_ik'^γ_ikk'
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                intermediate_price_index <- 1
                for (kprime in 1:n_sectors) {
                    intermediate_price_index <- intermediate_price_index * (prices[i, kprime] ^ gamma_matrix[i, k, kprime])
                }
                unit_costs[i, k] <- (wages[i, k] ^ beta_matrix[i, k]) * 
                                   (intermediate_price_index ^ (1 - beta_matrix[i, k]))
            }
        }
        
        # Step 2: Update price indices using: p_nk = [∑T_ik(c_ik*d_nik*(1+τ_nik))^(-θ)]^(-1/θ)
        for (n in 1:n_countries) {
            for (k in 1:n_sectors) {
                price_denominator <- 0
                for (i in 1:n_countries) {
                    trade_cost_total <- unit_costs[i, k] * iceberg_costs[i, n, k] * (1 + tariffs[i, n, k])
                    price_denominator <- price_denominator + technology_matrix[i, k] * (trade_cost_total ^ (-theta))
                }
                prices[n, k] <- price_denominator ^ (-1/theta)
            }
        }
        
        # Step 3: Update trade shares using: π_nik = T_ik(c_ik*d_nik*(1+τ_nik))^(-θ) / ∑T_i'k(...)^(-θ)
        for (n in 1:n_countries) {
            for (k in 1:n_sectors) {
                denominator <- 0
                for (i in 1:n_countries) {
                    trade_cost_total <- unit_costs[i, k] * iceberg_costs[i, n, k] * (1 + tariffs[i, n, k])
                    denominator <- denominator + technology_matrix[i, k] * (trade_cost_total ^ (-theta))
                }
                
                for (i in 1:n_countries) {
                    trade_cost_total <- unit_costs[i, k] * iceberg_costs[i, n, k] * (1 + tariffs[i, n, k])
                    numerator <- technology_matrix[i, k] * (trade_cost_total ^ (-theta))
                    trade_shares[i, n, k] <- numerator / denominator
                }
            }
        }
        
        # Step 4: Calculate total income Y_n = ∑w_nk*L_nk + ∑∑τ_nik*π_nik*X_nk (Income Identity)
        # First calculate expenditures and then update income iteratively
        for (n in 1:n_countries) {
            # Labor income component
            labor_income <- 0
            for (k in 1:n_sectors) {
                labor_income <- labor_income + wages[n, k] * labor_matrix[n, k]
            }
            
            # Initial income estimate (will be updated)
            total_income[n] <- labor_income
        }
        
        # Update expenditures and income iteratively (since they depend on each other)
        for (inner_iter in 1:50) {
            total_income_old <- total_income
            
            # Update expenditures: X_nk = α_nk * Y_n (Market Clearing)
            for (n in 1:n_countries) {
                for (k in 1:n_sectors) {
                    expenditures[n, k] <- alpha_matrix[n, k] * total_income[n]
                }
            }
            
            # Update total income with tariff revenue
            for (n in 1:n_countries) {
                # Labor income
                labor_income <- 0
                for (k in 1:n_sectors) {
                    labor_income <- labor_income + wages[n, k] * labor_matrix[n, k]
                }
                
                # Tariff revenue
                tariff_revenue <- 0
                for (k in 1:n_sectors) {
                    for (i in 1:n_countries) {
                        tariff_revenue <- tariff_revenue + tariffs[i, n, k] * trade_shares[i, n, k] * expenditures[n, k]
                    }
                }
                
                total_income[n] <- labor_income + tariff_revenue
            }
            
            # Check inner convergence
            if (max(abs(total_income - total_income_old)) < tolerance) {
                break
            }
        }
        
        # Step 5: Update wages using trade balance condition
        if (labor_mobility) {
            # Mobile labor: ∑∑π_ink*α_nk*Y_n = ∑w_ik*L_ik + ∑∑τ_nik*π_nik*α_nk*Y_n
            for (i in 1:n_countries) {
                total_exports <- 0
                total_labor_income <- 0
                total_tariff_revenue_from_exports <- 0
                
                for (k in 1:n_sectors) {
                    # Export revenue
                    for (n in 1:n_countries) {
                        total_exports <- total_exports + trade_shares[i, n, k] * alpha_matrix[n, k] * total_income[n]
                    }
                    
                    # Labor costs
                    total_labor_income <- total_labor_income + wages[i, k] * labor_matrix[i, k]
                    
                    # Tariff revenue generated by exports
                    for (n in 1:n_countries) {
                        total_tariff_revenue_from_exports <- total_tariff_revenue_from_exports + 
                            tariffs[i, n, k] * trade_shares[i, n, k] * alpha_matrix[n, k] * total_income[n]
                    }
                }
                
                # Update wages proportionally (mobile labor assumption)
                if (total_labor_income > 0) {
                    wage_adjustment <- (total_exports - total_tariff_revenue_from_exports) / total_labor_income
                    for (k in 1:n_sectors) {
                        if (!(i == 1 && k == 1)) {  # Skip normalization
                            wages[i, k] <- wages[i, k] * wage_adjustment
                        }
                    }
                }
            }
        } else {
            # Immobile labor: ∑π_ink*α_nk*Y_n = w_ik*L_ik + ∑τ_nik*π_nik*α_nk*Y_n
            for (i in 1:n_countries) {
                for (k in 1:n_sectors) {
                    if (!(i == 1 && k == 1)) {  # Skip normalization
                        sector_exports <- 0
                        sector_tariff_revenue <- 0
                        
                        for (n in 1:n_countries) {
                            sector_exports <- sector_exports + trade_shares[i, n, k] * alpha_matrix[n, k] * total_income[n]
                            sector_tariff_revenue <- sector_tariff_revenue + 
                                tariffs[i, n, k] * trade_shares[i, n, k] * alpha_matrix[n, k] * total_income[n]
                        }
                        
                        # Update sector-specific wage
                        if (labor_matrix[i, k] > 0) {
                            wages[i, k] <- (sector_exports - sector_tariff_revenue) / labor_matrix[i, k]
                        }
                    }
                }
            }
        }
        
        # Step 6: Update iceberg costs to match observed trade patterns (joint estimation)
        if (!is.null(observed_trade_shares) && iter %% 10 == 0) {
            adjustment_factor <- 0.05  # Learning rate for iceberg cost adjustment
            
            for (i in 1:n_countries) {
                for (n in 1:n_countries) {
                    for (k in 1:n_sectors) {
                        if (i != n) {  # Only adjust international trade costs
                            predicted <- trade_shares[i, n, k]
                            observed <- observed_trade_shares[i, n, k]
                            
                            if (!is.na(observed) && observed > 0 && predicted > 0) {
                                # Adjust iceberg cost based on trade share difference
                                ratio <- predicted / observed
                                if (ratio > 1.1) {
                                    # Predicted too high, increase cost
                                    iceberg_costs[i, n, k] <- iceberg_costs[i, n, k] * (1 + adjustment_factor * (ratio - 1))
                                } else if (ratio < 0.9) {
                                    # Predicted too low, decrease cost
                                    iceberg_costs[i, n, k] <- iceberg_costs[i, n, k] * (1 - adjustment_factor * (1 - ratio))
                                }
                            }
                        }
                    }
                }
            }
        }
        
        # Check convergence
        wage_diff <- max(abs(wages - wages_old))
        price_diff <- max(abs(prices - prices_old))
        iceberg_diff <- max(abs(iceberg_costs - iceberg_costs_old))
        
        if (wage_diff < tolerance && price_diff < tolerance && iceberg_diff < tolerance) {
            cat("  ✓ Converged after", iter, "iterations\n")
            cat("  - Max wage change:", round(wage_diff, 8), "\n")
            cat("  - Max price change:", round(price_diff, 8), "\n")
            cat("  - Max iceberg cost change:", round(iceberg_diff, 8), "\n")
            break
        }
        
        if (iter == max_iter) {
            warning("Maximum iterations reached without convergence")
        }
    }
    
    # Calculate final welfare measures
    real_income <- numeric(n_countries)
    real_tariff_revenue <- numeric(n_countries)
    aggregate_prices <- numeric(n_countries)
    
    for (n in 1:n_countries) {
        # Labor income
        labor_income <- 0
        for (k in 1:n_sectors) {
            labor_income <- labor_income + wages[n, k] * labor_matrix[n, k]
        }
        
        # Tariff revenue
        tariff_revenue <- 0
        for (k in 1:n_sectors) {
            for (i in 1:n_countries) {
                tariff_revenue <- tariff_revenue + tariffs[i, n, k] * trade_shares[i, n, k] * expenditures[n, k]
            }
        }
        
        # Aggregate price index: P_n = ∏p_nk^α_nk
        aggregate_price <- 1
        for (k in 1:n_sectors) {
            aggregate_price <- aggregate_price * (prices[n, k] ^ alpha_matrix[n, k])
        }
        
        real_income[n] <- labor_income / aggregate_price
        real_tariff_revenue[n] <- tariff_revenue / aggregate_price
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
        iceberg_costs = iceberg_costs,  # Include jointly estimated iceberg costs
        converged = iter < max_iter,
        iterations = iter,
        final_error = max(wage_diff, price_diff, iceberg_diff)
    ))
}



#' Calibrate CDK model baseline equilibrium using proper equilibrium conditions
#' @param parameters List containing all model parameters (alpha, beta, gamma, technology, theta, tariffs)
#' @param cleaned_data Cleaned WIOD and socioeconomic database (for dimensions and observed data)
#' @param labor_mobility Logical. TRUE for mobile labor, FALSE for immobile labor
#' @return List with three components: parameters, model_solution, outcome_variables
calibrate_model <- function(parameters, cleaned_data, labor_mobility = TRUE) {
    
    cat("Starting CDK model calibration using proper equilibrium conditions...\n")
    
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
    
    # Solve equilibrium system with joint iceberg cost estimation
    equilibrium <- solve_cdk_equilibrium(
        database = cleaned_data,
        parameters = parameters,
        labor_mobility = labor_mobility
    )
    
    # ========================================================================
    # ORGANIZE OUTPUT INTO THREE DISTINCT LISTS
    # ========================================================================
    
    # 1. PARAMETERS: Jointly estimated iceberg trade costs
    calibrated_parameters <- list(
        iceberg_costs = equilibrium$iceberg_costs  # Jointly estimated with equilibrium
    )
    
    # 2. MODEL SOLUTION: All equilibrium variables
    model_solution <- list(
        prices = equilibrium$prices,                    # Baseline sectoral prices
        costs = equilibrium$unit_costs,                 # Baseline unit costs
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
        convergence_achieved = equilibrium$converged,
        iterations = equilibrium$iterations,
        equilibrium_method = "Full system solution per body.tex"
    )
    
    cat("✓ Model calibration completed successfully\n")
    cat("  - Average domestic trade share:", round(mean(domestic_shares), 3), "\n")
    cat("  - Equilibrium convergence:", equilibrium$converged, "\n")
    cat("  - Iterations required:", equilibrium$iterations, "\n")
    
    # Return organized output
    return(list(
        parameters = calibrated_parameters,
        model_solution = model_solution, 
        outcome_variables = outcome_variables,
        summary = calibration_summary
    ))
}