###############################################################################
# Model Calibration Functions for CDK (2012) Model - Method of Moments Approach
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-04
###############################################################################

#' Calculate model-implied trade shares using CDK gravity equation
#' @param technology_matrix T_ik matrix (countries x sectors) 
#' @param iceberg_costs d_nik array (origin x destination x sectors)
#' @param unit_costs c_ik matrix (countries x sectors)
#' @param tariffs tau_nik array (origin x destination x sectors)  
#' @param theta Trade elasticity
#' @return Array of model-implied trade shares (origin x destination x sectors)
calculate_model_trade_shares <- function(technology_matrix, iceberg_costs, unit_costs, tariffs, theta) {
    
    n_countries <- nrow(technology_matrix)
    n_sectors <- ncol(technology_matrix)
    
    # Initialize trade shares array
    trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors))
    
    # Calculate trade shares using CDK gravity equation
    # π_nik = T_ik * (c_ik * d_nik * (1 + τ_nik))^(-θ) / Σ[T_i'k * (c_i'k * d_ni'k * (1 + τ_ni'k))^(-θ)]
    
    for (n in 1:n_countries) {      # destination
        for (k in 1:n_sectors) {    # sector
            
            # Calculate denominator (sum over all origins i')
            denominator <- 0
            for (iprime in 1:n_countries) {
                trade_cost <- unit_costs[iprime, k] * iceberg_costs[iprime, n, k] * (1 + tariffs[iprime, n, k])
                denominator <- denominator + technology_matrix[iprime, k] * (trade_cost^(-theta))
            }
            
            # Calculate numerator and trade share for each origin i
            for (i in 1:n_countries) {
                trade_cost <- unit_costs[i, k] * iceberg_costs[i, n, k] * (1 + tariffs[i, n, k])
                numerator <- technology_matrix[i, k] * (trade_cost^(-theta))
                
                if (denominator > 0) {
                    trade_shares[i, n, k] <- numerator / denominator
                } else {
                    trade_shares[i, n, k] <- 1 / n_countries  # Equal shares fallback
                }
            }
        }
    }
    
    return(trade_shares)
}

#' Calculate unit costs using CDK cost equation
#' @param wages w_ik matrix (countries x sectors)
#' @param prices p_ik matrix (countries x sectors) 
#' @param beta_matrix Labor shares β_ik (countries x sectors)
#' @param gamma_matrix Intermediate shares γ_ikk' (countries x sectors x sectors)
#' @return Matrix of unit costs c_ik (countries x sectors)
calculate_unit_costs <- function(wages, prices, beta_matrix, gamma_matrix) {
    
    n_countries <- nrow(wages)
    n_sectors <- ncol(wages)
    
    unit_costs <- matrix(0, n_countries, n_sectors)
    
    # Calculate unit costs: c_ik = w_ik^β_ik * Π[p_ik'^γ_ikk']^(1-β_ik)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            
            # Labor cost component
            labor_component <- wages[i, k]^beta_matrix[i, k]
            
            # Intermediate input price index
            intermediate_price_index <- 1
            for (kprime in 1:n_sectors) {
                if (gamma_matrix[i, k, kprime] > 0) {
                    intermediate_price_index <- intermediate_price_index * (prices[i, kprime]^gamma_matrix[i, k, kprime])
                }
            }
            
            # Combine components
            unit_costs[i, k] <- labor_component * (intermediate_price_index^(1 - beta_matrix[i, k]))
        }
    }
    
    return(unit_costs)
}

#' Method of moments objective function - minimizes distance between observed and model trade shares
#' @param params Vector of parameters to estimate (iceberg costs, wages, prices)
#' @param observed_trade_shares Observed π_nik from data (origin x destination x sectors)
#' @param technology_matrix T_ik from CDK regression (countries x sectors)
#' @param tariffs τ_nik policy data (origin x destination x sectors)  
#' @param beta_matrix Labor shares β_ik (countries x sectors)
#' @param gamma_matrix Intermediate shares γ_ikk' (countries x sectors x sectors)
#' @param theta Trade elasticity
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Scalar objective function value (sum of squared residuals)
moments_objective_function <- function(params, observed_trade_shares, technology_matrix, tariffs, 
                                     beta_matrix, gamma_matrix, theta, countries, sectors) {
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Unpack parameters from optimization vector
    n_iceberg <- n_countries * n_countries * n_sectors
    n_wages <- n_countries * n_sectors  
    n_prices <- n_countries * n_sectors
    
    # Extract iceberg costs (excluding domestic = 1 normalization)
    iceberg_vec <- params[1:n_iceberg]
    iceberg_costs <- array(iceberg_vec, dim = c(n_countries, n_countries, n_sectors))
    
    # Set domestic iceberg costs to 1 (normalization)
    for (k in 1:n_sectors) {
        for (i in 1:n_countries) {
            iceberg_costs[i, i, k] <- 1.0
        }
    }
    
    # Extract wages and prices  
    wages <- matrix(params[(n_iceberg + 1):(n_iceberg + n_wages)], n_countries, n_sectors)
    prices <- matrix(params[(n_iceberg + n_wages + 1):(n_iceberg + n_wages + n_prices)], n_countries, n_sectors)
    
    # Wage normalization: first country, first sector = 1
    wages[1, 1] <- 1.0
    
    # Calculate unit costs
    unit_costs <- calculate_unit_costs(wages, prices, beta_matrix, gamma_matrix)
    
    # Calculate model-implied trade shares
    model_trade_shares <- calculate_model_trade_shares(technology_matrix, iceberg_costs, unit_costs, tariffs, theta)
    
    # Calculate squared distance between observed and model trade shares
    residuals <- (observed_trade_shares - model_trade_shares)^2
    
    # Only consider non-missing observations  
    valid_obs <- !is.na(observed_trade_shares) & !is.na(model_trade_shares) & 
                 observed_trade_shares > 0 & model_trade_shares > 0
    
    objective_value <- sum(residuals[valid_obs])
    
    # Add penalty for extreme parameter values to ensure stability
    penalty <- 0
    if (any(iceberg_costs < 0.5, na.rm = TRUE) | any(iceberg_costs > 10, na.rm = TRUE)) {
        penalty <- penalty + 1000
    }
    if (any(wages < 0.1, na.rm = TRUE) | any(wages > 10, na.rm = TRUE)) {
        penalty <- penalty + 1000
    }
    if (any(prices < 0.1, na.rm = TRUE) | any(prices > 10, na.rm = TRUE)) {
        penalty <- penalty + 1000
    }
    
    return(objective_value + penalty)
}

#' Estimate CDK model using method of moments approach
#' @param database Processed database containing observed trade data
#' @param parameters List containing structural parameters (alpha, beta, gamma, technology, theta, tariffs)
#' @param labor_mobility Logical. TRUE for mobile labor, FALSE for immobile labor
#' @return Estimated model with iceberg costs that match observed trade shares
estimate_cdk_method_of_moments <- function(database, parameters, labor_mobility = TRUE) {
    
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