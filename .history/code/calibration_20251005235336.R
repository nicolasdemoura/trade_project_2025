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

#' Method of moments objective function with strict trade balance and decomposed iceberg costs
#' @param params Vector of parameters to estimate (decomposed iceberg costs, wages, prices)
#' @param observed_trade_shares Observed π_nik from data (origin x destination x sectors)
#' @param technology_matrix T_ik from CDK regression (countries x sectors)
#' @param tariffs τ_nik policy data (origin x destination x sectors)  
#' @param beta_matrix Labor shares β_ik (countries x sectors)
#' @param gamma_matrix Intermediate shares γ_ikk' (countries x sectors x sectors)
#' @param alpha_matrix Expenditure shares α_nk (countries x sectors)
#' @param labor_matrix Labor allocation L_nk (countries x sectors)
#' @param theta Trade elasticity
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @param labor_mobility Logical. TRUE for mobile labor, FALSE for immobile labor
#' @return Scalar objective function value (sum of squared residuals + trade balance violations)
moments_objective_function <- function(params, observed_trade_shares, technology_matrix, tariffs, 
                                     beta_matrix, gamma_matrix, alpha_matrix, labor_matrix,
                                     theta, countries, sectors, labor_mobility = TRUE) {
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Unpack parameters from optimization vector with decomposed iceberg costs
    # d_nik = d_ni * d_ik^importer * d_jk^exporter
    n_bilateral <- n_countries * (n_countries - 1)  # Bilateral components d_ni (exclude domestic)
    n_importer <- n_countries * n_sectors - 1       # Importer effects d_ik^importer (exclude normalization)
    n_exporter <- n_countries * n_sectors - 1       # Exporter effects d_jk^exporter (exclude normalization)
    n_iceberg_total <- n_bilateral + n_importer + n_exporter
    
    # Labor mobility affects wage structure
    if (labor_mobility) {
        n_wages <- n_countries - 1  # Mobile labor: one wage per country (exclude normalization)
    } else {
        n_wages <- n_countries * n_sectors - 1  # Immobile labor: wage per country-sector
    }
    
    n_prices <- n_countries * n_sectors
    
    # Extract decomposed iceberg cost components
    param_idx <- 1
    
    # 1. Bilateral components d_ni (country-to-country, exclude domestic d_ii = 1)
    bilateral_costs <- matrix(1.0, n_countries, n_countries)  # Initialize with domestic = 1
    for (i in 1:n_countries) {
        for (j in 1:n_countries) {
            if (i != j) {
                bilateral_costs[i, j] <- exp(params[param_idx])
                param_idx <- param_idx + 1
            }
        }
    }
    
    # 2. Importer effects d_ik^importer (normalize first country-sector = 1)
    importer_effects <- matrix(1.0, n_countries, n_sectors)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            if (!(i == 1 && k == 1)) {  # Skip normalization
                importer_effects[i, k] <- exp(params[param_idx])
                param_idx <- param_idx + 1
            }
        }
    }
    
    # 3. Exporter effects d_jk^exporter (normalize first country-sector = 1) 
    exporter_effects <- matrix(1.0, n_countries, n_sectors)
    for (j in 1:n_countries) {
        for (k in 1:n_sectors) {
            if (!(j == 1 && k == 1)) {  # Skip normalization
                exporter_effects[j, k] <- exp(params[param_idx])
                param_idx <- param_idx + 1
            }
        }
    }
    
    # Construct full iceberg cost matrix: d_nik = d_ni * d_ik^importer * d_jk^exporter
    iceberg_costs <- array(0, dim = c(n_countries, n_countries, n_sectors))
    for (i in 1:n_countries) {
        for (n in 1:n_countries) {
            for (k in 1:n_sectors) {
                iceberg_costs[i, n, k] <- bilateral_costs[i, n] * 
                                          importer_effects[n, k] * 
                                          exporter_effects[i, k]
            }
        }
    }
    
    # Extract wages based on labor mobility assumption
    wages <- matrix(1.0, n_countries, n_sectors)
    
    if (labor_mobility) {
        # Mobile labor: one wage per country w_i (same across sectors)
        wages[1, 1] <- 1.0  # Normalization: first country wage = 1
        country_wages <- numeric(n_countries)
        country_wages[1] <- 1.0
        
        for (i in 2:n_countries) {  # Skip normalized country
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
        # Immobile labor: sector-specific wages w_ik
        wages[1, 1] <- 1.0  # Normalization: first country-sector wage = 1
        
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                if (!(i == 1 && k == 1)) {  # Skip normalized wage
                    wages[i, k] <- exp(params[param_idx])
                    param_idx <- param_idx + 1
                }
            }
        }
    }
    
    prices <- matrix(0, n_countries, n_sectors)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            prices[i, k] <- exp(params[param_idx])
            param_idx <- param_idx + 1
        }
    }
    
    # Calculate unit costs
    unit_costs <- calculate_unit_costs(wages, prices, beta_matrix, gamma_matrix)
    
    # Calculate model-implied trade shares
    model_trade_shares <- calculate_model_trade_shares(technology_matrix, iceberg_costs, unit_costs, tariffs, theta)
    
    # Solve income-expenditure system as linear system
    # Y_n = ∑w_nk*L_nk + ∑∑τ_nik*π_nik*α_nk*Y_n
    # Rearranging: Y_n - ∑∑τ_nik*π_nik*α_nk*Y_n = ∑w_nk*L_nk
    # In matrix form: (I - T)*Y = L, where T_nn' = ∑∑τ_nik*π_nik*α_nk if n'=n, 0 otherwise
    
    # Calculate labor income vector L
    labor_income <- numeric(n_countries)
    for (n in 1:n_countries) {
        labor_income[n] <- sum(wages[n, ] * labor_matrix[n, ])
    }
    
    # Calculate tariff revenue coefficient matrix T
    # Only diagonal elements are set: T[n,n] = ∑∑τ_nik*π_nik*α_nk (tariff revenue as fraction of own income for country n).
    # Off-diagonal elements remain zero, as tariff revenue is only collected by the importing country.
    tariff_matrix <- matrix(0, n_countries, n_countries)  # Initialize as zero matrix
    
    for (n in 1:n_countries) {
        tariff_coeff <- 0
        for (k in 1:n_sectors) {
            for (i in 1:n_countries) {
                tariff_coeff <- tariff_coeff + tariffs[i, n, k] * model_trade_shares[i, n, k] * alpha_matrix[n, k]
            }
        }
        tariff_matrix[n, n] <- tariff_coeff
    }
    
    # Solve linear system: (I - T)*Y = L
    # Y = (I - T)^(-1) * L
    income_system_matrix <- diag(n_countries) - tariff_matrix
    total_income <- solve(income_system_matrix, labor_income)
    
    # Calculate expenditures: X_nk = α_nk * Y_n
    expenditures <- matrix(0, n_countries, n_sectors)
    for (n in 1:n_countries) {
        for (k in 1:n_sectors) {
            expenditures[n, k] <- alpha_matrix[n, k] * total_income[n]
        }
    }
    
    # Calculate trade balance violations
    trade_balance_violations <- 0
    
    if (labor_mobility) {
        # Mobile labor: ∑∑π_ink*X_nk = ∑w_ik*L_ik (export revenues = labor income)
        # Note: Tariff revenue is already included in total_income via linear system
        for (i in 1:n_countries) {
            lhs <- 0  # Left-hand side: export revenues
            for (k in 1:n_sectors) {
                for (n in 1:n_countries) {
                    lhs <- lhs + model_trade_shares[i, n, k] * expenditures[n, k]
                }
            }
            
            rhs <- 0  # Right-hand side: labor income only
            for (k in 1:n_sectors) {
                rhs <- rhs + wages[i, k] * labor_matrix[i, k]
            }
            
            trade_balance_violations <- trade_balance_violations + (lhs - rhs)^2
        }
    } else {
        # Immobile labor: ∑π_ink*X_nk = w_ik*L_ik (sector export revenues = sector labor income)
        # Note: Tariff revenue is already included in total_income via linear system
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                lhs <- 0  # Left-hand side: sector export revenues
                for (n in 1:n_countries) {
                    lhs <- lhs + model_trade_shares[i, n, k] * expenditures[n, k]
                }
                
                rhs <- wages[i, k] * labor_matrix[i, k]  # Sector labor income only
                
                trade_balance_violations <- trade_balance_violations + (lhs - rhs)^2
            }
        }
    }
    
    # Calculate price consistency violations
    # Price consistency: p_nk = [∑T_ik(c_ik*d_nik*(1+τ_nik))^(-θ)]^(-1/θ)
    price_consistency_violations <- 0
    
    for (n in 1:n_countries) {
        for (k in 1:n_sectors) {
            price_index_sum <- 0
            for (i in 1:n_countries) {
                trade_cost <- unit_costs[i, k] * iceberg_costs[i, n, k] * (1 + tariffs[i, n, k])
                price_index_sum <- price_index_sum + technology_matrix[i, k] * (trade_cost^(-theta))
            }
            price_index_sum <- price_index_sum^(-1/theta)
            price_consistency_violations <- price_consistency_violations + (prices[n, k] - price_index_sum)^2
            
        }
    }
    
    # Calculate squared distance between observed and model trade shares
    residuals <- (observed_trade_shares - model_trade_shares)^2
    
    # Only consider non-missing observations  
    valid_obs <- !is.na(observed_trade_shares) & !is.na(model_trade_shares) & 
                 observed_trade_shares > 0 & model_trade_shares > 0
    
    trade_share_objective <- sum(residuals[valid_obs])
    
    # Combine all objectives: trade share matching + trade balance + price consistency
    objective_value <- trade_share_objective + trade_balance_violations * 1e-9 + price_consistency_violations
    
    return(objective_value)
}

#' Estimate CDK model using method of moments approach
#' @param database Processed database containing observed trade data
#' @param parameters List containing structural parameters (alpha, beta, gamma, technology, theta, tariffs)
#' @param labor_mobility Logical. TRUE for mobile labor, FALSE for immobile labor
#' @return Estimated model with iceberg costs that match observed trade shares
solve_cdk_equilibrium <- function(database, parameters, labor_mobility = TRUE) {
    
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
    
    cat("Estimating CDK model using method of moments approach...\n")
    cat("  - Countries:", n_countries, "| Sectors:", n_sectors, "\n")
    cat("  - Labor mobility:", ifelse(labor_mobility, "Mobile", "Immobile"), "\n")
    cat("  - Objective: Match observed trade share moments\n")
    
    # Check if observed trade shares are available
    if (is.null(observed_trade_shares)) {
        stop("Method of moments requires observed trade shares for moment matching")
    }
    
    # Initialize parameter vectors for optimization with decomposed iceberg costs
    # Parameters: bilateral costs, importer effects, exporter effects, wages, prices
    n_bilateral <- n_countries * (n_countries - 1)  # Bilateral d_ni (exclude domestic)
    n_importer <- n_countries * n_sectors - 1       # Importer effects (exclude normalization)
    n_exporter <- n_countries * n_sectors - 1       # Exporter effects (exclude normalization)
    n_iceberg_total <- n_bilateral + n_importer + n_exporter
    
    # Labor mobility affects wage structure
    if (labor_mobility) {
        n_wages <- n_countries - 1  # Mobile labor: one wage per country
    } else {
        n_wages <- n_countries * n_sectors - 1  # Immobile labor: wage per country-sector
    }
    
    n_prices <- n_countries * n_sectors
    n_params <- n_iceberg_total + n_wages + n_prices
    
    cat("  - Parameters to estimate:", n_params, "\n")
    cat("    * Bilateral costs:", n_bilateral, "\n")
    cat("    * Importer effects:", n_importer, "\n") 
    cat("    * Exporter effects:", n_exporter, "\n")
    cat("    * Wages:", n_wages, "\n")
    cat("    * Prices:", n_prices, "\n")
    
    # Initial parameter values
    initial_params <- numeric(n_params)
    param_idx <- 1
    
    # 1. Initialize bilateral costs d_ni (log scale for exp() transformation, exp(0.18) ≈ 1.2)
    for (i in 1:n_countries) {
        for (j in 1:n_countries) {
            if (i != j) {
                initial_params[param_idx] <- 0.18  # exp(0.18) ≈ 1.2
                param_idx <- param_idx + 1
            }
        }
    }
    
    # 2. Initialize importer effects d_ik^importer (start at 1.0, exclude normalization)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            if (!(i == 1 && k == 1)) {
                initial_params[param_idx] <- 0.0
                param_idx <- param_idx + 1
            }
        }
    }
    
    # 3. Initialize exporter effects d_jk^exporter (start at 1.0, exclude normalization)
    for (j in 1:n_countries) {
        for (k in 1:n_sectors) {
            if (!(j == 1 && k == 1)) {
                initial_params[param_idx] <- 0.0
                param_idx <- param_idx + 1
            }
        }
    }
    
    # 4. Initialize wages based on labor mobility
    if (labor_mobility) {
        # Mobile labor: one wage per country (log scale, exclude normalization)
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
    
    # Wrapper function for optimization
    objective_wrapper <- function(params) {
        obj_value <- moments_objective_function(
            params = params,
            observed_trade_shares = observed_trade_shares,
            technology_matrix = technology_matrix,
            tariffs = tariffs,
            beta_matrix = beta_matrix,
            gamma_matrix = gamma_matrix,
            alpha_matrix = alpha_matrix,
            labor_matrix = labor_matrix,
            theta = theta,
            countries = countries,
            sectors = sectors,
            labor_mobility = labor_mobility
        )
        return(obj_value)
    }
    
    # Perform method of moments estimation
    cat("  - Starting nonlinear optimization to match trade share moments...\n")
    
    optimization_result <- optim(
        par = initial_params,
        fn = objective_wrapper,
        method = "BFGS",  # Quasi-Newton method
        control = list(
            maxit = 2000,    # Increased for price consistency
            reltol = 1e-2,   # Tighter convergence with price consistency
            trace = 1,       # Print progress
            REPORT = 10      # Report every 10 iterations
        )
    )
    
    cat("  ✓ Optimization completed\n")
    cat("  - Convergence:", optimization_result$convergence == 0, "\n")
    cat("  - Objective value:", round(optimization_result$value, 6), "\n")
    cat("  - Function evaluations:", optimization_result$counts[1], "\n")
    
    # Extract final parameter estimates
    final_params <- optimization_result$par
    
    # Unpack final parameters with decomposed iceberg costs
    final_params <- optimization_result$par
    param_idx <- 1
    
    # 1. Extract bilateral costs d_ni
    bilateral_costs <- matrix(1.0, n_countries, n_countries)  # Domestic = 1
    for (i in 1:n_countries) {
        for (j in 1:n_countries) {
            if (i != j) {
                bilateral_costs[i, j] <- exp(final_params[param_idx])
                param_idx <- param_idx + 1
            }
        }
    }
    
    # 2. Extract importer effects d_ik^importer
    importer_effects <- matrix(1.0, n_countries, n_sectors)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            if (!(i == 1 && k == 1)) {
                importer_effects[i, k] <- exp(final_params[param_idx])
                param_idx <- param_idx + 1
            }
        }
    }
    
    # 3. Extract exporter effects d_jk^exporter
    exporter_effects <- matrix(1.0, n_countries, n_sectors)
    for (j in 1:n_countries) {
        for (k in 1:n_sectors) {
            if (!(j == 1 && k == 1)) {
                exporter_effects[j, k] <- exp(final_params[param_idx])
                param_idx <- param_idx + 1
            }
        }
    }
    
    # Construct full iceberg costs: d_nik = d_ni * d_ik^importer * d_jk^exporter
    iceberg_costs <- array(0, dim = c(n_countries, n_countries, n_sectors))
    for (i in 1:n_countries) {
        for (n in 1:n_countries) {
            for (k in 1:n_sectors) {
                iceberg_costs[i, n, k] <- bilateral_costs[i, n] * 
                                          importer_effects[n, k] * 
                                          exporter_effects[i, k]
            }
        }
    }
    
    # 4. Extract wages based on labor mobility
    wages <- matrix(1.0, n_countries, n_sectors)
    
    if (labor_mobility) {
        # Mobile labor: one wage per country
        wages[1, 1] <- 1.0  # Normalization
        country_wages <- numeric(n_countries)
        country_wages[1] <- 1.0
        
        for (i in 2:n_countries) {
            country_wages[i] <- exp(final_params[param_idx])
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
                    wages[i, k] <- exp(final_params[param_idx])
                    param_idx <- param_idx + 1
                }
            }
        }
    }
    
    prices <- matrix(0, n_countries, n_sectors)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            prices[i, k] <- exp(final_params[param_idx])
            param_idx <- param_idx + 1
        }
    }
    
    # Calculate final unit costs using estimated wages and prices
    unit_costs <- calculate_unit_costs(wages, prices, beta_matrix, gamma_matrix)
    
    # Calculate final model-implied trade shares using estimated parameters
    trade_shares <- calculate_model_trade_shares(technology_matrix, iceberg_costs, unit_costs, tariffs, theta)
    
    # Calculate expenditures and total income based on estimated equilibrium
    total_income <- numeric(n_countries)
    expenditures <- matrix(0, n_countries, n_sectors)
    
    # Calculate total income using direct linear system solution
    # Y_n = ∑w_nk*L_nk + ∑∑τ_nik*π_nik*α_nk*Y_n
    # Rearranging: (I - T)*Y = L
    
    # Calculate labor income vector L
    labor_income <- numeric(n_countries)
    for (n in 1:n_countries) {
        labor_income[n] <- sum(wages[n, ] * labor_matrix[n, ])
    }
    
    # Calculate tariff revenue coefficient matrix T
    tariff_matrix <- matrix(0, n_countries, n_countries)  # Initialize as zero matrix
    
    for (n in 1:n_countries) {
        tariff_coeff <- 0
        for (k in 1:n_sectors) {
            for (i in 1:n_countries) {
                tariff_coeff <- tariff_coeff + tariffs[i, n, k] * trade_shares[i, n, k] * alpha_matrix[n, k]
            }
        }
        tariff_matrix[n, n] <- tariff_coeff
    }
    
    # Solve linear system: (I - T)*Y = L
    income_system_matrix <- diag(n_countries) - tariff_matrix
    total_income <- solve(income_system_matrix, labor_income)
    
    # Calculate expenditures: X_nk = α_nk * Y_n
    for (n in 1:n_countries) {
        for (k in 1:n_sectors) {
            expenditures[n, k] <- alpha_matrix[n, k] * total_income[n]
        }
    }
    
    # Calculate final welfare measures
    real_income <- numeric(n_countries)
    real_tariff_revenue <- numeric(n_countries)
    aggregate_prices <- numeric(n_countries)
    
    for (n in 1:n_countries) {
        # Labor income for country n
        country_labor_income <- sum(wages[n, ] * labor_matrix[n, ])
        
        # Tariff revenue for country n
        country_tariff_revenue <- 0
        for (k in 1:n_sectors) {
            for (i in 1:n_countries) {
                country_tariff_revenue <- country_tariff_revenue + tariffs[i, n, k] * trade_shares[i, n, k] * expenditures[n, k]
            }
        }
        
        # Aggregate price index: P_n = ∏p_nk^α_nk
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
    dimnames(iceberg_costs) <- list(countries, countries, sectors)
    names(total_income) <- countries
    names(real_income) <- countries
    names(real_tariff_revenue) <- countries
    names(aggregate_prices) <- countries
    
    # Calculate fit statistics for the method of moments estimation
    valid_obs <- !is.na(observed_trade_shares) & !is.na(trade_shares) & 
                 observed_trade_shares > 0 & trade_shares > 0
    
    fit_residuals <- (observed_trade_shares - trade_shares)[valid_obs]
    r_squared <- 1 - var(fit_residuals) / var(observed_trade_shares[valid_obs])
    
    cat("  ✓ Method of moments estimation completed\n")
    cat("  - Valid observations:", sum(valid_obs), "\n")
    cat("  - R-squared (trade shares):", round(r_squared, 4), "\n")
    cat("  - RMSE (trade shares):", round(sqrt(mean(fit_residuals^2)), 6), "\n")
    
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
        iceberg_costs = iceberg_costs,  # Full decomposed iceberg costs
        bilateral_costs = bilateral_costs,     # d_ni components
        importer_effects = importer_effects,   # d_ik^importer components  
        exporter_effects = exporter_effects,   # d_jk^exporter components
        converged = optimization_result$convergence == 0,
        iterations = optimization_result$counts[1],
        final_error = optimization_result$value,
        labor_mobility = labor_mobility  # Record mobility assumption used
    ))
}


#' Calibrate CDK model baseline equilibrium using method of moments approach
#' @param parameters List containing all model parameters (alpha, beta, gamma, technology, theta, tariffs)
#' @param database Cleaned WIOD and socioeconomic database (for dimensions and observed data)
#' @param labor_mobility Logical. TRUE for mobile labor, FALSE for immobile labor
#' @return List with three components: parameters, model_solution, outcome_variables
calibrate_model <- function(database, parameters, labor_mobility = TRUE) {
    
    cat("Starting CDK model calibration using method of moments approach...\n")
    
    # Extract dimensions
    countries <- database$countries
    sectors <- database$sectors
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
    labor_matrix <- database$labor_matrix
    
    # Solve equilibrium system using method of moments
    equilibrium <- solve_cdk_equilibrium(
        database = database,
        parameters = parameters,
        labor_mobility = labor_mobility
    )
    
    # ========================================================================
    # ORGANIZE OUTPUT INTO THREE DISTINCT LISTS
    # ========================================================================
    
    # 1. PARAMETERS: Estimated decomposed iceberg trade costs
    calibrated_parameters <- list(
        iceberg_costs = equilibrium$iceberg_costs,         # Full d_nik matrix
        bilateral_costs = equilibrium$bilateral_costs,     # d_ni components
        importer_effects = equilibrium$importer_effects,   # d_ik^importer
        exporter_effects = equilibrium$exporter_effects    # d_jk^exporter
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
        equilibrium_method = "Method of moments estimation"
    )
    
    cat("✓ Model calibration completed successfully\n")
    cat("  - Average domestic trade share:", round(mean(domestic_shares), 3), "\n")
    cat("  - Equilibrium convergence:", equilibrium$converged, "\n")
    cat("  - Function evaluations:", equilibrium$iterations, "\n")
    
    # Return organized output
    return(list(
        parameters = calibrated_parameters,
        model_solution = model_solution, 
        outcome_variables = outcome_variables,
        summary = calibration_summary
    ))
}