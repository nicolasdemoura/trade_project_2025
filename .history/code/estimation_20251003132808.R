
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Data frame ready for CDK regression analysis
prepare_cdk_regression_data <- function(trade_flows_array, expenditure_matrix, countries, sectors) {
  
  n        # Extract iceberg costs (excluding domestic costs which are fixed at 1)
        tau_array <- array(1, dim = c(n_countries, n_countries, n_sectors))
        for (s in 1:n_sectors) {
            for (i in 1:n_countries) {
                for (j in 1:n_countries) {
                    if (i != j) {  # Only for international trade
                        # Apply tariffs to iceberg costs: tau_ij * (1 + tariff_rate)
                        base_tau <- exp(params[param_idx])
                        tau_array[i, j, s] <- base_tau * (1 + baseline_tariffs[i, j, s])
                        param_idx <- param_idx + 1
                    }
                }
            }
        }- length(countries)
  n_sectors <- length(sectors)
  
  # Calculate trade shares: π_ijs = X_ijs / Y_js
  trade_shares_array <- array(0, dim = c(n_countries, n_countries, n_sectors))
  
  for (s in 1:n_sectors) {
    for (j in 1:n_countries) {
      if (expenditure_matrix[j, s] > 0) {
        trade_shares_array[, j, s] <- trade_flows_array[, j, s] / expenditure_matrix[j, s]
      }
    }
  }
  
  # Create long format data for regression
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
          expenditure = expenditure_matrix[j, s]
        )
        
        regression_data <- rbind(regression_data, row_data)
      }
    }
  }
  
  return(regression_data)
}

#' Estimate CDK (2012) technology parameters using Section 5.1 approach
#' @param regression_data Prepared CDK regression data
#' @param theta Vector of sector-specific trade elasticities (if provided)
#' @return List with estimated technology parameters and regression results
estimate_cdk_technology_params <- function(regression_data, theta = 6.53) {
    
    # CDK reduced form regression: ln(π_ijs) = α_is + γ_js + ε_ijs
    regression_formula <- log_pi_ijs ~ factor(origin_sector) + factor(dest_sector) - 1
    
    regression_result <- fixest::feols(regression_formula, data = regression_data)
    
    # Extract fixed effects coefficients
    coef_vec <- coef(regression_result)
    coef_names <- names(coef_vec)
    
    # Separate origin-sector and destination-sector effects
    origin_sector_effects <- coef_vec[grepl("factor\\(origin_sector\\)", coef_names)]
    dest_sector_effects <- coef_vec[grepl("factor\\(dest_sector\\)", coef_names)]
    
    # Extract technology parameters from origin-sector fixed effects
    # δ_is = ln(T_is) - θ_s * ln(w_i)
    # These contain both technology and wage information
    
    n_countries <- length(unique(regression_data$origin_id))
    n_sectors <- length(unique(regression_data$sector_id))
    
    # Create technology parameter matrix (countries x sectors)
    T_composite_matrix <- matrix(NA, n_countries, n_sectors)
    
    for (effect_name in names(origin_sector_effects)) {
        # Parse "factor(origin_sector)1_1" format
        clean_name <- gsub("factor\\(origin_sector\\)", "", effect_name)
        parts <- strsplit(clean_name, "_")[[1]]
        
        if (length(parts) == 2) {
        country_id <- as.numeric(parts[1])
        sector_id <- as.numeric(parts[2])
        
        if (!is.na(country_id) && !is.na(sector_id) && 
            country_id <= n_countries && sector_id <= n_sectors) {
            T_composite_matrix[country_id, sector_id] <- exp(origin_sector_effects[[effect_name]]/theta)
          }
        }
    }

    # Complete the NA entries in T_composite_matrix using soft imputation
    if (any(is.na(T_composite_matrix))) {
      # Convert to matrix for softImpute
      T_matrix <- as.matrix(T_composite_matrix)
      
      # Apply soft imputation with reasonable rank constraint
      soft_impute_result <- softImpute::softImpute(T_matrix, rank.max = min(3, min(dim(T_matrix))), 
                            lambda = 0.1, maxit = 100)
      
      # Complete the matrix with imputed values
      T_composite_matrix <- softImpute::complete(T_matrix, soft_impute_result)
    }
    
    rownames(T_composite_matrix) <- unique(regression_data$origin_country)
    colnames(T_composite_matrix) <- unique(regression_data$sector)

    return(list(
        technology_composite = T_composite_matrix,
        origin_sector_effects = origin_sector_effects,
        dest_sector_effects = dest_sector_effects,
        regression_results = regression_result,
        theta = theta,
        fitted_values = fitted(regression_result),
        residuals = residuals(regression_result),
        n_countries = n_countries,
        n_sectors = n_sectors
    ))
}

#' Calibrate multi-sector baseline equilibrium from estimated technology parameters
#' @param T_matrix Results from estimate_cdk_technology_params
#' @param cleaned_data Cleaned WIOD data with trade flows and country/sector info
#' @param theta Trade elasticity (default from CDK estimation)
#' @param labor_mobility Logical. TRUE for mobile labor (wages equalize across sectors), FALSE for immobile labor (default: TRUE)
#' @return Calibrated baseline equilibrium with technology and trade parameters
calibrate_multisector_baseline <- function(T_matrix, cleaned_data, theta = NULL, labor_mobility = TRUE) {

    # Extract dimensions and parameters
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Create trade cost array if not provided
    tau_array <- array(1.5, dim = c(n_countries, n_countries, n_sectors))  # Default iceberg costs

    # Set domestic trade costs to 1
    for (s in 1:n_sectors) {
      for (i in 1:n_countries) {
        tau_array[i, i, s] <- 1.0
      }
    }
    
    # Extract labor allocation matrix    
    labor_matrix <- cleaned_data$labor_matrix
    
    # Extract IO matrices
    beta_matrix <- cleaned_data$beta_matrix
    gamma_matrix <- cleaned_data$gamma_matrix
    expenditure_matrix <- cleaned_data$expenditure_matrix
    
    cat("Calibrating multi-sector baseline equilibrium...\n")
    cat("Countries:", n_countries, "| Sectors:", n_sectors, "| Trade elasticity:", theta, "\n")
    cat("Labor mobility:", ifelse(labor_mobility, "Mobile (wages equalize across sectors)", "Immobile (sector-specific wages)"), "\n")
    
    # Check if we have observed trade shares for direct matching
    baseline_equilibrium <- calibrate_equilibrium_from_trade_shares(
        theta = theta,
        T_matrix = T_matrix,
        labor_matrix = labor_matrix,
        beta_matrix = beta_matrix,
        gamma_matrix = gamma_matrix,
        expenditure_matrix = expenditure_matrix,
        observed_trade_shares = cleaned_data$observed_trade_shares,
        labor_mobility = labor_mobility
      )
   
    # Calculate additional statistics
    baseline_statistics <- list(
      # Average trade shares by sector
      domestic_shares = apply(baseline_equilibrium$trade_shares, 3, function(x) mean(diag(x))),
      # GDP shares by country
      gdp_shares = baseline_equilibrium$gdp / sum(baseline_equilibrium$gdp),
      # Price level dispersion
      price_dispersion = apply(baseline_equilibrium$price_indices, 2, sd),
      # Wage dispersion across countries
      wage_dispersion = apply(baseline_equilibrium$wages, 2, sd)
    )
    
    # Store calibration parameters
    calibration_params <- list(
      theta = theta,
      T_matrix = T_matrix,
      tau_array = tau_array,
      labor_matrix = labor_matrix,
      beta_matrix = beta_matrix,
      gamma_matrix = gamma_matrix,
      expenditure_matrix = expenditure_matrix,
      labor_mobility = labor_mobility,
      countries = countries,
      sectors = sectors
    )
    
    cat("Baseline calibration completed successfully.\n")
    cat("Average domestic trade share:", round(mean(baseline_statistics$domestic_shares), 3), "\n")
    
    return(list(
      equilibrium = baseline_equilibrium,
      statistics = baseline_statistics,
      parameters = calibration_params,
      technology_estimation = tech_estimation,
      data = cleaned_data,
      # Include matrices and settings at top level for backward compatibility
      beta_matrix = beta_matrix,
      gamma_matrix = gamma_matrix,
      expenditure_matrix = expenditure_matrix,
      labor_mobility = labor_mobility
    ))
}


#' Calculate tariff revenue for each importing country
#' @param trade_shares Trade share matrix (origin x destination x sectors)
#' @param tau_array Total trade costs including tariffs (origin x destination x sectors)
#' @param tariff_rates Applied tariff rates (origin x destination x sectors)
#' @param expenditure_matrix Final expenditure matrix (countries x sectors)
#' @param wages Wage matrix (countries x sectors)
#' @param n_countries Number of countries
#' @param n_sectors Number of sectors
#' @return Matrix of tariff revenues by importing country and sector
calculate_tariff_revenue <- function(trade_shares, tau_array, tariff_rates, expenditure_matrix, wages, n_countries, n_sectors) {
    
    tariff_revenue <- matrix(0, n_countries, n_sectors)
    
    for (j in 1:n_countries) {  # Importing country
        for (s in 1:n_sectors) {  # Sector
            total_revenue <- 0
            total_expenditure <- expenditure_matrix[j, s]
            
            for (i in 1:n_countries) {  # Exporting country  
                if (i != j) {  # Only international trade generates tariff revenue
                    # Import value from country i to country j in sector s
                    import_value <- trade_shares[i, j, s] * total_expenditure
                    # Tariff revenue = tariff_rate * import_value
                    total_revenue <- total_revenue + (tariff_rates[i, j, s] * import_value)
                }
            }
            tariff_revenue[j, s] <- total_revenue
        }
    }
    
    return(tariff_revenue)
}

#' Optimize iceberg costs, wages, and prices to match observed trade shares
#' @param theta Trade elasticity parameter
#' @param T_matrix Technology matrix (countries x sectors)
#' @param labor_matrix Labor allocation matrix (countries x sectors)
#' @param beta_matrix Labor share matrix (countries x sectors)
#' @param gamma_matrix Input-output coefficients (countries x sectors x sectors)
#' @param expenditure_matrix Final expenditure matrix (countries x sectors)
#' @param observed_trade_shares Observed bilateral trade shares (origin x destination x sectors)
#' @param baseline_tariffs Baseline tariff rates (origin x destination x sectors) from cleaned_data$tariffs
#' @param labor_mobility Logical. TRUE for mobile labor (wages equalize across sectors), FALSE for immobile labor (default: FALSE)
#' @return Equilibrium object with optimized parameters to minimize trade share prediction error
calibrate_equilibrium_from_trade_shares <- function(theta, T_matrix, labor_matrix, beta_matrix, gamma_matrix, expenditure_matrix, observed_trade_shares, baseline_tariffs = NULL, labor_mobility = FALSE) {
    
    n_countries <- nrow(T_matrix)
    n_sectors <- ncol(T_matrix)
    
    # Handle baseline tariffs (if not provided, assume no tariffs)
    if (is.null(baseline_tariffs)) {
        baseline_tariffs <- array(0, dim = c(n_countries, n_countries, n_sectors))
        cat("No baseline tariffs provided - assuming zero tariffs\n")
    } else {
        cat("Using provided baseline tariffs\n")
    }
    
    cat("Optimizing parameters to match observed trade shares...\n")
    cat("Labor mobility mode:", ifelse(labor_mobility, "Mobile", "Immobile"), "\n")
    
    # Define objective function: minimize sum of squared differences between predicted and observed trade shares
    objective_function <- function(params) {
        
        # Extract parameters from optimization vector
        # Structure: [log(tau_ij), log(w_ik), log(p_nk)]
        n_tau_params <- n_countries * n_countries * n_sectors - n_sectors  # Exclude domestic costs (=1)
        n_wage_params <- if (labor_mobility) (n_countries - 1) else n_countries * n_sectors - 1
        n_price_params <- n_countries * n_sectors
        
        param_idx <- 1
        
        # Extract iceberg costs (excluding domestic costs which are fixed at 1)
        tau_array <- array(1, dim = c(n_countries, n_countries, n_sectors))
        for (s in 1:n_sectors) {
            for (i in 1:n_countries) {
                for (j in 1:n_countries) {
                    if (i != j) {  # Only non-domestic costs are optimized
                        # Apply tariffs to iceberg costs: tau_ij * (1 + tariff_rate)
                        base_tau <- exp(params[param_idx])
                        tau_array[i, j, s] <- base_tau * (1 + baseline_tariffs[i, j, s])
                        param_idx <- param_idx + 1
                    }
                }
            }
        }
        
        # Extract wages
        wages <- matrix(1, n_countries, n_sectors)
        if (labor_mobility) {
            # Mobile labor: one wage per country (excluding numeraire)
            for (i in 2:n_countries) {
                common_wage <- exp(params[param_idx])
                wages[i, ] <- common_wage
                param_idx <- param_idx + 1
            }
        } else {
            # Immobile labor: sector-specific wages (excluding numeraire)
            for (i in 1:n_countries) {
                for (k in 1:n_sectors) {
                    if (i == 1 && k == 1) {
                        next  # Skip numeraire wage
                    }
                    wages[i, k] <- exp(params[param_idx])
                    param_idx <- param_idx + 1
                }
            }
        }
        
        # Extract price indices
        price_indices <- array(1, dim = c(n_countries, n_sectors))
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                price_indices[i, k] <- exp(params[param_idx])
                param_idx <- param_idx + 1
            }
        }
        
        # Calculate unit costs
        unit_costs <- array(0, dim = c(n_countries, n_sectors))
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                # Labor component
                labor_cost <- wages[i, k]^beta_matrix[i, k]
                
                # Intermediate input component
                intermediate_cost <- 1
                for (kprime in 1:n_sectors) {
                    intermediate_cost <- intermediate_cost * (price_indices[i, kprime]^gamma_matrix[i, k, kprime])
                }
                intermediate_cost <- intermediate_cost^(1 - beta_matrix[i, k])
                
                unit_costs[i, k] <- labor_cost * intermediate_cost
            }
        }
        
        # Calculate predicted trade shares
        predicted_trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors))
        
        for (n in 1:n_countries) {
            for (i in 1:n_countries) {
                for (k in 1:n_sectors) {
                    total_cost_ink <- unit_costs[i, k] * tau_array[i, n, k]
                    numerator <- T_matrix[i, k] * (total_cost_ink)^(-theta)
                    
                    # Calculate denominator (price index consistency)
                    denominator <- 0
                    for (iprime in 1:n_countries) {
                        cost_iprime <- unit_costs[iprime, k] * tau_array[iprime, n, k]
                        denominator <- denominator + T_matrix[iprime, k] * (cost_iprime)^(-theta)
                    }
                    
                    if (denominator > 0) {
                        predicted_trade_shares[i, n, k] <- numerator / denominator
                    }
                }
            }
        }
        
        # Calculate objective: sum of squared differences (with weights)
        objective_value <- 0
        for (i in 1:n_countries) {
            for (n in 1:n_countries) {
                for (k in 1:n_sectors) {
                    if (!is.na(observed_trade_shares[i, n, k]) && observed_trade_shares[i, n, k] > 0) {
                        # Weight by trade volume importance
                        weight <- observed_trade_shares[i, n, k]
                        diff <- predicted_trade_shares[i, n, k] - observed_trade_shares[i, n, k]
                        objective_value <- objective_value + weight * diff^2
                    }
                }
            }
        }
        
        # Add penalty for price index consistency (optional constraint)
        price_penalty <- 0
        for (n in 1:n_countries) {
            for (k in 1:n_sectors) {
                sum_competitors <- 0
                for (i in 1:n_countries) {
                    total_cost_ink <- unit_costs[i, k] * tau_array[i, n, k]
                    sum_competitors <- sum_competitors + T_matrix[i, k] * (total_cost_ink)^(-theta)
                }
                theoretical_price <- sum_competitors^(-1/theta)
                price_penalty <- price_penalty + (price_indices[n, k] - theoretical_price)^2
            }
        }
        
        return(objective_value + price_penalty) 
    }
    
    # Set up initial parameters
    n_tau_params <- n_countries * n_countries * n_sectors - n_sectors  # Exclude domestic costs
    n_wage_params <- if (labor_mobility) (n_countries - 1) else (n_countries - 1) * n_sectors
    n_price_params <- n_countries * n_sectors
    
    total_params <- n_tau_params + n_wage_params + n_price_params
    
    # Initialize parameters (in log space)
    initial_params <- numeric(total_params)
    
    # Initialize iceberg costs (slightly above 1, excluding domestic)
    param_idx <- 1
    for (s in 1:n_sectors) {
        for (i in 1:n_countries) {
            for (j in 1:n_countries) {
                if (i != j) {
                    initial_params[param_idx] <- log(1.2)  # Start with 20% iceberg costs
                    param_idx <- param_idx + 1
                }
            }
        }
    }
    
    # Initialize wages (log(1) = 0 for non-numeraire countries)
    for (w in 1:n_wage_params) {
        initial_params[param_idx] <- 0  # log(1) = 0
        param_idx <- param_idx + 1
    }
    
    # Initialize prices (log(1) = 0)
    for (p in 1:n_price_params) {
        initial_params[param_idx] <- 0  # log(1) = 0
        param_idx <- param_idx + 1
    }
    
    cat("Starting optimization with", total_params, "parameters...\n")
    cat("Iceberg costs:", n_tau_params, "| Wages:", n_wage_params, "| Prices:", n_price_params, "\n")
    
    # Run optimization
    optimization_result <- optim(
        par = initial_params,
        fn = objective_function,
        method = "BFGS",
        control = list(
            maxit = 1000,
            abstol = 1e-3,
            reltol = 1e-3,
            trace = 1,  # Show optimization progress
            REPORT = 10  # Report every 10 iterations
        )
    )
    
    # Extract optimal parameters
    optimal_params <- optimization_result$par
    param_idx <- 1
    
    # Extract optimal iceberg costs (including tariffs)
    tau_array_optimal <- array(1, dim = c(n_countries, n_countries, n_sectors))
    tau_base_optimal <- array(1, dim = c(n_countries, n_countries, n_sectors)) # Base costs without tariffs
    for (s in 1:n_sectors) {
        for (i in 1:n_countries) {
            for (j in 1:n_countries) {
                if (i != j) {
                    base_tau <- exp(optimal_params[param_idx])
                    tau_base_optimal[i, j, s] <- base_tau
                    tau_array_optimal[i, j, s] <- base_tau * (1 + baseline_tariffs[i, j, s])
                    param_idx <- param_idx + 1
                }
            }
        }
    }
    
    # Extract optimal wages
    wages_optimal <- matrix(1, n_countries, n_sectors)
    if (labor_mobility) {
        for (i in 2:n_countries) {
            common_wage <- exp(optimal_params[param_idx])
            wages_optimal[i, ] <- common_wage
            param_idx <- param_idx + 1
        }
    } else {
        for (i in 2:n_countries) {
            for (k in 1:n_sectors) {
                wages_optimal[i, k] <- exp(optimal_params[param_idx])
                param_idx <- param_idx + 1
            }
        }
    }
    
    # Extract optimal prices
    price_indices_optimal <- array(1, dim = c(n_countries, n_sectors))
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            price_indices_optimal[i, k] <- exp(optimal_params[param_idx])
            param_idx <- param_idx + 1
        }
    }
    
    # Calculate final unit costs and trade shares with optimal parameters
    unit_costs_optimal <- array(0, dim = c(n_countries, n_sectors))
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            labor_cost <- wages_optimal[i, k]^beta_matrix[i, k]
            intermediate_cost <- 1
            for (kprime in 1:n_sectors) {
                intermediate_cost <- intermediate_cost * (price_indices_optimal[i, kprime]^gamma_matrix[i, k, kprime])
            }
            intermediate_cost <- intermediate_cost^(1 - beta_matrix[i, k])
            unit_costs_optimal[i, k] <- labor_cost * intermediate_cost
        }
    }
    
    # Calculate final predicted trade shares
    trade_shares_optimal <- array(0, dim = c(n_countries, n_countries, n_sectors))
    for (n in 1:n_countries) {
        for (i in 1:n_countries) {
            for (k in 1:n_sectors) {
                total_cost_ink <- unit_costs_optimal[i, k] * tau_array_optimal[i, n, k]
                numerator <- T_matrix[i, k] * (total_cost_ink)^(-theta)
                
                denominator <- 0
                for (iprime in 1:n_countries) {
                    cost_iprime <- unit_costs_optimal[iprime, k] * tau_array_optimal[iprime, n, k]
                    denominator <- denominator + T_matrix[iprime, k] * (cost_iprime)^(-theta)
                }
                
                if (denominator > 0) {
                    trade_shares_optimal[i, n, k] <- numerator / denominator
                }
            }
        }
    }
    
    # Calculate fit statistics
    total_sse <- sum((trade_shares_optimal - observed_trade_shares)^2, na.rm = TRUE)
    correlation <- cor(as.vector(trade_shares_optimal), as.vector(observed_trade_shares), use = "complete.obs")
    
    # Calculate welfare and GDP
    welfare <- numeric(n_countries)
    gdp <- numeric(n_countries)
    
    for (i in 1:n_countries) {
        gdp[i] <- sum(wages_optimal[i, ] * labor_matrix[i, ])
        
        total_labor <- sum(labor_matrix[i, ])
        if (total_labor > 0) {
            avg_wage <- sum(wages_optimal[i, ] * labor_matrix[i, ]) / total_labor
            
            price_level <- 1
            for (k in 1:n_sectors) {
                sector_share <- expenditure_matrix[i, k] / sum(expenditure_matrix[i, ])
                price_level <- price_level * (price_indices_optimal[i, k]^sector_share)
            }
            
            welfare[i] <- avg_wage / price_level
        } else {
            welfare[i] <- 1
        }
    }
    
    cat("Optimization completed.\n")
    cat("Convergence:", optimization_result$convergence == 0, "| Objective value:", round(optimization_result$value, 6), "\n")
    cat("Trade share correlation:", round(correlation, 4), "| Sum of squared errors:", round(total_sse, 6), "\n")
    cat("Average iceberg cost:", round(mean(tau_array_optimal[tau_array_optimal > 1]), 3), "\n")
    
    return(list(
        wages = wages_optimal,
        price_indices = price_indices_optimal,
        trade_shares = trade_shares_optimal,  # Optimally fitted to observed data
        unit_costs = unit_costs_optimal,
        tau_array = tau_array_optimal,  # Optimized iceberg costs
        gdp = gdp,
        welfare = welfare,
        optimization_result = optimization_result,
        fit_statistics = list(
            correlation = correlation,
            sum_squared_errors = total_sse,
            convergence = optimization_result$convergence
        )    
    ))
}
