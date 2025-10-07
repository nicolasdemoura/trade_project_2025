#' Create input-output matrices from cleaned WIOD data
#' @param cleaned_data Cleaned WIOD data
#' @return List with beta_matrix, gamma_matrix, and expenditure_matrix
create_io_matrices <- function(cleaned_data) {
    
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Initialize matrices
    beta_matrix <- array(0.7, dim = c(n_countries, n_sectors))  # Labor shares (default 0.7)
    gamma_matrix <- array(0, dim = c(n_countries, n_sectors, n_sectors))  # IO coefficients
    expenditure_matrix <- array(0, dim = c(n_countries, n_sectors))  # Final expenditure
    
    # Set input-output coefficients (simplified structure)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            # Each sector uses inputs from all sectors
            for (kprime in 1:n_sectors) {
                if (k == kprime) {
                    gamma_matrix[i, k, kprime] <- 0.2  # Own-sector usage
                } else {
                    gamma_matrix[i, k, kprime] <- (1 - beta_matrix[i, k] - 0.2) / (n_sectors - 1)  # Other sectors
                }
            }
        }
    }
    
    # Set expenditure matrix from absorption data or GDP
    if (!is.null(cleaned_data$gdp_vector)) {
        for (i in 1:n_countries) {
            total_gdp <- cleaned_data$gdp_vector[i]
            for (k in 1:n_sectors) {
                # Equal expenditure shares across sectors (can be improved with data)
                expenditure_matrix[i, k] <- total_gdp / n_sectors
            }
        }
    } else {
        # Use absorption matrix if available
        if (!is.null(cleaned_data$absorption_matrix)) {
            expenditure_matrix <- cleaned_data$absorption_matrix
        } else {
            expenditure_matrix[] <- 1  # Default
        }
    }
    
    return(list(
        beta_matrix = beta_matrix,
        gamma_matrix = gamma_matrix,
        expenditure_matrix = expenditure_matrix
    ))
}

#' Create input-output matrices from cleaned WIOD data
#' @param cleaned_data Cleaned WIOD data
#' @return List with beta_matrix, gamma_matrix, and expenditure_matrix
create_io_matrices <- function(cleaned_data) {
    
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Initialize matrices
    beta_matrix <- array(0.7, dim = c(n_countries, n_sectors))  # Labor shares (default 0.7)
    gamma_matrix <- array(0, dim = c(n_countries, n_sectors, n_sectors))  # IO coefficients
    expenditure_matrix <- array(0, dim = c(n_countries, n_sectors))  # Final expenditure
    
    # Set input-output coefficients (simplified structure)
    for (i in 1:n_countries) {
        for (k in 1:n_sectors) {
            # Each sector uses inputs from all sectors
            for (kprime in 1:n_sectors) {
                if (k == kprime) {
                    gamma_matrix[i, k, kprime] <- 0.2  # Own-sector usage
                } else {
                    gamma_matrix[i, k, kprime] <- (1 - beta_matrix[i, k] - 0.2) / (n_sectors - 1)  # Other sectors
                }
            }
        }
    }
    
    # Set expenditure matrix from absorption data or GDP
    if (!is.null(cleaned_data$gdp_vector)) {
        for (i in 1:n_countries) {
            total_gdp <- cleaned_data$gdp_vector[i]
            for (k in 1:n_sectors) {
                # Equal expenditure shares across sectors (can be improved with data)
                expenditure_matrix[i, k] <- total_gdp / n_sectors
            }
        }
    } else {
        # Use absorption matrix if available
        if (!is.null(cleaned_data$absorption_matrix)) {
            expenditure_matrix <- cleaned_data$absorption_matrix
        } else {
            expenditure_matrix[] <- 1  # Default
        }
    }
    
    return(list(
        beta_matrix = beta_matrix,
        gamma_matrix = gamma_matrix,
        expenditure_matrix = expenditure_matrix
    ))
}

#' Prepare data for CDK (2012) regression estimation
#' @param trade_flows_array Bilateral trade flows (origin x destination x sectors)
#' @param absorption_matrix Total sectoral absorption by country (countries x sectors)
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Data frame ready for CDK regression analysis
prepare_cdk_regression_data <- function(trade_flows_array, absorption_matrix, countries, sectors) {
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Calculate trade shares: π_ijs = X_ijs / Y_js
  trade_shares_array <- array(0, dim = c(n_countries, n_countries, n_sectors))
  
  for (s in 1:n_sectors) {
    for (j in 1:n_countries) {
      if (absorption_matrix[j, s] > 0) {
        trade_shares_array[, j, s] <- trade_flows_array[, j, s] / absorption_matrix[j, s]
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
          absorption = absorption_matrix[j, s]
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
      soft_impute_result <- softImpute::softImpute(T_matrix, rank.max = min(5, min(dim(T_matrix))), 
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
#' @param cdk_estimation_results Results from estimate_cdk_technology_params
#' @param cleaned_data Cleaned WIOD data with trade flows and country/sector info
#' @param io_matrices Input-output matrices from create_io_matrices
#' @param tau_array Trade cost array (origin x destination x sectors), if NULL uses iceberg costs
#' @param theta Trade elasticity (default from CDK estimation)
#' @return Calibrated baseline equilibrium with technology and trade parameters
calibrate_multisector_baseline <- function(cdk_estimation_results, cleaned_data, io_matrices, tau_array = NULL, theta = NULL) {
    
    # Extract dimensions and parameters
    n_countries <- cdk_estimation_results$n_countries
    n_sectors <- cdk_estimation_results$n_sectors
    countries <- cleaned_data$countries
    sectors <- cleaned_data$sectors
    
    # Use estimated theta or provided theta
    if (is.null(theta)) {
      theta <- cdk_estimation_results$theta
    }
    
    # Extract technology matrix
    T_matrix <- cdk_estimation_results$technology_composite
    
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
    beta_matrix <- io_matrices$beta_matrix
    gamma_matrix <- io_matrices$gamma_matrix
    expenditure_matrix <- io_matrices$expenditure_matrix
    
    cat("Calibrating multi-sector baseline equilibrium...\n")
    cat("Countries:", n_countries, "| Sectors:", n_sectors, "| Trade elasticity:", theta, "\n")
    
    # Solve baseline equilibrium
    baseline_equilibrium <- solve_equilibrium_cdk_multi_sector(
      theta = theta,
      T_matrix = T_matrix,
      tau_array = tau_array,
      labor_matrix = labor_matrix,
      beta_matrix = beta_matrix,
      gamma_matrix = gamma_matrix,
      expenditure_matrix = expenditure_matrix,
      max_iter = 1000,
      tol = 1e-8
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
      countries = countries,
      sectors = sectors
    )
    
    cat("Baseline calibration completed successfully.\n")
    cat("Average domestic trade share:", round(mean(baseline_statistics$domestic_shares), 3), "\n")
    
    return(list(
      equilibrium = baseline_equilibrium,
      statistics = baseline_statistics,
      parameters = calibration_params,
      technology_estimation = cdk_estimation_results,
      data = cleaned_data
    ))
}



#' Solve multi-sector input-output equilibrium with endogenous wages and prices
#' @param theta Trade elasticity parameter
#' @param T_matrix Technology matrix (countries x sectors)
#' @param tau_array Trade cost array (origin x destination x sectors, domestic costs = 1)
#' @param labor_matrix Labor allocation matrix (countries x sectors)
#' @param beta_matrix Labor share matrix (countries x sectors)
#' @param gamma_matrix Input-output coefficients (countries x sectors x sectors)
#' @param expenditure_matrix Final expenditure matrix (countries x sectors)
#' @param max_iter Maximum iterations for convergence
#' @param tol Convergence tolerance
#' @return Equilibrium object with wages, prices, trade shares
solve_equilibrium_cdk_multi_sector <- function(theta, T_matrix, tau_array, labor_matrix, beta_matrix, gamma_matrix, expenditure_matrix, max_iter = 1000, tol = 1e-8) {
    
    n_countries <- nrow(T_matrix)
    n_sectors <- ncol(T_matrix)
    
    # Ensure domestic trade costs are exactly 1
    for (s in 1:n_sectors) {
        for (i in 1:n_countries) {
            tau_array[i, i, s] <- 1.0
        }
    }
    
    # Initialize wages and prices
    wages <- matrix(1, n_countries, n_sectors)  # wages[i,k] = w_ik
    wages[1, ] <- 1  # Normalize wages in first country
    
    price_indices <- array(1, dim = c(n_countries, n_sectors))  # prices[n,k] = p_nk
    
    cat("Starting multi-sector equilibrium solver...\n")
    
    for (iter in 1:max_iter) {
        wages_old <- wages
        price_indices_old <- price_indices
        
        # Step 1: Calculate unit costs c_ik using current wages and prices
        # c_ik = (w_ik)^β_ik * (∏_{k'} p_ik'^γ_{ik'})^{1-β_ik}
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
        
        # Step 2: Calculate new price indices
        # p_nk = [∑_i T_ik (c_ik * d_nik)^{-θ}]^{-1/θ}
        for (n in 1:n_countries) {
            for (k in 1:n_sectors) {
                sum_competitors <- 0
                for (i in 1:n_countries) {
                    total_cost_ink <- unit_costs[i, k] * tau_array[i, n, k]
                    sum_competitors <- sum_competitors + T_matrix[i, k] * (total_cost_ink)^(-theta)
                }
                price_indices[n, k] <- sum_competitors^(-1/theta)
            }
        }
        
        # Step 3: Calculate trade shares
        # π_nik = T_ik (c_ik * d_nik)^{-θ} / ∑_{i'} T_{i'k} (c_{i'k} * d_{ni'k})^{-θ}
        trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors))
        
        for (n in 1:n_countries) {
            for (i in 1:n_countries) {
                for (k in 1:n_sectors) {
                    total_cost_ink <- unit_costs[i, k] * tau_array[i, n, k]
                    numerator <- T_matrix[i, k] * (total_cost_ink)^(-theta)
                    denominator <- price_indices[n, k]^(-theta)
                    trade_shares[i, n, k] <- numerator / denominator
                }
            }
        }
        
        # Step 4: Update wages using labor income clearing condition
        # ∑_k w_ik L_ik = ∑_k ∑_n π_nik X_nk
        for (i in 1:n_countries) {
            if (i > 1) {  # Skip first country (numeraire)
                for (k in 1:n_sectors) {
                    # Labor income from sector k in country i
                    labor_income <- wages[i, k] * labor_matrix[i, k]
                    
                    # Revenue from selling to all destinations
                    total_revenue <- 0
                    for (n in 1:n_countries) {
                        total_revenue <- total_revenue + trade_shares[i, n, k] * expenditure_matrix[n, k]
                    }
                    
                    # Update wage to satisfy labor income = β_ik * total_revenue
                    if (labor_matrix[i, k] > 0) {
                        wages[i, k] <- (beta_matrix[i, k] * total_revenue) / labor_matrix[i, k]
                    }
                }
            }
        }
        
        # Check convergence
        wage_change <- max(abs(wages - wages_old) / (wages_old + 1e-10))
        price_change <- max(abs(price_indices - price_indices_old) / (price_indices_old + 1e-10))
        
        if (wage_change < tol && price_change < tol) {
            cat("Converged after", iter, "iterations\n")
            break
        }
        
        if (iter %% 100 == 0) {
            cat("Iteration", iter, ": wage change =", wage_change, ", price change =", price_change, "\n")
        }
    }
    
    if (iter == max_iter) {
        cat("Warning: Maximum iterations reached without convergence\n")
    }
    
    # Calculate welfare (real wages)
    welfare <- numeric(n_countries)
    for (i in 1:n_countries) {
        # Aggregate wage across sectors (weighted by labor allocation)
        total_labor <- sum(labor_matrix[i, ])
        if (total_labor > 0) {
            avg_wage <- sum(wages[i, ] * labor_matrix[i, ]) / total_labor
            
            # Aggregate price index (Cobb-Douglas over sectors)
            price_level <- 1
            for (k in 1:n_sectors) {
                sector_share <- expenditure_matrix[i, k] / sum(expenditure_matrix[i, ])
                price_level <- price_level * (price_indices[i, k]^sector_share)
            }
            
            welfare[i] <- avg_wage / price_level
        } else {
            welfare[i] <- 1
        }
    }
    
    # Calculate GDP
    gdp <- numeric(n_countries)
    for (i in 1:n_countries) {
        gdp[i] <- sum(wages[i, ] * labor_matrix[i, ])
    }
    
    return(list(
        wages = wages,  # Matrix: countries x sectors
        price_indices = price_indices,  # Matrix: countries x sectors
        trade_shares = trade_shares,  # Array: origin x destination x sectors
        unit_costs = unit_costs,  # Matrix: countries x sectors
        gdp = gdp,  # Vector: countries
        welfare = welfare,  # Vector: countries
        iterations = iter
    ))
}

#' Diagnostic: Compare model fit with observed data
#' @param baseline_results Baseline equilibrium results
#' @param observed_data Observed trade shares, GDP, etc.
#' @return List with goodness-of-fit measures
check_model_fit <- function(baseline_results, observed_data) {
  
  # Compare predicted vs observed trade shares
  predicted_shares <- baseline_results$equilibrium$trade_shares
  observed_shares <- observed_data$trade_shares
  
  # Calculate correlation
  if (is.array(predicted_shares) && is.array(observed_shares)) {
    correlation <- cor(as.vector(predicted_shares), as.vector(observed_shares), use = "complete.obs")
  } else {
    correlation <- cor(predicted_shares, observed_shares, use = "complete.obs")
  }
  
  # Calculate RMSE
  if (is.array(predicted_shares) && is.array(observed_shares)) {
    rmse <- sqrt(mean((as.vector(predicted_shares) - as.vector(observed_shares))^2, na.rm = TRUE))
  } else {
    rmse <- sqrt(mean((predicted_shares - observed_shares)^2, na.rm = TRUE))
  }
  
  # Compare GDP per capita if available
  gdp_correlation <- NA
  if (!is.null(observed_data$gdp_per_capita) && length(observed_data$gdp_per_capita) > 0) {
    model_gdp <- baseline_results$equilibrium$welfare  # Real income per capita
    observed_gdp <- observed_data$gdp_per_capita
    # Ensure same length and order
    if (length(model_gdp) == length(observed_gdp) && sum(observed_gdp) > 0) {
      gdp_correlation <- cor(model_gdp, observed_gdp, use = "complete.obs")
    }
  }
  
  fit_measures <- list(
    trade_share_correlation = correlation,
    trade_share_rmse = rmse,
    gdp_per_capita_correlation = gdp_correlation
  )
  
  if (!is.na(gdp_correlation)) {
    # GDP correlation available
  }
  
  return(fit_measures)
}
