###############################################################################
# Parameter Estimation for CDK (2012) Multi-Sector Ricardian Model
# Implementing exact CDK (2012) Section 5.1 approach
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

###############################################################################
# CDK (2012) Section 5.1: Technology Parameter Estimation
###############################################################################

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


#' Simplified CDK (2012) baseline calibration using sequential approach
#' @param observed_trade_shares Observed trade share array (origin x destination x sectors)
#' @param observed_trade_balance Observed trade balance by country (can be NULL for balanced trade)
#' @param T_composite_matrix Composite technology parameters from regression
#' @param labor_matrix Labor allocation matrix (countries x sectors)
#' @param theta Vector or scalar of trade elasticities
#' @param beta_vec Expenditure shares by sector
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return List with calibrated baseline equilibrium
calibrate_cdk_baseline_simple <- function(observed_trade_shares, observed_trade_balance, T_composite_matrix, labor_matrix, 
                                         theta, beta_vec, countries, sectors) {
    
    n_countries <- length(countries)
    n_sectors <- length(sectors)
    
    # Handle theta parameter (scalar or vector)
    if (length(theta) == 1) {
        theta_vec <- rep(theta, n_sectors)
    } else {
        theta_vec <- theta
    }
    
    # Step 1: Extract normalized technology parameters
    pure_T_matrix <- matrix(1, n_countries, n_sectors)
    rownames(pure_T_matrix) <- countries
    colnames(pure_T_matrix) <- sectors
    
    for (s in 1:n_sectors) {
        for (i in 1:n_countries) {
            if (!is.na(T_composite_matrix[i, s])) {
                pure_T_matrix[i, s] <- exp(T_composite_matrix[i, s])
            }
        }
        # Normalize by first country
        pure_T_matrix[, s] <- pure_T_matrix[, s] / pure_T_matrix[1, s]
    }
    
    # Step 2: Simple wage initialization (GDP-based)
    total_labor <- rowSums(labor_matrix)
    wages <- total_labor / mean(total_labor)  # Normalize around 1
    names(wages) <- countries
    
    # Step 3: Calibrate trade costs from observed trade shares
    # Using the fact that π_ijs = T_is * (τ_ijs * w_i)^(-θ_s) / P_js^(-θ_s)
    # Rearranging: τ_ijs = (T_is / (π_ijs * P_js^(-θ_s)))^(1/θ_s) / w_i
    
    tau_array <- array(1, dim = c(n_countries, n_countries, n_sectors),
                      dimnames = list(countries, countries, sectors))
    
    # Simple approach: back out trade costs from observed trade shares
    for (s in 1:n_sectors) {
        for (i in 1:n_countries) {
            for (j in 1:n_countries) {
                if (i != j && observed_trade_shares[i, j, s] > 0) {
                    # Approximate trade cost using gravity-style relationship
                    domestic_share <- observed_trade_shares[j, j, s]
                    import_share <- observed_trade_shares[i, j, s]
                    
                    if (domestic_share > 0 && import_share > 0) {
                        # Trade cost that rationalizes observed trade patterns
                        tau_array[i, j, s] <- ((domestic_share / import_share) * 
                                              (pure_T_matrix[j, s] / pure_T_matrix[i, s]) * 
                                              (wages[j] / wages[i]))^(1/theta_vec[s])
                    } else {
                        tau_array[i, j, s] <- 2.0  # High trade cost for zero trade
                    }
                } else {
                    tau_array[i, j, s] <- 1.0  # No domestic trade costs
                }
            }
        }
    }
    
    # Step 4: Solve simple equilibrium with calibrated parameters
    baseline_equilibrium <- solve_equilibrium_cdk_simple(theta_vec, pure_T_matrix, tau_array, 
                                                        labor_matrix, beta_vec, wages)
    
    return(list(
    equilibrium = final_equilibrium,
    pure_technology = pure_T_matrix,
    calibrated_wages = final_wages,
    calibrated_trade_costs = final_tau,
    theta = theta,
    theta_vec = if(length(theta) == 1) rep(theta, n_sectors) else theta,  # Handle scalar or vector theta
    beta_vec = beta_vec,
    labor_matrix = labor_matrix,
    countries = countries,
    sectors = sectors,
    optimization_result = optim_result,
    final_objective_value = optim_result$value
    ))
}

#' Solve CDK equilibrium with fixed wages
#' @param theta Vector of trade elasticities
#' @param T_matrix Technology matrix
#' @param tau_array Trade cost array
#' @param labor_matrix Labor allocation
#' @param beta_vec Expenditure shares
#' @param wages Fixed wage vector
#' @return Equilibrium object
solve_equilibrium_cdk_fixed_wages <- function(theta, T_matrix, tau_array, labor_matrix, beta_vec, wages) {
    
    n_countries <- nrow(T_matrix)
    n_sectors <- ncol(T_matrix)
    
    # Calculate price indices given fixed wages
    price_indices <- array(0, dim = c(n_countries, n_sectors))
    
    for (j in 1:n_countries) {
      for (s in 1:n_sectors) {
          sum_competitors <- 0
          for (i in 1:n_countries) {
            cost_ijs <- tau_array[i, j, s] * wages[i]
            sum_competitors <- sum_competitors + T_matrix[i, s] * (cost_ijs)^(-theta)
          }
          price_indices[j, s] <- sum_competitors^(-1/theta)
      }
    }
    
    # Calculate trade shares
    trade_shares <- array(0, dim = c(n_countries, n_countries, n_sectors))
    
    for (s in 1:n_sectors) {
      for (i in 1:n_countries) {
        for (j in 1:n_countries) {
          cost_ijs <- tau_array[i, j, s] * wages[i]
          numerator <- T_matrix[i, s] * (cost_ijs)^(-theta)
          denominator <- price_indices[j, s]^(-theta)
          trade_shares[i, j, s] <- numerator / denominator
        }
      }
    }
    
    # Calculate other equilibrium objects
    gdp <- rowSums(wages * labor_matrix)  # Simplified GDP calculation
    
    return(list(
    wages = wages,
    price_indices = price_indices,
    trade_shares = trade_shares,
    gdp = gdp,
    welfare = wages  # Real wages as welfare measure
    ))
}

#' Calculate trade balance from equilibrium
#' @param equilibrium Equilibrium results
#' @param labor_matrix Labor allocation
#' @param beta_vec Expenditure shares
#' @return Trade balance by country
calculate_trade_balance <- function(equilibrium, labor_matrix, beta_vec) {
  
  n_countries <- length(equilibrium$wages)
  n_sectors <- length(beta_vec)
  
  trade_balance <- rep(0, n_countries)
  
  # Simplified trade balance calculation
  # TB_i = Exports_i - Imports_i
  for (i in 1:n_countries) {
    
    # Exports: sum over destinations and sectors
    exports <- 0
    for (j in 1:n_countries) {
      for (s in 1:n_sectors) {
        if (i != j) {  # Only international trade
          exports <- exports + equilibrium$trade_shares[i, j, s] * equilibrium$gdp[j] * beta_vec[s]
        }
      }
    }
  
  # Imports: sum over origins and sectors  
  imports <- 0
    for (k in 1:n_countries) {
      for (s in 1:n_sectors) {
        if (k != i) {  # Only international trade
          imports <- imports + equilibrium$trade_shares[k, i, s] * equilibrium$gdp[i] * beta_vec[s]
        }
      }
  }
  
  trade_balance[i] <- exports - imports
  }
  
  return(trade_balance)
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