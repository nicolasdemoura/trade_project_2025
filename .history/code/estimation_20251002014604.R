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

    # Complete the NA entries in T_composite_matrix with low-rank approximation through SVD
    T_composite_matrix <- as.matrix(T_composite_matrix)
    T_filled <- softImpute(T_composite_matrix, rank.max = 5, lambda = 0)
    T_composite_matrix <- complete(T_filled, T_composite_matrix)
    T_composite_matrix <- as.data.frame(T_composite_matrix)
    
    
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

#' Back out sector-specific technology parameters
#' @param estimation_results Results from technology parameter estimation
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return Matrix of technology parameters (countries x sectors)
extract_technology_matrix <- function(estimation_results, countries, sectors) {
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Initialize technology parameter matrix
  T_matrix <- matrix(1, nrow = n_countries, ncol = n_sectors,
                    dimnames = list(countries, sectors))
  
  # Extract from regression coefficients
  # This is a simplified extraction - the full method depends on the exact specification
  
  origin_effects <- estimation_results$technology_params$origin_effects
  
  # Convert coefficient names back to country-sector pairs
  # This requires parsing the factor level names
  
  # For now, return normalized values
  # Normalize first country in each sector to 1
  for (s in 1:n_sectors) {
    for (i in 1:n_countries) {
      if (i == 1) {
        T_matrix[i, s] <- 1  # Normalization
      } else {
        # Use relative differences from regression
        # This is placeholder logic - needs proper implementation
        T_matrix[i, s] <- exp(runif(1, -0.5, 0.5))  # Random for now
      }
    }
  }
  
  return(T_matrix)
}

#' Calibrate CDK (2012) baseline equilibrium
#' @param observed_trade_shares Observed trade share array (origin x destination x sectors)
#' @param T_composite_matrix Composite technology parameters from regression
#' @param labor_matrix Labor allocation matrix (countries x sectors)
#' @param theta Vector of trade elasticities by sector
#' @param beta_vec Expenditure shares by sector
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @return List with calibrated baseline equilibrium
calibrate_cdk_baseline <- function(observed_trade_shares, T_composite_matrix, labor_matrix, 
                                  theta, beta_vec, countries, sectors) {
  
  n_countries <- length(countries)
  n_sectors <- length(sectors)
  
  # Step 1: Decompose composite technology parameters
  # T_composite_is = ln(T_is) - θ_s * ln(w_i)
  # We need to separate pure technology T_is from wage effects
  
  # Normalize: set first country's wage to 1 in each sector
  # This means T_composite_1s = ln(T_1s) for all sectors
  
  pure_T_matrix <- matrix(1, n_countries, n_sectors)
  
  for (s in 1:n_sectors) {
    # Normalize technology in sector s relative to first country
    baseline_composite <- T_composite_matrix[1, s]
    
    for (i in 1:n_countries) {
      if (!is.na(T_composite_matrix[i, s])) {
        # T_is = exp(T_composite_is - T_composite_1s) * T_1s
        # With T_1s = 1 (normalization)
        pure_T_matrix[i, s] <- exp(T_composite_matrix[i, s] - baseline_composite)
      }
    }
  }
  
  # Step 2: Initial guess for trade costs (identity matrix + random component)
  tau_array <- array(1, dim = c(n_countries, n_countries, n_sectors))
  
  # Add some heterogeneity for non-domestic trade
  for (s in 1:n_sectors) {
    for (i in 1:n_countries) {
      for (j in 1:n_countries) {
        if (i != j) {
          tau_array[i, j, s] <- 1 + runif(1, 0, 0.5)  # Random trade costs 1-1.5
        }
      }
    }
  }
  
  # Step 3: Solve for equilibrium wages given technology and trade costs
  equilibrium <- solve_equilibrium_cdk(theta, pure_T_matrix, tau_array, 
                                      labor_matrix, beta_vec)
  
  # Step 4: Back out trade costs that rationalize observed trade shares

  
  calibrated_tau <- array(1, dim = c(n_countries, n_countries, n_sectors))
  
  for (s in 1:n_sectors) {
    for (i in 1:n_countries) {
      for (j in 1:n_countries) {
        
        observed_pi_ijs <- observed_trade_shares[i, j, s]
        
        if (!is.na(observed_pi_ijs) && observed_pi_ijs > 0) {
          
          # From CDK: π_ijs = T_is * (τ_ijs * w_i)^(-θ_s) / (P_js)^(-θ_s)
          # Solve for τ_ijs given observed π_ijs
          
          P_js <- equilibrium$price_indices[j, s]
          T_is <- pure_T_matrix[i, s]
          w_i <- equilibrium$wages[i]
          
          if (P_js > 0 && T_is > 0 && w_i > 0) {
            # τ_ijs = [(T_is / (π_ijs * P_js^(-θ_s)))^(1/θ_s)] / w_i
            numerator <- (T_is / (observed_pi_ijs * P_js^(-theta)))^(1/theta)
            calibrated_tau[i, j, s] <- numerator / w_i
          }
        }
      }
    }
  }
  
  # Step 5: Re-solve equilibrium with calibrated trade costs

  
  final_equilibrium <- solve_equilibrium_cdk(theta, pure_T_matrix, calibrated_tau, 
                                            labor_matrix, beta_vec)

  
  return(list(
    equilibrium = final_equilibrium,
    pure_technology = pure_T_matrix,
    calibrated_trade_costs = calibrated_tau,
    theta = theta,
    beta_vec = beta_vec,
    labor_matrix = labor_matrix,
    countries = countries,
    sectors = sectors
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
  if (!is.null(observed_data$gdp_per_capita)) {
    model_gdp <- baseline_results$equilibrium$welfare  # Real income per capita
    gdp_correlation <- cor(model_gdp, observed_data$gdp_per_capita, use = "complete.obs")
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