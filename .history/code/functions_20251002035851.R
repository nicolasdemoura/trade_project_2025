###############################################################################
# Functions for CDK (2012) Multi-Sector Ricardian Trade Model
# Following Costinot, Donaldson, and Komunjer (2012) exactly
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

###########################################################################
# Function to map sectors
#############################################################################

# Create mapping function
map_sector <- function(x) {
  idx <- match(x, raw_sectors)
  ifelse(!is.na(idx), clean_sectors[idx], NA)
}


#' Calculate trade share from CDK (2012) multi-sector Ricardian model
#' @param theta_s Trade elasticity parameter for sector s
#' @param T_is Technology parameter for country i in sector s
#' @param tau_ijs Bilateral trade cost from i to j in sector s
#' @param w_i Wage in origin country i
#' @param P_js Price index in destination country j, sector s
#' @return Trade share pi_ijs (share of country j's sector s spending on country i)
calculate_trade_share_cdk <- function(theta_s, T_is, tau_ijs, w_i, P_js) {
  # CDK (2012) equation: π_ijs = T_is * (τ_ijs * w_i)^(-θ_s) / (P_js)^(-θ_s)
  numerator <- T_is * (tau_ijs * w_i)^(-theta_s)
  denominator <- P_js^(-theta_s)
  return(numerator / denominator)
}

#' Calculate sector-specific price index from CDK (2012)
#' @param theta_s Trade elasticity for sector s
#' @param T_s Vector of technology parameters for all countries in sector s
#' @param tau_s Matrix of trade costs to destination j in sector s
#' @param w Vector of wages for all countries
#' @param j Destination country index
#' @return Price index P_js for country j, sector s
calculate_price_index_cdk <- function(theta_s, T_s, tau_s, w, j) {
  # CDK (2012): P_js = [∑_i T_is * (τ_ijs * w_i)^(-θ_s)]^(-1/θ_s)
  sum_term <- sum(T_s * (tau_s[, j] * w)^(-theta_s))
  return(sum_term^(-1/theta_s))
}

#' Calculate welfare (real income per capita) in CDK (2012) model
#' @param w_j Nominal wage in country j
#' @param P_j Vector of price indices for all sectors in country j
#' @param beta_s Vector of expenditure shares for each sector s
#' @return Real income per capita (exact price index approach)
calculate_welfare_cdk <- function(w_j, P_j, beta_s) {
  # CDK (2012) welfare: W_j = w_j / [∏_s (P_js)^β_s]
  # This is the exact price index for Cobb-Douglas preferences
  aggregate_price_index <- prod(P_j^beta_s)
  return(w_j / aggregate_price_index)
}

#' Labor market clearing condition for CDK (2012) multi-sector model
#' @param w Vector of wages by country
#' @param L Matrix of labor allocation (countries x sectors)
#' @param pi_array Array of trade shares (origin x destination x sectors)
#' @param Y_js Matrix of sectoral absorption (countries x sectors)
#' @return Matrix of excess labor demand by country and sector
labor_market_clearing_cdk <- function(w, L, pi_array, Y_js) {
  n_countries <- length(w)
  n_sectors <- dim(pi_array)[3]
  
  excess_demand <- matrix(0, n_countries, n_sectors)
  
  for (s in 1:n_sectors) {
    for (i in 1:n_countries) {
      # Labor demand in sector s, country i = ∑_j π_ijs * Y_js
      labor_demand <- sum(pi_array[i, , s] * Y_js[, s])
      # Labor supply in sector s, country i = w_i * L_is
      labor_supply <- w[i] * L[i, s]
      excess_demand[i, s] <- labor_demand - labor_supply
    }
  }
  
  return(excess_demand)
}

#' Trade balance condition for CDK (2012) model
#' @param pi_array Array of trade shares (origin x destination x sectors)
#' @param Y_js Matrix of sectoral absorption (countries x sectors)
#' @param D_j Vector of trade deficits by country (default: balanced trade)
#' @return Vector of trade balance residuals by country
trade_balance_condition_cdk <- function(pi_array, Y_js, D_j = NULL) {
  n_countries <- dim(pi_array)[1]
  n_sectors <- dim(pi_array)[3]
  
  if (is.null(D_j)) {
    D_j <- rep(0, n_countries)  # Balanced trade assumption
  }
  
  trade_balance <- numeric(n_countries)
  
  for (i in 1:n_countries) {
    # Total exports from country i: ∑_s ∑_j π_ijs * Y_js
    exports_i <- sum(pi_array[i, , ] * Y_js)
    
    # Total imports to country i: ∑_s ∑_k π_kis * Y_is  
    imports_i <- sum(pi_array[, i, ] * matrix(Y_js[i, ], nrow = n_countries, ncol = n_sectors, byrow = TRUE))
    
    trade_balance[i] <- exports_i - imports_i - D_j[i]
  }
  
  return(trade_balance)
}

#' Solve CDK (2012) multi-sector equilibrium
#' @param theta_vec Vector of trade elasticities by sector
#' @param T_matrix Technology parameters (countries x sectors)
#' @param tau_array Trade costs (origin x destination x sectors, domestic costs = 1)
#' @param L_matrix Labor endowments (countries x sectors)
#' @param beta_vec Expenditure shares by sector
#' @param D_vec Trade deficits by country (default: balanced trade)
#' @param max_iter Maximum iterations
#' @param tol Convergence tolerance
#' @return List with equilibrium wages, trade shares, price indices, welfare
solve_equilibrium_cdk <- function(theta_vec, T_matrix, tau_array, L_matrix, beta_vec, 
                                 D_vec = NULL, max_iter = 1000, tol = 1e-8) {
  
  n_countries <- nrow(T_matrix)
  n_sectors <- ncol(T_matrix)
  
  # Ensure domestic trade costs are exactly 1 (no iceberg costs for domestic trade)
  for (s in 1:n_sectors) {
    for (i in 1:n_countries) {
      tau_array[i, i, s] <- 1.0
    }
  }
  
  if (is.null(D_vec)) {
    D_vec <- rep(0, n_countries)
  }
  
  # Initial guess for wages (normalize first country to 1)
  w <- rep(1, n_countries)
  w[1] <- 1
  
  cat("Starting CDK equilibrium solver...\n")
  
  for (iter in 1:max_iter) {
    w_old <- w
    
    # Step 1: Calculate price indices for all countries and sectors
    P_matrix <- matrix(0, n_countries, n_sectors)
    for (j in 1:n_countries) {
      for (s in 1:n_sectors) {
        P_matrix[j, s] <- calculate_price_index_cdk(theta_vec[s], T_matrix[, s], tau_array[, , s], w, j)
      }
    }
    
    # Step 2: Calculate trade shares for all country pairs and sectors
    pi_array <- array(0, dim = c(n_countries, n_countries, n_sectors))
    for (i in 1:n_countries) {
      for (j in 1:n_countries) {
        for (s in 1:n_sectors) {
          pi_array[i, j, s] <- calculate_trade_share_cdk(theta_vec[s], T_matrix[i, s], 
                                                        tau_array[i, j, s], w[i], P_matrix[j, s])
        }
      }
    }
    
    # Step 3: Calculate sectoral absorption Y_js = β_s * w_j * L_j (total labor income)
    total_labor_income <- w * rowSums(L_matrix)
    Y_matrix <- matrix(0, n_countries, n_sectors)
    for (s in 1:n_sectors) {
      Y_matrix[, s] <- beta_vec[s] * total_labor_income
    }
    
    # Step 4: Update wages using labor market clearing (skip numeraire country)
    for (i in 2:n_countries) {
      total_labor_demand <- 0
      total_labor_supply <- 0
      
      for (s in 1:n_sectors) {
        # Labor demand in sector s: ∑_j π_ijs * Y_js
        labor_demand_s <- sum(pi_array[i, , s] * Y_matrix[, s])
        total_labor_demand <- total_labor_demand + labor_demand_s
        
        # Labor supply in sector s: L_is (labor is mobile across sectors)
        total_labor_supply <- total_labor_supply + L_matrix[i, s]
      }
      
      # Update wage: total labor demand = w_i * total labor supply
      w[i] <- total_labor_demand / (w[1] * total_labor_supply)  # Relative to numeraire
    }
    
    # Check convergence
    if (max(abs(w - w_old)) < tol) {
      cat("CDK equilibrium converged in", iter, "iterations\n")
      break
    }
    
    if (iter == max_iter) {
      warning("CDK equilibrium: Maximum iterations reached without convergence")
    }
  }
  
  # Final calculations
  # Recalculate everything with final wages
  for (j in 1:n_countries) {
    for (s in 1:n_sectors) {
      P_matrix[j, s] <- calculate_price_index_cdk(theta_vec[s], T_matrix[, s], tau_array[, , s], w, j)
    }
  }
  
  for (i in 1:n_countries) {
    for (j in 1:n_countries) {
      for (s in 1:n_sectors) {
        pi_array[i, j, s] <- calculate_trade_share_cdk(theta_vec[s], T_matrix[i, s], 
                                                      tau_array[i, j, s], w[i], P_matrix[j, s])
      }
    }
  }
  
  # Calculate welfare for each country
  welfare <- numeric(n_countries)
  for (j in 1:n_countries) {
    welfare[j] <- calculate_welfare_cdk(w[j], P_matrix[j, ], beta_vec)
  }
  
  return(list(
    wages = w,
    trade_shares = pi_array,
    price_indices = P_matrix,
    welfare = welfare,
    sectoral_absorption = Y_matrix,
    labor_allocation = L_matrix
  ))
}

#' Back out trade costs from observed trade shares
#' @param theta Trade elasticity
#' @param T Technology parameters
#' @param w Wages
#' @param pi_observed Observed trade share matrix
#' @return Implied trade cost matrix
back_out_trade_costs <- function(theta, T, w, pi_observed) {
  n_countries <- length(T)
  tau_matrix <- matrix(1, n_countries, n_countries)
  
  for (i in 1:n_countries) {
    for (j in 1:n_countries) {
      if (i != j && pi_observed[i, j] > 0) {
        # From pi_ij = T_i * (tau_ij * w_i)^(-theta) / P_j^(-theta)
        # We need P_j first
        P_j <- sum(T * (tau_matrix[, j] * w)^(-theta))^(-1/theta)
        
        # Then solve for tau_ij
        tau_matrix[i, j] <- ((T[i] / (pi_observed[i, j] * P_j^(-theta)))^(1/theta)) / w[i]
      }
    }
  }
  
  return(tau_matrix)
}

#' Apply tariffs to trade cost matrix
#' @param tau_baseline Baseline trade costs
#' @param tariff_matrix Matrix of tariff rates (as proportions, e.g., 0.25 for 25%)
#' @return New trade cost matrix with tariffs
apply_tariffs <- function(tau_baseline, tariff_matrix) {
  return(tau_baseline * (1 + tariff_matrix))
}

#' Calculate welfare changes from baseline to counterfactual
#' @param welfare_baseline Baseline welfare levels
#' @param welfare_counterfactual Counterfactual welfare levels
#' @return Percentage change in welfare
calculate_welfare_changes <- function(welfare_baseline, welfare_counterfactual) {
  return((welfare_counterfactual - welfare_baseline) / welfare_baseline * 100)
}

#' Estimate technology parameters using CDK (2012) approach
#' @param trade_data Data frame with bilateral trade flows
#' @param output_data Data frame with country outputs
#' @param theta Trade elasticity (fixed or estimated)
#' @return List with estimated technology parameters and regression results
estimate_technology_parameters <- function(trade_data, output_data, theta = NULL) {
  
  # This function will implement the regression approach from CDK (2012) Section 5.1
  # The exact specification will depend on the data structure
  
  # For now, return a placeholder
  # TODO: Implement the actual regression once we understand the data structure
  
  return(list(
    T_estimates = NULL,
    regression_results = NULL,
    theta_estimate = theta
  ))
}

#' Format results for output tables
#' @param baseline_results Baseline equilibrium results
#' @param counterfactual_results Counterfactual results
#' @param country_names Vector of country names
#' @return Formatted data frame for tables
format_results_table <- function(baseline_results, counterfactual_results, country_names) {
  
  welfare_change <- calculate_welfare_changes(baseline_results$welfare, 
                                            counterfactual_results$welfare)
  
  results_df <- data.frame(
    Country = country_names,
    Baseline_Wage = baseline_results$wages,
    Baseline_Welfare = baseline_results$welfare,
    Counterfactual_Wage = counterfactual_results$wages,
    Counterfactual_Welfare = counterfactual_results$welfare,
    Welfare_Change_Percent = welfare_change
  )
  
  return(results_df)
}




