###############################################################################
# Functions for CDK (2012) Multi-Sector Ricardian Trade Model
# Following Costinot, Donaldson, and Komunjer (2012) exactly
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

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

#' Trade balance condition
#' @param trade_shares Matrix of trade shares (i to j)
#' @param Y Vector of outputs by country
#' @param deficits Vector of trade deficits (can be zero for balanced trade)
#' @return Trade balance residuals
trade_balance_condition <- function(trade_shares, Y, deficits = rep(0, length(Y))) {
  # Exports: sum over j of pi_ij * Y_j
  exports <- rowSums(trade_shares * matrix(Y, nrow = nrow(trade_shares), ncol = ncol(trade_shares), byrow = TRUE))
  # Imports: sum over i of pi_ij * Y_j  
  imports <- colSums(trade_shares * matrix(Y, nrow = nrow(trade_shares), ncol = ncol(trade_shares), byrow = FALSE))
  return(exports - imports - deficits)
}

#' Solve for equilibrium wages given technology and trade costs
#' @param theta Trade elasticity
#' @param T Technology parameters
#' @param tau Trade cost matrix
#' @param L Labor endowments
#' @param deficits Trade deficits (default to zero)
#' @param max_iter Maximum iterations
#' @param tol Tolerance for convergence
#' @return List with wages, trade shares, price indices
solve_equilibrium <- function(theta, T, tau, L, deficits = rep(0, length(L)), 
                             max_iter = 1000, tol = 1e-8) {
  
  n_countries <- length(L)
  
  # Initial guess for wages (normalize first country wage to 1)
  w <- rep(1, n_countries)
  w[1] <- 1
  
  for (iter in 1:max_iter) {
    w_old <- w
    
    # Calculate price indices for all countries
    P <- numeric(n_countries)
    for (j in 1:n_countries) {
      P[j] <- calculate_price_index(theta, T, tau[, j], w)
    }
    
    # Calculate trade shares matrix
    pi_matrix <- matrix(0, n_countries, n_countries)
    for (i in 1:n_countries) {
      for (j in 1:n_countries) {
        pi_matrix[i, j] <- calculate_trade_share(theta, T[i], tau[i, j], w[i], P[j])
      }
    }
    
    # Calculate outputs (Y = w * L in this model)
    Y <- w * L
    
    # Update wages using market clearing conditions
    # Skip the first country (numeraire)
    for (i in 2:n_countries) {
      # Labor market clearing: sum_j pi_ij * Y_j = w_i * L_i
      labor_demand <- sum(pi_matrix[i, ] * Y)
      w[i] <- labor_demand / L[i]
    }
    
    # Check convergence
    if (max(abs(w - w_old)) < tol) {
      cat("Converged in", iter, "iterations\n")
      break
    }
    
    if (iter == max_iter) {
      warning("Maximum iterations reached without convergence")
    }
  }
  
  # Final calculations
  P <- numeric(n_countries)
  for (j in 1:n_countries) {
    P[j] <- calculate_price_index(theta, T, tau[, j], w)
  }
  
  pi_matrix <- matrix(0, n_countries, n_countries)
  for (i in 1:n_countries) {
    for (j in 1:n_countries) {
      pi_matrix[i, j] <- calculate_trade_share(theta, T[i], tau[i, j], w[i], P[j])
    }
  }
  
  return(list(
    wages = w,
    trade_shares = pi_matrix,
    price_indices = P,
    welfare = w / P,
    output = w * L
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