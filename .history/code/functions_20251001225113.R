###############################################################################
# Functions for Eaton-Kortum Multi-Sector Trade Model
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

#' Calculate trade share from Eaton-Kortum model
#' @param theta Trade elasticity parameter (sector-specific)
#' @param T_i Technology parameter for origin country
#' @param T_j Technology parameter for destination country  
#' @param tau_ij Bilateral trade cost
#' @param w_i Wage in origin country
#' @param w_j Wage in destination country
#' @param P_j Price index in destination country
#' @return Trade share pi_ij
calculate_trade_share <- function(theta, T_i, tau_ij, w_i, P_j) {
  numerator <- T_i * (tau_ij * w_i)^(-theta)
  denominator <- P_j^(-theta)
  return(numerator / denominator)
}

#' Calculate price index in Eaton-Kortum model
#' @param theta Trade elasticity parameter
#' @param T Vector of technology parameters for all countries
#' @param tau Vector of trade costs to destination j
#' @param w Vector of wages for all countries
#' @return Price index P_j
calculate_price_index <- function(theta, T, tau, w) {
  sum_term <- sum(T * (tau * w)^(-theta))
  return(sum_term^(-1/theta))
}

#' Calculate welfare (real income per capita)
#' @param w_j Nominal wage in country j
#' @param P_j Price index in country j
#' @param L_j Labor force in country j
#' @return Real income per capita
calculate_welfare <- function(w_j, P_j, L_j) {
  return(w_j / P_j)
}

#' Market clearing condition for labor
#' @param w Vector of wages
#' @param L Vector of labor endowments
#' @param trade_shares Matrix of trade shares
#' @param Y Vector of outputs
#' @return Excess demand for labor (should be zero in equilibrium)
labor_market_clearing <- function(w, L, trade_shares, Y) {
  # Labor demand = sum over all destinations j of (trade share * Y_j)
  labor_demand <- rowSums(trade_shares * matrix(Y, nrow = length(Y), ncol = length(Y), byrow = TRUE))
  # Labor supply = w * L (total labor income)
  labor_supply <- w * L
  return(labor_demand - labor_supply)
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