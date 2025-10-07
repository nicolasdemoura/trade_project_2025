###############################################################################
# Counterfactual Analysis: Trump Tariff Impact using CDK (2012) Methodology
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

###############################################################################
# Define Trump Tariff Scenarios for CDK (2012) Model
###############################################################################

#' Create Trump tariff scenario matrix for CDK (2012) analysis
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @param baseline_year Base year for analysis
#' @return Array of tariff rates (origin x destination x sector)
create_trump_tariff_scenario_cdk <- function(countries, sectors, baseline_year = 2011) {
  
  n_countries <- length(countries)
  
  # CDK multi-sector case (sectors should not be null)
  n_sectors <- length(sectors)
  tariff_array <- array(0, dim = c(n_countries, n_countries, n_sectors),
                       dimnames = list(countries, countries, sectors))
  
  # Identify US in country list
  us_index <- which(toupper(countries) %in% c("USA", "US", "UNITED STATES", "AMERICA"))
  if (length(us_index) == 0) {
    # Try alternative names
    us_index <- which(grepl("US|AMERICA", toupper(countries)))
  }
  
  if (length(us_index) == 0) {
    warning("Could not identify United States in country list")
    return(tariff_array)
  }
  
  us_idx <- us_index[1]
  
  # Trump tariff scenario (based on his 2024 campaign proposals):
  # - 10-20% universal tariff on all imports
  # - 60% tariff on China
  # - Additional tariffs on specific countries/sectors
  
  # Identify China
  china_index <- which(toupper(countries) %in% c("CHINA", "CHN", "CN"))
  if (length(china_index) == 0) {
    china_index <- which(grepl("CHINA", toupper(countries)))
  }
  
  # Apply universal tariff (15% average of 10-20% range)
  universal_tariff <- 0.15
  
  if (is.null(sectors)) {
    # Single sector
    tariff_matrix[, us_idx] <- universal_tariff
    
    # Higher tariff on China
    if (length(china_index) > 0) {
      china_idx <- china_index[1]
      tariff_matrix[china_idx, us_idx] <- 0.60
    }
    
    # No tariffs on domestic trade
    tariff_matrix[us_idx, us_idx] <- 0
    
  } else {
    # Multi-sector
    for (s in 1:length(sectors)) {
      tariff_matrix[, us_idx, s] <- universal_tariff
      
      # Higher tariff on China
      if (length(china_index) > 0) {
        china_idx <- china_index[1]
        tariff_matrix[china_idx, us_idx, s] <- 0.60
      }
      
      # No tariffs on domestic trade
      tariff_matrix[us_idx, us_idx, s] <- 0
    }
    
    if (length(china_index) > 0) {
      # Applied 60% tariff on China -> US across all sectors
    }
  }
  
  return(tariff_matrix)
}

#' Alternative tariff scenarios for robustness
#' @param countries Vector of country names  
#' @param sectors Vector of sector names
#' @param scenario Scenario type ("moderate", "aggressive", "targeted")
#' @return Tariff matrix for alternative scenario
create_alternative_tariff_scenario <- function(countries, sectors = NULL, scenario = "moderate") {
  
  n_countries <- length(countries)
  
  if (is.null(sectors)) {
    tariff_matrix <- matrix(0, n_countries, n_countries)
  } else {
    tariff_matrix <- array(0, dim = c(n_countries, n_countries, length(sectors)))
  }
  
  # Find US index
  us_index <- which(toupper(countries) %in% c("USA", "US", "UNITED STATES", "AMERICA"))
  if (length(us_index) == 0) {
    us_index <- which(grepl("US|AMERICA", toupper(countries)))
  }
  
  if (length(us_index) > 0) {
    us_idx <- us_index[1]
    
    # Different scenarios
    if (scenario == "moderate") {
      # Moderate tariffs
      universal_rate <- 0.10
      china_rate <- 0.30
    } else if (scenario == "aggressive") {
      # More aggressive tariffs
      universal_rate <- 0.25
      china_rate <- 0.100  # 100% tariff on China
    } else if (scenario == "targeted") {
      # Targeted tariffs on specific sectors/countries
      universal_rate <- 0.05
      china_rate <- 0.60
    }
    
    # Apply tariffs
    if (is.null(sectors)) {
      tariff_matrix[, us_idx] <- universal_rate
      tariff_matrix[us_idx, us_idx] <- 0
    } else {
      for (s in 1:length(sectors)) {
        tariff_matrix[, us_idx, s] <- universal_rate
        tariff_matrix[us_idx, us_idx, s] <- 0
      }
    }
    
    # Apply China tariffs if identified
    china_index <- which(toupper(countries) %in% c("CHINA", "CHN", "CN"))
    if (length(china_index) > 0) {
      china_idx <- china_index[1]
      if (is.null(sectors)) {
        tariff_matrix[china_idx, us_idx] <- china_rate
      } else {
        for (s in 1:length(sectors)) {
          tariff_matrix[china_idx, us_idx, s] <- china_rate
        }
      }
    }
  }
  
  return(tariff_matrix)
}

###############################################################################
# Counterfactual Equilibrium Computation
###############################################################################

#' Compute CDK (2012) counterfactual equilibrium with tariffs
#' @param baseline_results CDK baseline equilibrium results
#' @param tariff_array Array of new tariff rates (origin x destination x sector)
#' @param max_iter Maximum iterations for convergence
#' @param tolerance Convergence tolerance
#' @return CDK counterfactual equilibrium results
compute_counterfactual_equilibrium_cdk <- function(baseline_results, tariff_array, 
                                                  max_iter = 1000, tolerance = 1e-8) {
  
  # Extract baseline parameters
  baseline_tau <- if (!is.null(baseline_results$calibrated_trade_costs)) {
    baseline_results$calibrated_trade_costs
  } else {
    baseline_results$equilibrium$calibrated_trade_costs
  }
  
  # Get base iceberg costs (without baseline tariffs)
  base_tau <- if (!is.null(baseline_results$equilibrium$base_trade_costs)) {
    baseline_results$equilibrium$base_trade_costs
  } else {
    baseline_tau  # Fallback if base costs not available
  }
  
  # Get baseline tariffs
  baseline_tariffs <- if (!is.null(baseline_results$equilibrium$baseline_tariffs)) {
    baseline_results$equilibrium$baseline_tariffs
  } else {
    array(0, dim = dim(tariff_array))  # Assume no baseline tariffs
  }
  
  T_matrix <- baseline_results$pure_technology
  theta_vec <- baseline_results$theta_vec
  beta_vec <- baseline_results$beta_vec
  labor_matrix <- baseline_results$labor_matrix
  
  # Apply new tariffs to base trade costs
  # New trade costs = base_iceberg_costs * (1 + new_tariff_rate)
  new_tau <- base_tau * (1 + tariff_array)
  

  
  # Check if we have multi-sector matrices available
  if (!is.null(baseline_results$beta_matrix)) {
    # Extract labor mobility setting from baseline results (default to FALSE if not available)
    labor_mobility <- if (!is.null(baseline_results$labor_mobility)) baseline_results$labor_mobility else FALSE
    
    # Use baseline tau_array from optimization if available, otherwise use default
    if (!is.null(baseline_results$equilibrium$tau_array)) {
      baseline_tau <- baseline_results$equilibrium$tau_array
      # Apply tariffs to optimized baseline trade costs
      new_tau <- baseline_tau * (1 + tariff_array)
    } else {
      # Fall back to provided or default tau_array
      new_tau <- baseline_tau * (1 + tariff_array)  
    }
    
    # Use multi-sector solver
    counterfactual_equilibrium <- solve_equilibrium_cdk_multi_sector(
      theta = theta_vec[1],  # Use first element as scalar
      T_matrix = T_matrix, 
      tau_array = new_tau,
      labor_matrix = labor_matrix,
      beta_matrix = baseline_results$beta_matrix,
      gamma_matrix = baseline_results$gamma_matrix,
      expenditure_matrix = baseline_results$expenditure_matrix,
      labor_mobility = labor_mobility,
      max_iter = max_iter, 
      tol = tolerance
    )
  } else {
    # Use original solver for backward compatibility  
    counterfactual_equilibrium <- solve_equilibrium_cdk(theta_vec, T_matrix, new_tau, 
                                                       labor_matrix, beta_vec,
                                                       max_iter = max_iter, tol = tolerance)
  }
  

  
  return(list(
    equilibrium = counterfactual_equilibrium,
    new_trade_costs = new_tau,
    tariff_array = tariff_array,
    # Keep baseline parameters for comparison
    baseline_trade_costs = baseline_tau,
    technology_matrix = T_matrix,
    theta_vec = theta_vec,
    beta_vec = beta_vec
  ))
}

###############################################################################
# Welfare Analysis
###############################################################################

#' Calculate detailed welfare effects using CDK (2012) methodology
#' @param baseline_results CDK baseline equilibrium
#' @param counterfactual_results CDK counterfactual equilibrium  
#' @param countries Vector of country names
#' @return Detailed CDK welfare analysis results
analyze_welfare_effects_cdk <- function(baseline_results, counterfactual_results, countries) {
  
  # Extract basic welfare measures
  baseline_welfare <- baseline_results$equilibrium$welfare
  counterfactual_welfare <- counterfactual_results$equilibrium$welfare
  
  # Extract real income measures (including tariff revenue)
  baseline_real_income <- if (!is.null(baseline_results$equilibrium$real_income)) {
    baseline_results$equilibrium$real_income
  } else {
    baseline_welfare  # Fallback to basic welfare
  }
  
  counterfactual_real_income <- if (!is.null(counterfactual_results$equilibrium$real_income)) {
    counterfactual_results$equilibrium$real_income  
  } else {
    counterfactual_welfare  # Fallback to basic welfare
  }
  
  # Extract tariff revenues
  baseline_tariff_revenue <- if (!is.null(baseline_results$equilibrium$tariff_revenue)) {
    rowSums(baseline_results$equilibrium$tariff_revenue)  # Total by country
  } else {
    rep(0, length(countries))
  }
  
  counterfactual_tariff_revenue <- if (!is.null(counterfactual_results$equilibrium$tariff_revenue)) {
    rowSums(counterfactual_results$equilibrium$tariff_revenue)  # Total by country
  } else {
    rep(0, length(countries))
  }
  
  # Calculate welfare changes (% change)
  welfare_changes <- (counterfactual_welfare - baseline_welfare) / baseline_welfare * 100
  real_income_changes <- (counterfactual_real_income - baseline_real_income) / baseline_real_income * 100
  tariff_revenue_changes <- (counterfactual_tariff_revenue - baseline_tariff_revenue)
  
  # CDK welfare decomposition
  baseline_wages <- baseline_results$equilibrium$wages
  baseline_price_matrix <- baseline_results$equilibrium$price_indices
  counterfactual_wages <- counterfactual_results$equilibrium$wages  
  counterfactual_price_matrix <- counterfactual_results$equilibrium$price_indices
  
  # Calculate aggregate price indices (Cobb-Douglas)
  beta_vec <- baseline_results$beta_vec
  
  baseline_aggregate_prices <- numeric(length(countries))
  counterfactual_aggregate_prices <- numeric(length(countries))
  
  for (j in 1:length(countries)) {
    baseline_aggregate_prices[j] <- prod(baseline_price_matrix[j, ]^beta_vec)
    counterfactual_aggregate_prices[j] <- prod(counterfactual_price_matrix[j, ]^beta_vec)
  }
  
  # Decompose welfare changes
  # log(W'/W) = log(w'/w) - log(P'/P)
  wage_change <- (counterfactual_wages - baseline_wages) / baseline_wages * 100
  price_change <- (counterfactual_aggregate_prices - baseline_aggregate_prices) / baseline_aggregate_prices * 100
  
  # Sectoral price changes
  n_sectors <- ncol(baseline_price_matrix)
  sectoral_price_changes <- matrix(0, length(countries), n_sectors)
  
  for (s in 1:n_sectors) {
    sectoral_price_changes[, s] <- (counterfactual_price_matrix[, s] - baseline_price_matrix[, s]) / 
                                  baseline_price_matrix[, s] * 100
  }
  
  # Create detailed results data frame
  welfare_df <- data.frame(
    Country = countries,
    Baseline_Welfare = baseline_welfare,
    Counterfactual_Welfare = counterfactual_welfare,
    Welfare_Change_Percent = welfare_changes,
    Baseline_Real_Income = baseline_real_income,
    Counterfactual_Real_Income = counterfactual_real_income,
    Real_Income_Change_Percent = real_income_changes,
    Baseline_Tariff_Revenue = baseline_tariff_revenue,
    Counterfactual_Tariff_Revenue = counterfactual_tariff_revenue,
    Tariff_Revenue_Change = tariff_revenue_changes,
    Wage_Change_Percent = wage_change,
    Aggregate_Price_Change_Percent = price_change,
    stringsAsFactors = FALSE
  )
  
  # Add sectoral price changes
  for (s in 1:n_sectors) {
    welfare_df[[paste0("Price_Change_Sector_", s)]] <- sectoral_price_changes[, s]
  }
  
  # Global welfare calculation (GDP-weighted)
  # Use baseline wages as proxy for country size
  country_weights <- baseline_wages / sum(baseline_wages)
  global_welfare_change <- sum(welfare_changes * country_weights)
  
  # Identify winners and losers
  winners <- countries[welfare_changes > 0]
  losers <- countries[welfare_changes < 0]
  
  # Calculate terms of trade effects
  # This requires comparing import and export price changes
  tot_effects <- numeric(length(countries))
  # Placeholder for now - would need detailed calculation
  

  
  return(list(
    welfare_table = welfare_df,
    global_change = global_welfare_change,
    winners = winners,
    losers = losers,
    sectoral_price_changes = sectoral_price_changes,
    country_weights = country_weights,
    summary_stats = list(
      mean_change = mean(welfare_changes),
      median_change = median(welfare_changes),
      max_gain = max(welfare_changes),
      max_loss = min(welfare_changes),
      weighted_mean = global_welfare_change
    )
  ))
}

#' Run multiple tariff scenarios for robustness
#' @param baseline_results Baseline equilibrium
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @param theta Trade elasticity
#' @return Results from multiple scenarios
run_robustness_analysis <- function(baseline_results, countries, sectors = NULL, theta) {
  

  
  scenarios <- c("moderate", "aggressive", "targeted")
  robustness_results <- list()
  
  for (scenario in scenarios) {
    # Running scenario
    
    # Create tariff matrix for this scenario
    tariff_matrix <- create_alternative_tariff_scenario(countries, sectors, scenario)
    
    # Compute counterfactual
    counterfactual <- compute_counterfactual_equilibrium(baseline_results, tariff_matrix, theta)
    
    # Analyze welfare effects
    welfare_analysis <- analyze_welfare_effects(baseline_results, counterfactual, countries)
    
    robustness_results[[scenario]] <- list(
      tariffs = tariff_matrix,
      counterfactual = counterfactual,
      welfare = welfare_analysis
    )
  }
  

  
  return(robustness_results)
}

###############################################################################
# Results Comparison and Visualization
###############################################################################

#' Compare results across scenarios
#' @param main_results Main Trump scenario results
#' @param robustness_results Alternative scenario results
#' @param countries Vector of country names
#' @return Comparison table
compare_scenarios <- function(main_results, robustness_results, countries) {
  
  # Extract welfare changes from all scenarios
  main_changes <- main_results$welfare_table$Welfare_Change_Percent
  
  comparison_df <- data.frame(
    Country = countries,
    Main_Scenario = main_changes
  )
  
  # Add robustness scenarios
  for (scenario_name in names(robustness_results)) {
    scenario_changes <- robustness_results[[scenario_name]]$welfare$welfare_table$Welfare_Change_Percent
    comparison_df[[paste0(scenario_name, "_scenario")]] <- scenario_changes
  }
  
  # Calculate range and standard deviation across scenarios
  scenario_cols <- grep("scenario", colnames(comparison_df), value = TRUE)
  all_scenario_cols <- c("Main_Scenario", scenario_cols)
  
  comparison_df$Min_Change <- apply(comparison_df[all_scenario_cols], 1, min)
  comparison_df$Max_Change <- apply(comparison_df[all_scenario_cols], 1, max)
  comparison_df$Range <- comparison_df$Max_Change - comparison_df$Min_Change
  comparison_df$Std_Dev <- apply(comparison_df[all_scenario_cols], 1, sd)
  
  return(comparison_df)
}

#' Generate CDK summary statistics and tables
#' @param welfare_analysis CDK welfare analysis results
#' @param countries Vector of country names
#' @return Formatted CDK summary tables
generate_summary_tables <- function(welfare_analysis, countries) {
  
  welfare_table <- welfare_analysis$welfare_table
  
  # Sort by welfare change (largest losses to largest gains)
  welfare_table <- welfare_table[order(welfare_table$Welfare_Change_Percent), ]
  
  # Create summary by region (if we can identify regions)
  # This would require mapping countries to regions
  
  # Top gainers and losers
  top_gainers <- head(welfare_table[order(-welfare_table$Welfare_Change_Percent), ], 5)
  top_losers <- head(welfare_table[order(welfare_table$Welfare_Change_Percent), ], 5)
  
  return(list(
    full_table = welfare_table,
    top_gainers = top_gainers,
    top_losers = top_losers,
    summary_stats = welfare_analysis$summary_stats
  ))
}

###############################################################################
# Compatibility functions for CDK methodology
###############################################################################

#' Compatibility wrapper for format_results_table
format_results_table <- function(baseline_results, counterfactual_results, countries) {
  
  baseline_welfare <- baseline_results$equilibrium$welfare
  counterfactual_welfare <- counterfactual_results$equilibrium$welfare
  baseline_wages <- baseline_results$equilibrium$wages
  counterfactual_wages <- counterfactual_results$equilibrium$wages
  
  welfare_change <- (counterfactual_welfare - baseline_welfare) / baseline_welfare * 100
  
  results_df <- data.frame(
    Country = countries,
    Baseline_Wage = baseline_wages,
    Baseline_Welfare = baseline_welfare,
    Counterfactual_Wage = counterfactual_wages,
    Counterfactual_Welfare = counterfactual_welfare,
    Welfare_Change_Percent = welfare_change
  )
  
  return(results_df)
}

###############################################################################
# Compatibility functions for CDK methodology
###############################################################################

#' Format results table for main analysis
#' @param baseline_results Baseline equilibrium results
#' @param counterfactual_results Counterfactual results
#' @param countries Vector of country names
#' @return Formatted data frame for tables
format_results_table <- function(baseline_results, counterfactual_results, countries) {
  
  baseline_welfare <- baseline_results$equilibrium$welfare
  counterfactual_welfare <- counterfactual_results$equilibrium$welfare
  baseline_wages <- baseline_results$equilibrium$wages
  counterfactual_wages <- counterfactual_results$equilibrium$wages
  
  welfare_change <- (counterfactual_welfare - baseline_welfare) / baseline_welfare * 100
  
  results_df <- data.frame(
    Country = countries,
    Baseline_Wage = baseline_wages,
    Baseline_Welfare = baseline_welfare,
    Counterfactual_Wage = counterfactual_wages,
    Counterfactual_Welfare = counterfactual_welfare,
    Welfare_Change_Percent = welfare_change
  )
  
  return(results_df)
}

#' Compare results across scenarios  
#' @param main_results Main Trump scenario results
#' @param robustness_results Alternative scenario results
#' @param countries Vector of country names
#' @return Comparison table
compare_scenarios <- function(main_results, robustness_results, countries) {
  
  # Extract welfare changes from main results
  main_changes <- main_results$welfare_table$Welfare_Change_Percent
  
  comparison_df <- data.frame(
    Country = countries,
    Main_Scenario = main_changes
  )
  
  return(comparison_df)
}

#' Generate summary statistics and tables
#' @param welfare_analysis CDK welfare analysis results
#' @param countries Vector of country names
#' @return Formatted CDK summary tables
generate_summary_tables <- function(welfare_analysis, countries) {
  
  welfare_table <- welfare_analysis$welfare_table
  
  # Sort by welfare change
  welfare_table <- welfare_table[order(welfare_table$Welfare_Change_Percent), ]
  
  # Top gainers and losers
  top_gainers <- head(welfare_table[order(-welfare_table$Welfare_Change_Percent), ], 5)
  top_losers <- head(welfare_table[order(welfare_table$Welfare_Change_Percent), ], 5)
  
  return(list(
    full_table = welfare_table,
    top_gainers = top_gainers,
    top_losers = top_losers,
    summary_stats = welfare_analysis$summary_stats
  ))
}

#' Run robustness analysis with multiple tariff scenarios
#' @param baseline_results Baseline equilibrium
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @param theta Trade elasticity
#' @return Results from multiple scenarios
run_robustness_analysis <- function(baseline_results, countries, sectors, theta_vec) {
  
  # For now, return empty robustness results to avoid errors
  # This function can be expanded later if needed
  robustness_results <- list()
  
  return(robustness_results)
}
run_robustness_analysis <- function(baseline_results, countries, sectors, theta_vec) {
  

  
  scenarios <- c("moderate", "aggressive", "targeted")
  robustness_results <- list()
  
  for (scenario in scenarios) {
    # Running scenario
    
    # Create tariff matrix for this scenario
    tariff_array <- create_alternative_tariff_scenario(countries, sectors, scenario)
    
    # Compute counterfactual
    counterfactual <- compute_counterfactual_equilibrium_cdk(baseline_results, tariff_array)
    
    # Analyze welfare effects
    welfare_analysis <- analyze_welfare_effects_cdk(baseline_results, counterfactual, countries)
    
    robustness_results[[scenario]] <- list(
      tariffs = tariff_array,
      counterfactual = counterfactual,
      welfare = welfare_analysis
    )
  }
  

  
  return(robustness_results)
}

#' Compatibility wrapper for check_model_fit
check_model_fit <- function(baseline_results, observed_data) {
  

  
  # Compare predicted vs observed trade shares
  predicted_shares <- baseline_results$equilibrium$trade_shares
  observed_shares <- observed_data$trade_shares
  
  # Calculate correlation
  if (is.array(predicted_shares) && is.array(observed_shares)) {
    correlation <- cor(as.vector(predicted_shares), as.vector(observed_shares), use = "complete.obs")
    rmse <- sqrt(mean((as.vector(predicted_shares) - as.vector(observed_shares))^2, na.rm = TRUE))
  } else {
    correlation <- NA
    rmse <- NA
    # Warning: Cannot compute model fit - incompatible data structures
  }
  
  # Compare GDP per capita if available
  gdp_correlation <- NA
  if (!is.null(observed_data$gdp_per_capita)) {
    model_gdp <- baseline_results$equilibrium$welfare
    gdp_correlation <- cor(model_gdp, observed_data$gdp_per_capita, use = "complete.obs")
  }
  
  fit_measures <- list(
    trade_share_correlation = correlation,
    trade_share_rmse = rmse,
    gdp_per_capita_correlation = gdp_correlation
  )
  
  if (!is.na(correlation)) {
    # Trade share correlation and RMSE available
  }
  if (!is.na(gdp_correlation)) {
    # GDP per capita correlation available
  }
  
  return(fit_measures)
}