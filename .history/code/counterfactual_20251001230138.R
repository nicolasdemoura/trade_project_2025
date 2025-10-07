###############################################################################
# Counterfactual Analysis: Trump Tariff Impact using CDK (2012) Methodology
# Author: Nícolas de Moura (<nicolasgoulartdemoura@gmail.com>)
# Date: 2025-10-01
###############################################################################

# Source required functions
source("functions.R")

###############################################################################
# Define Trump Tariff Scenarios for CDK (2012) Model
###############################################################################

#' Create Trump tariff scenario matrix for CDK (2012) analysis
#' @param countries Vector of country names
#' @param sectors Vector of sector names
#' @param baseline_year Base year for analysis
#' @return Array of tariff rates (origin x destination x sector)
create_trump_tariff_scenario_cdk <- function(countries, sectors, baseline_year = 2011) {
  
  cat("Creating Trump tariff scenario...\n")
  
  n_countries <- length(countries)
  
  if (is.null(sectors)) {
    # Single sector case
    tariff_matrix <- matrix(0, n_countries, n_countries,
                           dimnames = list(countries, countries))
  } else {
    # Multi-sector case
    n_sectors <- length(sectors)
    tariff_matrix <- array(0, dim = c(n_countries, n_countries, n_sectors),
                          dimnames = list(countries, countries, sectors))
  }
  
  # Identify US in country list
  us_index <- which(toupper(countries) %in% c("USA", "US", "UNITED STATES", "AMERICA"))
  if (length(us_index) == 0) {
    # Try alternative names
    us_index <- which(grepl("US|AMERICA", toupper(countries)))
  }
  
  if (length(us_index) == 0) {
    warning("Could not identify United States in country list")
    cat("Available countries:", paste(countries, collapse = ", "), "\n")
    return(tariff_matrix)
  }
  
  us_idx <- us_index[1]  # Take first match
  cat("Identified US as country index:", us_idx, "(", countries[us_idx], ")\n")
  
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
      cat("Applied 60% tariff on China (", countries[china_idx], ") -> US\n")
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
      cat("Applied 60% tariff on China (", countries[china_index[1]], ") -> US across all sectors\n")
    }
  }
  
  cat("Applied", universal_tariff * 100, "% universal tariff on imports to US\n")
  
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
  
  cat("Computing CDK (2012) counterfactual equilibrium with tariffs...\n")
  
  # Extract baseline parameters
  baseline_tau <- baseline_results$calibrated_trade_costs
  T_matrix <- baseline_results$pure_technology
  theta_vec <- baseline_results$theta_vec
  beta_vec <- baseline_results$beta_vec
  labor_matrix <- baseline_results$labor_matrix
  
  # Apply tariffs to baseline trade costs
  # New trade costs = baseline_costs * (1 + tariff_rate)
  new_tau <- baseline_tau * (1 + tariff_array)
  
  cat("Applied tariffs. Computing new equilibrium...\n")
  
  # Solve CDK equilibrium with new trade costs
  counterfactual_equilibrium <- solve_equilibrium_cdk(theta_vec, T_matrix, new_tau, 
                                                     labor_matrix, beta_vec,
                                                     max_iter = max_iter, tol = tolerance)
  
  cat("CDK counterfactual equilibrium computed successfully.\n")
  
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

#' Calculate detailed welfare effects
#' @param baseline_results Baseline equilibrium
#' @param counterfactual_results Counterfactual equilibrium  
#' @param countries Vector of country names
#' @return Detailed welfare analysis results
analyze_welfare_effects <- function(baseline_results, counterfactual_results, countries) {
  
  cat("Analyzing welfare effects...\n")
  
  baseline_welfare <- baseline_results$equilibrium$welfare
  counterfactual_welfare <- counterfactual_results$equilibrium$welfare
  
  # Calculate welfare changes
  welfare_changes <- calculate_welfare_changes(baseline_welfare, counterfactual_welfare)
  
  # Decompose welfare changes into price and income effects
  baseline_wages <- baseline_results$equilibrium$wages
  baseline_prices <- baseline_results$equilibrium$price_indices
  counterfactual_wages <- counterfactual_results$equilibrium$wages  
  counterfactual_prices <- counterfactual_results$equilibrium$price_indices
  
  # Real wage decomposition: w/P = w * P^(-1)
  # % change in welfare ≈ % change in wages - % change in prices
  wage_change <- (counterfactual_wages - baseline_wages) / baseline_wages * 100
  price_change <- (counterfactual_prices - baseline_prices) / baseline_prices * 100
  
  # Create results data frame
  welfare_df <- data.frame(
    Country = countries,
    Baseline_Welfare = baseline_welfare,
    Counterfactual_Welfare = counterfactual_welfare,
    Welfare_Change_Percent = welfare_changes,
    Wage_Change_Percent = wage_change,
    Price_Change_Percent = price_change,
    stringsAsFactors = FALSE
  )
  
  # Summary statistics
  global_welfare_change <- sum(welfare_changes * baseline_welfare) / sum(baseline_welfare)
  
  # Identify winners and losers
  winners <- countries[welfare_changes > 0]
  losers <- countries[welfare_changes < 0]
  
  cat("Global welfare change:", round(global_welfare_change, 2), "%\n")
  cat("Countries that gain:", paste(winners, collapse = ", "), "\n")
  cat("Countries that lose:", paste(losers, collapse = ", "), "\n")
  
  return(list(
    welfare_table = welfare_df,
    global_change = global_welfare_change,
    winners = winners,
    losers = losers,
    summary_stats = list(
      mean_change = mean(welfare_changes),
      median_change = median(welfare_changes),
      max_gain = max(welfare_changes),
      max_loss = min(welfare_changes)
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
  
  cat("Running robustness analysis with multiple scenarios...\n")
  
  scenarios <- c("moderate", "aggressive", "targeted")
  robustness_results <- list()
  
  for (scenario in scenarios) {
    cat("Running scenario:", scenario, "\n")
    
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
  
  cat("Robustness analysis completed.\n")
  
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

#' Generate summary statistics and tables
#' @param welfare_analysis Welfare analysis results
#' @param countries Vector of country names
#' @return Formatted summary tables
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