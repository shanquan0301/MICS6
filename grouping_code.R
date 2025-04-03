library(dplyr)

run_analysis <- function(group_var, exposure_var, intervention_var) {
  dat_grouped <- dat_ts
  
  # If a grouping variable is specified, group by it; otherwise, use the entire dataset
  if (!is.null(group_var)) {
    dat_grouped <- dat_grouped %>% group_by({{group_var}})
  }
  
  res <- dat_grouped %>%
    do(fun_desc(data = .,
                exposure = exposure_var, 
                intervention = intervention_var,
                round_dig = 2,
                covars = c("age", "female", if (is.null(group_var)) {{ group_var }} else NULL)))
  
  # Select only relevant variables for the final table
  res_select <- res %>%
    select({{ group_var }}, N_all, Prev_all, Prev_noEXP, Prev_EXP, aRisk_ratio, aRisk_ratio_low, aRisk_ratio_high) %>%
    ungroup()  # Remove grouping before returning the result
  
  return(res_select)
}

# Example usage:
# To analyze "disability" and "health_insurance" without grouping
res_overall <- run_analysis(group_var = NULL, exposure_var = "disability", intervention_var = "health_insurance")

# To analyze "disability" and "health_insurance" grouped by "country_full"
res_country <- run_analysis(group_var = "country_full", exposure_var = "disability", intervention_var = "health_insurance")

# To analyze "disability" and "health_insurance" grouped by "Income_Group"
res_income <- run_analysis(group_var = "Income_Group", exposure_var = "disability", intervention_var = "health_insurance")
