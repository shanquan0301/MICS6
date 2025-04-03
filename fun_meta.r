fun_meta_data <- function(data = res_table, 
                          N_threshold = 25,#N_EXP >= N_threshold, n_EXP >= n_threshold
                          n_threshold = 0,
                          show_unadj = TRUE,
                          ...){
  #add region and country name-----
  data <- left_join(data, regions %>% select(country = name, iso3, unicef_reg),
                    by = c("country_iso3" = "iso3"))
  data <- data %>% ungroup()
  #transfer the data, generate cat, adjust----------
  str_name <- names(data %>% select(-contains("Risk"), -contains("Prev"), -contains("cases")))
  
  data_1 <- data %>% select(all_of(str_name), starts_with("cRisk_ratio"))
  
  names(data_1) <- c(str_name, "value", "value_low", "value_high", "eff", "sei")
  data_1$cat <- "cRisk_ratio"
  
  data_2 <- data %>% select(all_of(str_name), starts_with("aRisk_ratio"))
  names(data_2) <- c(str_name, "value", "value_low", "value_high", "eff", "sei")
  data_2$cat <- "aRisk_ratio"
  
  data_3 <- data %>% select(all_of(str_name), starts_with("cRisk_difference"))
  names(data_3) <- c(str_name, "value", "value_low", "value_high", "eff", "sei")
  data_3$cat <- "cRisk_difference"
  
  data_4 <- data %>% select(all_of(str_name), starts_with("aRisk_difference"))
  names(data_4) <- c(str_name, "value", "value_low", "value_high", "eff", "sei")
  data_4$cat <- "aRisk_difference"
  
  data <- bind_rows(data_1, data_2, data_3, data_4)
  
  data <- data %>% rowwise() %>% mutate(
    comb = str_glue("{value} [{value_low}, {value_high}]")
  )
  
  #data for meta-----------
  #browser()
  dat_meta <- data %>% filter(N_EXP >= N_threshold, n_EXP >= n_threshold)
  dat_meta <- dat_meta %>% mutate(
    adjusted = if_else(str_sub(cat, 1, 1) == "c", FALSE, TRUE),
    cat = str_sub(cat, 2, -1),
    eff = as.numeric(eff),
    sei = as.numeric(sei)
  )
  
  if(show_unadj != TRUE){
    dat_meta <- dat_meta %>% filter(adjusted == TRUE)
  }
  return(dat_meta)
}

fun_meta <- function(data,
                     show_data = FALSE, ...){
  #data <- fun_meta_data(data = data)
  #meta------------------
  print(str_c(unique(data$outcome), "----------"))
  print("rr vs rd")
  if (unique(data$cat) == "Risk_ratio"){
    res_ran <- rma(yi = eff, sei = sei, 
                   measure = "RR",
                   data = data, 
                   control=list(stepadj=0.5, maxiter=10000),
                   method="REML")
    res_fix <- rma(yi = eff, sei = sei, 
                   measure = "RR",
                   data = data, 
                   control=list(stepadj=0.5, maxiter=10000),
                   method="FE")
  }
  
  if (unique(data$cat) == "Risk_difference"){
    res_ran <- rma(yi = eff, sei = sei, 
                   measure = "RD",
                   data = data, 
                   control=list(stepadj=0.5, maxiter=10000),
                   method="REML")
    res_fix <- rma(yi = eff, sei = sei, 
                   measure = "RD",
                   data = data, 
                   control=list(stepadj=0.5, maxiter=10000),
                   method="FE")
  }
  
  #browser()
  
  tau_square <- sprintf(str_glue("%.2f"), round(res_ran$tau2, 2))
  tau_square <- ifelse(tau_square == "0.00", "<0.01", str_c("=", tau_square))
  chi_square <- sprintf(str_glue("%.2f"), round(res_ran$QE, 2))
  df <- res_ran$k - res_ran$p
  p_value_1 <- sprintf(str_glue("%.3f"), round(res_ran$QEp, 3))
  p_value_1 <- ifelse(p_value_1  == "0.000", "<0.001", str_c("=", p_value_1))
  i_square <- sprintf(str_glue("%.0f"), round(res_ran$I2, 0))
  i_square <- str_c("=", i_square, "%")

  #extract-----
  print("extract--")
  #browser()
  round_dig <- str_length(str_split(data$value[1], pattern = "\\.")[[1]][2])
  if(res_ran$QEp < 0.1){
    method <- "Random effect"
    
    z_value <- sprintf(str_glue("%.2f"), round(res_ran$zval, 2))
    p_value_2 <- sprintf(str_glue("%.3f"), round(res_ran$pval, 3))
    p_value_2 <- ifelse(p_value_2  == "0.000", "<0.001", str_c("=", p_value_2))
      
    
    overall <- res_ran$b
    overall_low <- res_ran$ci.lb
    overall_high <- res_ran$ci.ub
  }
  
  if(res_ran$QEp >= 0.1){
    method <- "Fixed effect"
    
    z_value <- sprintf(str_glue("%.2f"), round(res_fix$zval, 2))
    p_value_2 <- sprintf(str_glue("%.3f"), round(res_fix$pval, 3))
    p_value_2 <- ifelse(p_value_2  == "0.000", "<0.001", str_c("=", p_value_2))
    
    overall <- res_fix$b
    overall_low <- res_fix$ci.lb
    overall_high <- res_fix$ci.ub
  }
  
  if (unique(data$cat) == "Risk_ratio"){
    mid_overall <- sprintf(str_glue("%.{round_dig}f"), round(exp(overall), round_dig))
    mid_overall_low <- sprintf(str_glue("%.{round_dig}f"), round(exp(overall_low), round_dig))
    mid_overall_high <- sprintf(str_glue("%.{round_dig}f"), round(exp(overall_high), round_dig))
    overall_comb <- str_glue("{mid_overall} [{mid_overall_low}, {mid_overall_high}]")
  }
  
  if (unique(data$cat) == "Risk_difference"){
    mid_overall <- sprintf(str_glue("%.{round_dig}f"), round(overall*100, round_dig))
    mid_overall_low <- sprintf(str_glue("%.{round_dig}f"), round(overall_low*100, round_dig))
    mid_overall_high <- sprintf(str_glue("%.{round_dig}f"), round(overall_high*100, round_dig))
    overall_comb <- str_glue("{mid_overall} [{mid_overall_low}, {mid_overall_high}]")
  }
  
  # if (unique(data$cat) == "Risk_ratio"){
  #   res_meta <- data.frame(
  #     method = method, 
  #     tau_square = tau_square,
  #     chi_square = chi_square,
  #     df = df,
  #     p_value_1 = p_value_1,
  #     i_square = i_square,
  #     z_value = z_value,
  #     p_value_2 = p_value_2,
  #     overall = overall,
  #     overall_low = overall_low,
  #     overall_high = overall_high,
  #     overall_comb = overall_comb)
  # }
  
  # if (unique(data$cat) == "Risk_difference"){
  #   res_meta <- data.frame(
  #     method = method, 
  #     tau_square = tau_square,
  #     chi_square = chi_square,
  #     df = df,
  #     p_value_1 = p_value_1,
  #     i_square = i_square,
  #     z_value = z_value,
  #     p_value_2 = p_value_2,
  #     overall = overall*100,
  #     overall_low = overall_low*100,
  #     overall_high = overall_high*100,
  #     overall_comb = overall_comb)
  # }
    res_meta <- data.frame(
      method = method,
      tau_square = tau_square,
      chi_square = chi_square,
      df = df,
      p_value_1 = p_value_1,
      i_square = i_square,
      z_value = z_value,
      p_value_2 = p_value_2,
      overall = overall,
      overall_low = overall_low,
      overall_high = overall_high,
      overall_comb = overall_comb)
  #browser()
  if(show_data == TRUE){
    return(list(meta_table = data,
                res_meta = res_meta))
  } else {
    return(res_meta)
  }
}