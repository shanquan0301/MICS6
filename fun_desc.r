#function----------------------------------------------
fun_desc <- function(data,
                     exposure = "disability",
                     intervention = "ANCcounsel",
                     covars = c("age", "wealth", "urban"),
                     uid = "unique_hh_ln",
                     weight = "weight",
                     round_dig = 2,
                     level = 1){
  print(unique(data$country_full))
  cat(str_c(str_c(rep("-", times = level + 1), collapse = ""),
            glue_col("The exposure is {blue {exposure}} and outcome is {blue {intervention}})\n\n")))
  #generate data for description-------------
  mdat <- data %>% select(country_full, country_iso3,
                          all_of(c(uid, exposure, intervention, covars, weight)))
  
  mdat$exposure <- eval(parse(text = str_glue("mdat${exposure}")))
  mdat$intervention <- eval(parse(text = str_glue("mdat${intervention}")))
  mdat$uid <- eval(parse(text = str_glue("mdat${uid}")))
  mdat$weight <- eval(parse(text = str_glue("mdat${weight}")))
  
  #judge if dataset is empty after omitting the missing value
  dat_ana <- mdat %>% na.omit()
  if(nrow(dat_ana) <= 1){
    cat(str_c(str_c(rep("-", times = level + 3), collapse = ""),
              glue_col("{red No one have {intervention} in {unique(data$country_full)}}\n\n")))
    return(data.frame())
  }
  
  res <- dat_ana %$% table(exposure, intervention)
  if(length(res) <= 2){
    cat(str_c(str_c(rep("-", times = level + 3), collapse = ""),
              glue_col("{red {exposure}/Non-{exposure} have no {intervention} in {unique(data$country_full)}}\n\n")))
    return(data.frame())
  }
  
  
  my_design <- svydesign(ids = ~ uid, weights = ~ weight, data = dat_ana)
  n_total <- nrow(mdat)
  n_omit <- nrow(mdat) - nrow(dat_ana)
  cat(str_c(str_c(rep("-", times = level + 1), collapse = ""),
            glue_col("Omitted the cases ({yellow {n_omit} out of {n_total}}) with missing values on exposure: {blue {exposure}} or on intervention: {blue {intervention}})\n\n")))
  
  #overall description------------------
  cat(str_c(str_c(rep("-", times = level + 3), collapse = ""),
            "Overall description\n"))
  
  # all number of cases
  N_all <- nrow(dat_ana)
  # all number and prevalence of intervention
  n_all <- nrow(dat_ana %>% filter(intervention == TRUE))
  Prev_all <- n_all / N_all
  # weighted prevalence of intervention, and its confidence interval
  res <- svyciprop(~ intervention, design = my_design)
  wPrev_all <- res[1]
  wPrev_all_low <-  confint(res)[1]
  wPrev_all_high <- confint(res)[2]
  
  #description on those without disability ------------------
  cat(str_c(str_c(rep("-", times = level + 3), collapse = ""),
            "Description on those without exposure\n"))
  
  # all number of cases
  N_noEXP <- nrow(dat_ana %>% filter(exposure == FALSE))
  # all number and prevalence of intervention
  n_noEXP <- nrow(dat_ana %>% filter(exposure == FALSE, intervention == TRUE))
  Prev_noEXP <-  n_noEXP / N_noEXP
  # weighted prevalence of intervention, and its confidence interval
  res <- svyciprop(~ intervention, design = subset(my_design, exposure == FALSE))
  wPrev_noEXP <- res[1]
  wPrev_noEXP_low <-  confint(res)[1]
  wPrev_noEXP_high <- confint(res)[2]
  
  #description on those with disability------------------
  cat(str_c(str_c(rep("-", times = level + 3), collapse = ""),
            "Description on those with exposure\n"))
  
  # all number of cases
  N_EXP <- nrow(dat_ana %>% filter(exposure == TRUE))
  # all number and prevalence of intervention
  n_EXP <- nrow(dat_ana %>% filter(exposure == TRUE, intervention == TRUE)) 
  #n_EXP = O will lead to RR = 0
  Prev_EXP <- n_EXP / N_EXP
  # weighted prevalence of intervention, and its confidence interval
  res <- svyciprop(~ intervention, design = subset(my_design, exposure == TRUE))
  wPrev_EXP <- res[1]
  wPrev_EXP_low <- confint(res)[1]
  wPrev_EXP_high <- confint(res)[2]
  
  #get the crude risk ratio and risk difference--------
  # reference https://academic.oup.com/aje/article/189/6/508/5812650
  if(n_EXP == 0){
    set.seed(123)
    n_add <- sample(1:N_EXP, size = 1)
    dat_ana$intervention[dat_ana$exposure == TRUE][n_add] <- TRUE
    
    my_design <- svydesign(ids = ~ uid, 
                           weights = ~ weight, 
                           data = dat_ana)
  }
  cat(str_c(str_c(rep("-", times = level + 3), collapse = ""),
            "Get the crude risk ratio and risk difference\n"))
  
  res_rr <- svyglm(intervention ~ exposure,
                   design = my_design,
                   family = quasipoisson(link = "log"))
  
  cRisk_ratio <- exp(res_rr$coefficients[str_detect(names(res_rr$coefficients), "exposure")])
  cRisk_ratio_low <- exp(confint(res_rr)[, 1][str_detect(names(res_rr$coefficients), "exposure")])
  cRisk_ratio_high <- exp(confint(res_rr)[, 2][str_detect(names(res_rr$coefficients), "exposure")])
  cRisk_ratio_eff <- res_rr$coefficients[str_detect(names(res_rr$coefficients), "exposure")]
  cRisk_ratio_sei <- sqrt(diag(vcov(res_rr)))[str_detect(names(res_rr$coefficients), "exposure")]
  
  start_num <- rep(0.5, length(res_rr$coefficients))
  start_num <- str_c(start_num, collapse = ", ")
  start_num <- str_glue("c({start_num})")
  res_rd <- eval(parse(text = str_glue(
    "svyglm(intervention ~ exposure,
                   design = my_design,
                   start = {start_num},
                   family = quasipoisson(link = 'identity'))"
  )))
  # res_rd <- svyglm(intervention ~ exposure,
  #                  design = my_design,
  #                  family = quasipoisson(link = "identity"))
  cRisk_difference <- res_rd$coefficients[str_detect(names(res_rd$coefficients), "exposure")]
  cRisk_difference_low <- confint(res_rd)[, 1][str_detect(names(res_rd$coefficients), "exposure")]
  cRisk_difference_high <- confint(res_rd)[, 2][str_detect(names(res_rd$coefficients), "exposure")]
  cRisk_difference_eff <- res_rd$coefficients[str_detect(names(res_rd$coefficients), "exposure")]
  cRisk_difference_sei <- sqrt(diag(vcov(res_rd)))[str_detect(names(res_rd$coefficients), "exposure")]
  #get the adjusted risk ratio and risk difference--------
  # reference https://academic.oup.com/aje/article/189/6/508/5812650
  cat(str_c(str_c(rep("-", times = level + 3), collapse = ""),
            "Get the adjusted risk ratio and risk difference\n"))
  
  str_formula <- str_c("intervention ~ exposure", str_c(covars, collapse  = " + "), sep = " + ")
  res_rr <- svyglm(as.formula(str_formula),
                   design = my_design,
                   family = quasipoisson(link = "log"))
  
  aRisk_ratio <- exp(res_rr$coefficients[str_detect(names(res_rr$coefficients), "exposure")])
  aRisk_ratio_low <- exp(confint(res_rr)[, 1][str_detect(names(res_rr$coefficients), "exposure")])
  aRisk_ratio_high <- exp(confint(res_rr)[, 2][str_detect(names(res_rr$coefficients), "exposure")])
  aRisk_ratio_eff <- res_rr$coefficients[str_detect(names(res_rr$coefficients), "exposure")]
  aRisk_ratio_sei <- sqrt(diag(vcov(res_rr)))[str_detect(names(res_rr$coefficients), "exposure")]
  
  start_num <- rep(0.5, length(res_rr$coefficients))
  start_num <- str_c(start_num, collapse = ", ")
  start_num <- str_glue("c({start_num})")
  res_rd <- eval(parse(text = str_glue(
    "svyglm(as.formula(str_formula),
                   design = my_design,
                   start = {start_num},
                   family = quasipoisson(link = 'identity'))"
  )))
  aRisk_difference <- res_rd$coefficients[str_detect(names(res_rd$coefficients), "exposure")]
  aRisk_difference_low <- confint(res_rd)[, 1][str_detect(names(res_rd$coefficients), "exposure")]
  aRisk_difference_high <- confint(res_rd)[, 2][str_detect(names(res_rd$coefficients), "exposure")]
  aRisk_difference_eff <- res_rd$coefficients[str_detect(names(res_rd$coefficients), "exposure")]
  aRisk_difference_sei <- sqrt(diag(vcov(res_rd)))[str_detect(names(res_rd$coefficients), "exposure")]
  
  res <- data_frame(
    `total cases` = n_total,
    `omit_cases` = n_omit,
    N_all = N_all,
    n_all = n_all,
    Prev_all = Prev_all,
    wPrev_all = wPrev_all,
    wPrev_all_low = wPrev_all_low,
    wPrev_all_high = wPrev_all_high,
    
    N_noEXP = N_noEXP,
    n_noEXP = n_noEXP,
    Prev_noEXP = Prev_noEXP,
    wPrev_noEXP = wPrev_noEXP,
    wPrev_noEXP_low = wPrev_noEXP_low,
    wPrev_noEXP_high = wPrev_noEXP_high,
    
    N_EXP = N_EXP,
    n_EXP = n_EXP,
    Prev_EXP = Prev_EXP,
    wPrev_EXP = wPrev_EXP,
    wPrev_EXP_low = wPrev_EXP_low,
    wPrev_EXP_high = wPrev_EXP_high,
    
    cRisk_ratio = cRisk_ratio,
    cRisk_ratio_low = cRisk_ratio_low,
    cRisk_ratio_high = cRisk_ratio_high,
    aRisk_ratio = aRisk_ratio,
    aRisk_ratio_low = aRisk_ratio_low,
    aRisk_ratio_high = aRisk_ratio_high,
    cRisk_ratio_eff = cRisk_ratio_eff,
    cRisk_ratio_sei = cRisk_ratio_sei,
    aRisk_ratio_eff = aRisk_ratio_eff,
    aRisk_ratio_sei = aRisk_ratio_sei,
    
    cRisk_difference = cRisk_difference,
    cRisk_difference_low = cRisk_difference_low,
    cRisk_difference_high = cRisk_difference_high,
    aRisk_difference = aRisk_difference,
    aRisk_difference_low = aRisk_difference_low,
    aRisk_difference_high = aRisk_difference_high,
    cRisk_difference_eff = cRisk_difference_eff,
    cRisk_difference_sei = cRisk_difference_sei,
    aRisk_difference_eff = aRisk_difference_eff,
    aRisk_difference_sei = aRisk_difference_sei,
  )
  
  
  #transfer percentage, risk ratio, and risk difference into %
  name_perc <- names(res %>% select(contains(c("Prev", "Risk_difference"))))
  name_perc <- name_perc[!str_fun(name_perc, c("eff", "sei"))]
  name_ratio <- names(res %>% select(contains(c("Risk_ratio"))))
  name_ratio <- name_ratio[!str_fun(name_ratio, c("eff", "sei"))]
  res <- res %>% mutate(
    across(all_of(name_perc), 
           ~sprintf(str_glue("%.{round_dig}f"), round(.x * 100, round_dig))),
    across(all_of(name_ratio), 
           ~sprintf(str_glue("%.{round_dig}f"), round(.x, round_dig))),
  )
  #sprintf(str_glue("%.{round_dig}f"), round(.x * 100, round_dig))
  
  return(res)
}
