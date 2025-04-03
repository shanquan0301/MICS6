dat_wm <- dat_wm %>% mutate(
  #add overall health insurance
  health_insurance = case_when(
    WB18 == 2 ~ FALSE,
    WB18 == 1 ~ TRUE,
    WB18 == 9 ~ NA,
  ),
  # ins_sub_community = ifelse(health_insurance == TRUE, UB10A == "A", health_insurance),
  # ins_sub_employer = ifelse(health_insurance == TRUE, UB10B == "B", health_insurance),
  # ins_sub_employer = ifelse(health_insurance == TRUE, UB10B == "B", health_insurance),
)