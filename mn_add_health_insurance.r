dat_mn <- dat_mn %>% mutate(
  #add overall health insurance
  health_insurance = case_when(
    MWB18 == 2 ~ FALSE,
    MWB18 == 1 ~ TRUE,
    MWB18 == 9 ~ NA,
  ),
  # ins_sub_community = ifelse(health_insurance == TRUE, UB10A == "A", health_insurance),
  # ins_sub_employer = ifelse(health_insurance == TRUE, UB10B == "B", health_insurance),
  # ins_sub_employer = ifelse(health_insurance == TRUE, UB10B == "B", health_insurance),
)