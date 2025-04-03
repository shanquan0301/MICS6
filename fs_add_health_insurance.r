dat_fs <- dat_fs %>% mutate(
  #add overall health insurance
  health_insurance = case_when(
    CB11 == 2 ~ FALSE,
    CB11 == 1 ~ TRUE,
    CB11 == 9 ~ NA,
  ),
  # ins_sub_community = ifelse(health_insurance == TRUE, UB10A == "A", health_insurance),
  # ins_sub_employer = ifelse(health_insurance == TRUE, UB10B == "B", health_insurance),
  # ins_sub_employer = ifelse(health_insurance == TRUE, UB10B == "B", health_insurance),
)