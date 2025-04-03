dat_ch <- dat_ch %>% mutate(
  #add overall health insurance
  health_insurance = case_when(
    UB9 == 2 ~ FALSE,
    UB9 == 1 ~ TRUE,
    UB9 == 9 ~ NA,
  ),
  # ins_sub_community = ifelse(health_insurance == TRUE, UB10A == "A", health_insurance),
  # ins_sub_employer = ifelse(health_insurance == TRUE, UB10B == "B", health_insurance),
  # ins_sub_employer = ifelse(health_insurance == TRUE, UB10B == "B", health_insurance),
)