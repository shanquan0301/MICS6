#Early Childhood Education-------
dat_hl <- dat_hl %>% mutate(
  #Ever attended school or any Early Childhood Education programme
  early_edu_1 = case_when(
    ED4 == 1 ~ TRUE,
    ED4 == 2 ~ FALSE,
    TRUE ~ NA
  ),
  #Check: Ever attended school or any Early Childhood Education programme
  early_edu_2 = case_when(
    ED8 == 1 ~ TRUE,
    ED8 == 2 ~ FALSE,
    TRUE ~ NA
  ),
  #Attended school during current school year
  early_edu_3 = case_when(
    ED9 == 1 ~ TRUE,
    ED9 == 2 ~ FALSE,
    TRUE ~ NA
  )
)

dat_hl <- dat_hl %>% mutate(
  early_edu_ever = mapply(any, early_edu_1, early_edu_2),
  early_edu_current = early_edu_3
)
