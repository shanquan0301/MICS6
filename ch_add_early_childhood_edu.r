#early childhood education--------------
dat_ch <- dat_ch %>% mutate(
  #Ever attended early childhood education programme
  early_edu_1 = case_when(
    UB6 == 1 ~ TRUE,
    UB6 == 2 ~ FALSE,
    TRUE ~ NA
  ),
  #Attended early childhood education programme anytime since the beginning of current school year
  early_edu_3 = case_when(
    UB7 == 1 ~ TRUE,
    UB7 == 2 ~ FALSE,
    TRUE ~ NA
  ),
  #Currently attending early childhood education programme
  early_edu_4 = case_when(
    UB8 == 1 ~ TRUE,
    UB8 == 2 ~ FALSE,
    TRUE ~ NA
  )
)

dat_ch <- dat_ch %>% mutate(
  early_edu_ever = early_edu_1,
  early_edu_current = mapply(any, early_edu_3, early_edu_4)
)