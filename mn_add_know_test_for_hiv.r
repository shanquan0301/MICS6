#Know of a place to get tested for HIV 
dat_mn <- dat_mn %>%
  mutate(across(c(MHA24, MHA27), ~ ifelse(.x > 8, NA, .x)))

dat_mn <- dat_mn %>% mutate(
  testknow = mapply(any_by_row, MHA24 == 1, MHA27 == 1)
)

#consider the missing value bias becaue of HA1----------
dat_mn <- dat_mn %>% mutate(
  testknow = case_when(
    is.na(testknow) & MHA1 == 2 ~ FALSE,
    TRUE ~ testknow
  )
)
