#Know of at least a place to get tested for HIV 
dat_wm <- dat_wm %>%
  mutate(across(c(HA14, HA19, HA22, HA24, HA27), ~ ifelse(.x > 8, NA, .x)))

dat_wm <- dat_wm %>% mutate(
  testknow = mapply(any_by_row, HA14 == 1, HA19 == 1, HA22 == 1, HA24 == 1, HA27 == 1)
)

#consider the missing value bias becaue of HA1----------
dat_wm <- dat_wm %>% mutate(
  testknow = case_when(
    is.na(testknow) & HA1 == 2 ~ FALSE,
    TRUE ~ testknow
  )
)
