#add Know all means of MTCT - including drug risk reduction
dat_wm <- dat_wm %>%
  mutate(across(c(HA8A, HA8B, HA8C, HA10), ~ ifelse(.x > 8, NA, .x)))

dat_wm <- dat_wm %>% mutate(
  mtct3 = mapply(all, HA8A == 1, HA8B == 1, HA8C == 1, HA10 == 1)
)

#consider the missing value bias becaue of HA1----------
dat_wm <- dat_wm %>% mutate(
  mtct3 = case_when(
    is.na(mtct3) & HA1 == 2 ~ FALSE,
    TRUE ~ mtct3
  )
)