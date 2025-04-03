#add HIV variables-----------
#for hax >8 is missing value, 8 was treated as not known
dat_wm <- dat_wm %>%
  mutate(across(c(HA2, HA3, HA4, HA5, HA6, HA7), ~ ifelse(.x > 8, NA, .x)))

dat_wm <- dat_wm %>% mutate(
  #number of missing
  hiv_na = mapply(sum_by_row, is.na(HA2), is.na(HA3), is.na(HA4), is.na(HA5), is.na(HA6), is.na(HA7)),
  #number with correct answer
  hiv_num = mapply(sum_by_row, HA2 == 1, HA3 == 2, HA4 == 1, HA5 == 2, HA6 == 2, HA7 == 1)
)

dat_wm <- dat_wm %>% mutate(
  HIVknow = case_when(
    hiv_num == 6 ~ TRUE,
    hiv_num + hiv_na == 6 ~ NA,
    hiv_num + hiv_na < 6 ~ FALSE,
  )
)

#consider the missing value bias becaue of HA1----------
dat_wm <- dat_wm %>% mutate(
  HIVknow = case_when(
    is.na(HIVknow) & HA1 == 2 ~ FALSE,
    TRUE ~ HIVknow
  )
)




