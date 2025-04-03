#add HIV variables-----------
#for hax >8 is missing value, 8 was treated as not known
dat_mn <- dat_mn %>%
  mutate(across(c(MHA2, MHA3, MHA4, MHA5, MHA6, MHA7), ~ ifelse(.x > 8, NA, .x)))

dat_mn <- dat_mn %>% mutate(
  #number of missing
  hiv_na = mapply(sum_by_row, is.na(MHA2), is.na(MHA3), is.na(MHA4), is.na(MHA5), is.na(MHA6), is.na(MHA7)),
  #number with correct answer
  hiv_num = mapply(sum_by_row, MHA2 == 1, MHA3 == 2, MHA4 == 1, MHA5 == 2, MHA6 == 2, MHA7 == 1)
)

dat_mn <- dat_mn %>% mutate(
  HIVknow = case_when(
    hiv_num == 6 ~ TRUE,
    hiv_num + hiv_na == 6 ~ NA,
    hiv_num + hiv_na < 6 ~ FALSE,
  ))

#consider the missing value bias becaue of HA1----------
dat_mn <- dat_mn %>% mutate(
  HIVknow = case_when(
    is.na(HIVknow) & MHA1 == 2 ~ FALSE,
    TRUE ~ HIVknow
  )
)