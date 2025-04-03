#add Know all means of MTCT - including drug risk reduction
dat_mn <- dat_mn %>%
  mutate(across(c(MHA8A, MHA8B, MHA8C, MHA10), ~ ifelse(.x > 8, NA, .x)))

dat_mn <- dat_mn %>% mutate(
  mtct3 = mapply(all, MHA8A == 1, MHA8B == 1, MHA8C == 1, MHA10 == 1)
)

#consider the missing value bias becaue of HA1----------
dat_mn <- dat_mn %>% mutate(
  mtct3 = case_when(
    is.na(mtct3) & MHA1 == 2 ~ FALSE,
    TRUE ~ mtct3
  )
)