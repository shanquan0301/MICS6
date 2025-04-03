#add use of hiv service----------------------
#for hax >= 8 is missing value-------
dat_mn <- dat_mn %>%
  mutate(across(c(MHA24, MHA25, MHA26), 
                ~ ifelse(.x >= 8, NA, .x)))


dat_mn <- dat_mn %>% mutate(
  #Ever been tested for HIV and know the result----
  evertest = mapply(all, MHA24 == 1, MHA26 == 1),
  #Tested in the last 12 months and know the results---
  test12mo = mapply(all, MHA25 == 1, MHA26 == 1)
)

#consider the missing value bias becaue of HA1----------
dat_mn <- dat_mn %>% mutate(
  evertest = case_when(
    is.na(evertest) & MHA1 == 2 ~ FALSE,
    TRUE ~ evertest
  ),
  test12mo = case_when(
    is.na(test12mo) & MHA1 == 2 ~ FALSE,
    TRUE ~ test12mo
  )
)




