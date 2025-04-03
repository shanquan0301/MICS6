#add use of hiv service----------------------
#for hax >= 8 is missing value-------
dat_wm <- dat_wm %>%
  mutate(across(c(HA13A, HA13B, HA13C, HA13D, HA14, HA15, HA19, HA20, HA22, HA23, HA24, HA25, HA26), 
                ~ ifelse(.x >= 8, NA, .x)))

dat_wm <- dat_wm %>% mutate(
  #HIV counselling during ANC------
  ANCcounsel = mapply(all, HA13A == 1, HA13B == 1, HA13C == 1),
  #Offered HIV testing during ANC----
  offeredANCtest = HA13D == 1,
  #HIV testing during ANC---
  ANCtest = mapply(all, HA14 == 1, HA15 == 1),
  #HIV test at delivery and know result----
  test_atdelivery = mapply(all, HA19 == 1, HA20 == 1),
  #HIV test since birth and know result---
  test_postbirth = mapply(all, HA22 == 1, HA26 == 1),
  #HIV test and know result, but no birth---
  test_no_bh = mapply(all, HA24 == 1, HA26 == 1)
)

#consider the missing value bias becaue of HA1----------
dat_wm <- dat_wm %>% mutate(
  ANCcounsel = case_when(
    is.na(ANCcounsel) & HA1 == 2 ~ FALSE,
    TRUE ~ ANCcounsel
  ),
  offeredANCtest = case_when(
    is.na(offeredANCtest) & HA1 == 2 ~ FALSE,
    TRUE ~ offeredANCtest
  ),
  ANCtest = case_when(
    is.na(ANCtest) & HA1 == 2 ~ FALSE,
    TRUE ~ ANCtest
  ),
  test_atdelivery = case_when(
    is.na(test_atdelivery) & HA1 == 2 ~ FALSE,
    TRUE ~ test_atdelivery
  ),
  test_postbirth = case_when(
    is.na(test_postbirth) & HA1 == 2 ~ FALSE,
    TRUE ~ test_postbirth
  ),
  test_no_bh = case_when(
    is.na( test_no_bh) & HA1 == 2 ~ FALSE,
    TRUE ~  test_no_bh
  )
)

#get the ever test and test in the past 12 months----------------
#get test in the past 12 months----------------
dat_wm <- dat_wm %>% mutate(
  #Ever been tested for HIV and know the result----
  evertest = mapply(any_by_row, ANCtest == TRUE, test_atdelivery == TRUE, test_postbirth == TRUE, test_no_bh == TRUE),
  #Tested in the last 12 months and know the results---
  test12mo_1_1 = HA23 == 1,
  test12mo_1_2 = mapply(any, HA15 == 1, HA20 == 1),
  test12mo_2 = mapply(all, HA25 == 1, HA26 == 1)
)

dat_wm <- dat_wm %>% mutate(
  test12mo_1 = mapply(all, test12mo_1_1 == TRUE, test12mo_1_2 == TRUE)
)
dat_wm <- dat_wm %>% mutate(
  test12mo = mapply(any_by_row, test12mo_1 == TRUE, test12mo_2 == TRUE)
)

dat_wm <- dat_wm %>% mutate(
  test12mo = case_when(
    is.na(test12mo) & HA1 == 2 ~ FALSE,
    TRUE ~ test12mo
  )
)



