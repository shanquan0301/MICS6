#add age, gender, birth, wealth, urban, FCD(parents attitude)-------
dat_fs <- dat_fs %>% mutate(
  age = CB3,
  female = HL4 == 2,
  birth_year = CB2Y,
  birth_month = CB2M,
  #birth_day = UB1D,
  wealth = windex5,
  windex5r = ifelse(is.na(windex5r),
                    ifelse(is.na(windex5rc), windex5ri, windex5rc),
                    windex5r),
  wealth_10 = windex10,
  urban_1 = case_when(
    HH6 == 1 ~ TRUE,
    HH6 == 2 ~ FALSE,
    TRUE ~ NA
  ),
  urban_2 = ifelse(is.na(windex5u), !is.na(windex5r), !is.na(windex5u)))

dat_fs <- dat_fs %>% mutate(
  urban = ifelse(is.na(urban_1), urban_2, urban_1))

#for FCD2, 9 is missing value
dat_fs <- dat_fs %>% mutate(
  across(starts_with("FCD2"), ~ ifelse(.x > 5, NA, .x == 1)),
  across(starts_with("CL"), ~ ifelse(.x == 99, NA, .x)),
)