#add age, gender, birth, wealth, urban-------
dat_mn <- dat_mn %>% mutate(
  age = MWB4,
  female = FALSE,
  birth_year = MWB3Y,
  birth_month = MWB3M,
  #birth_day = WB3D,
  wealth = windex5,
  windex5r = ifelse(is.na(windex5r),
                    ifelse(is.na(windex5rc), windex5ri, windex5rc),
                    windex5r),
  urban_1 = case_when(
    HH6 == 1 ~ TRUE,
    HH6 == 2 ~ FALSE,
    TRUE ~ NA
  ),
  urban_2 = ifelse(is.na(windex5u), !is.na(windex5r), !is.na(windex5u)))

dat_mn <- dat_mn %>% mutate(
  urban = ifelse(is.na(urban_1), urban_2, urban_1))