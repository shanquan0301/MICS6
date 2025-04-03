#add age, gender, birth, wealth, urban-------
dat_hl <- dat_hl %>% mutate(
  windex5 = ifelse(is.na(windex5), windex5MICS, windex5),
  windex5u = ifelse(is.na(windex5uMICS), windex5uMICS, windex5u),
  windex5r = ifelse(is.na(windex5r), windex5rMICS, windex5r),
  windex10 = ifelse(is.na(windex10), windex10MICS, windex10),
  windex10u = ifelse(is.na(windex10uMICS), windex10uMICS, windex10u),
  windex10r = ifelse(is.na(windex10r), windex10rMICS, windex10r)
)

dat_hl <- dat_hl %>% mutate(
  age = ifelse(is.na(HL6) | HL6 >= 98, ED2A, HL6),
  female = HL4 == 2,
  birth_year = HL5Y,
  birth_month = HL5M,
  birth_day = HL5D,
  wealth = windex5,
  wealth_10 = windex10,
  
  windex5r = ifelse(is.na(windex5r),
                    ifelse(is.na(windex5rc), windex5ri, windex5rc),
                    windex5r),
  urban_1 = case_when(
    HH6 == 1 ~ TRUE,
    HH6 == 2 ~ FALSE,
    TRUE ~ NA
  ),
  urban_2 = ifelse(is.na(windex5u), !is.na(windex5r), !is.na(windex5u)))

dat_hl <- dat_hl %>% mutate(
  urban = ifelse(is.na(urban_1), urban_2, urban_1)
)