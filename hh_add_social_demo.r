#add wealth, urban-------
dat_hh <- dat_hh %>% mutate(
  windex5 = ifelse(is.na(windex5), windex5MICS, windex5),
  windex5u = ifelse(is.na(windex5uMICS), windex5uMICS, windex5u),
  windex5r = ifelse(is.na(windex5r), windex5rMICS, windex5r),
  windex10 = ifelse(is.na(windex10), windex10MICS, windex10),
  windex10u = ifelse(is.na(windex10uMICS), windex10uMICS, windex10u),
  windex10r = ifelse(is.na(windex10r), windex10rMICS, windex10r)
)

dat_hh <- dat_hh %>% mutate(
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

dat_hh <- dat_hh %>% mutate(
  urban = ifelse(is.na(urban_1), urban_2, urban_1)
)