#add age, gender, birth, wealth, urban-------
dat_ch <- dat_ch %>% mutate(
  windex5 = ifelse(is.na(windex5), windex5MICS, windex5),
  windex5u = ifelse(is.na(windex5uMICS), windex5uMICS, windex5u),
  windex5r = ifelse(is.na(windex5r), windex5rMICS, windex5r),
)

dat_ch <- dat_ch %>% mutate(
  age = ifelse(is.na(UB2), AN4, UB2),
  female = HL4 == 2,
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

dat_ch <- dat_ch %>% mutate(
  urban = ifelse(is.na(urban_1), urban_2, urban_1)
)