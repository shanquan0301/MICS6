#add education variables 

dat_hl <- dat_hl %>% mutate(
  
primary_hl = case_when(
  ED10A == 0 & age > 4 & age < 18 ~ FALSE, 
  ED10A > 1 ~ TRUE), 

lower_sec_hl = case_when(
  ED10A %in% c(0,1) & age > 4 & age < 18 ~ FALSE, 
  ED10A > 1 ~ TRUE), 

upper_sec

)
