dat_wm <- dat_wm %>% mutate(
  #dat_all <- dat_all %>% mutate(
  
  #BH4Y issue 
  BH4Y = ifelse(is.na(BH4Y_LAST), as.numeric(BH4Y_last), as.numeric(BH4Y_LAST)),
  
  #Education
  welevel = case_when(
    welevel == 0 ~ 0, #Pre-primary or none
    welevel == 1 ~ 1, #Primary education
    welevel >= 2 ~ 2), #Secondary or higher 
  
  #Living children
  kids = as.numeric((CM3 + CM4 + CM6 + CM7) - (CM9 + CM10)),
  
  # Compute `livingChildren` -SR to check
  livingChildren = ifelse(CM1 == 1 & CM8 != 1, CM11, ifelse(CM1 == 1 & CM8 == 1, CM11 - (CM9 + CM10), 0)), 
  
  #Marital status
  married = case_when(
    MA1 %in% c(1,2) ~ TRUE, 
    MA1 == 3 ~ FALSE, 
    MA1 == 9 ~ NA_real_),
  
  #Union for reporting if in union or not, since differs from marriage
  union = case_when(
    MA1 %in% c(1,2) ~ TRUE, 
    (MA1 == 3 | SB2U <28) ~ FALSE),
  
  #sexual activity 
  sex_activity = case_when(
    (SB2U %in% c(1,2) | MA1 %in% c(1,2)) ~ TRUE, 
    SB2U %in% c(3,4) ~ FALSE), 
  
  #create any contraceptive variable
  any_contraceptive = case_when(
    MA1 %in% c(1,2) & CP2 == 1 ~ TRUE,
    MA1 %in% c(1,2) & (CP2 == 2  | CP1 == 1 | CP2 == 9) ~ FALSE),
  
  #Separate the types of contraception  
  # Assuming your data is in a dataframe called `df`
  contraceptiveMethod = 99,
  
  # Assign values to contraceptiveMethod based on conditions
  contraceptiveMethod = case_when(
    CP4P == "P" ~ 17, #Kiribati cycle beads
    CP4O == "O" ~ 16, #Kiribati ovulation method
    CP4N == "N" & country_full %in% c("Georgia", "Togo") ~ 15, # candle (Georgia – assume this is a fertility awareness method and therefore TRADITIONAL METHOD) or traditional method (Togo) (TRADITIONAL METHOD)
    CP4X == "X" ~ 14,
    CP4M == "M" ~ 13,
    CP4L == "L" ~ 12,
    CP4K == "K" ~ 11,
    CP4N == "N" ~ 10, #CP4N - either emergency contraception (MODERN METHOD) or patch (MODERN METHOD)
    CP4I == "I" | CP4J == "J" ~ 9,
    CP4H == "H" ~ 8,
    CP4G == "G" ~ 7,
    CP4F == "F" ~ 6,
    CP4E == "E" ~ 5,
    CP4D == "D" ~ 4,
    CP4C == "C" ~ 3,
    CP4B == "B" ~ 2,
    CP4A == "A" ~ 1,
    TRUE ~ contraceptiveMethod),  # Keep the current value if no condition matches
  
  # Override contraceptiveMethod if certain conditions are met
  contraceptiveMethod = ifelse(CP1 == 1 | CP2 == 2 | CP2 == 9, 0, contraceptiveMethod), 
  
  # Compute `anyModern`
  anyModern = ifelse(contraceptiveMethod >= 1 & contraceptiveMethod <= 11, TRUE, FALSE),
  
  # Compute `anyTraditional`
  anyTraditional = ifelse(contraceptiveMethod >= 12 & contraceptiveMethod < 99, TRUE, FALSE),
  
  # Compute `anyMethod`
  anyMethod = case_when(
    anyModern == TRUE | anyTraditional == TRUE ~ TRUE, 
    anyModern == FALSE & anyTraditional == FALSE ~ FALSE), 
  
  
  #female sterilization
  fem_sterile = case_when(
    CP4A %in% c('A') ~ TRUE, 
    CP4A %in% c('?', '') ~ FALSE), 
  
  #variables needed to calculate unmet need
  
  #time since last birth 
  tsinceb = (WDOI - WDOBLC),
  
  #time since last period
  tsincep = case_when(
    UN14N >= 0 & UN14N <= 90 & UN14U == 1 ~ floor(UN14N/30),
    UN14N >= 0 & UN14N <= 90 & UN14U == 2 ~ floor(UN14N/4.3),
    UN14N >= 0 & UN14N <= 90 & UN14U == 3 ~ UN14N,
    UN14N >= 0 & UN14N <= 90 & UN14U == 4 ~ UN14N * 12),
  
  #postpartum amenhoreic (PPA)
  pregPPA = case_when(
    CP1 == 1 | MN35 == 2 ~ 1, 
    MN35 == 9 & !(is.na(tsinceb)) & !(is.na(tsincep)) & tsincep > tsinceb & tsinceb <60 ~ 1, 
    MN35 == 9 & !(is.na(tsinceb)) & UN14N %in% c(94, 994) & tsinceb < 60 ~ 1),
  
  #recode as PPA in last 24 months
  pregPPA24 = case_when(
    CP1==1 | (pregPPA == 1 & tsinceb < 24) ~ 1),
  
  #UNWANTED CHILDREN
  wantedlast = case_when(
    CP1 == 1 & UN2 == 2 ~ as.numeric(UN4 + 1), 
    CP1 == 1 & UN2 == 2 & UN4 == 9 ~ NA_real_, 
    CP1 == 1 & DB2 == 2 ~ as.numeric(DB4 + 1), 
    CP1 == 1 & DB2 == 2 & DB4 == 9 ~ NA_real_),
  
  #Fecundity 
  infec = case_when(
    CP1 != 1 & pregPPA24 != 1 ~ 0, 
    #married
    married == 1 & (WDOI - WDOM >=60) & tsinceb >= 60 & CP3 != 1 ~ 1,
    #declared infecund 
    UN7 == 3 | UN8N == 94 | UN8N == 994 ~ 1, 
    #menopausal/hysterectomy 
    UN12B == 'B' | UN12D == 'D' | (UN12C == 'C' & tsinceb >= 60) ~ 1, 
    #time since last period is greater than 6 months and not PPA
    !(is.na(tsincep)) & tsincep >= 6 & pregPPA != 1 ~ 1, 
    #menopause/hysterectomy on time since last period 
    UN14N == 93 | UN14N == 993 ~ 1, 
    (UN14N == 95 | UN14N == 995) & (is.na(tsinceb) | tsinceb >= 60) ~ 1, 
    #time since last birth 
    (UN14N == 94 | UN14N == 994) & tsinceb >= 60 ~ 1, 
    is.na(tsinceb) & (UN14N == 94 | UN14N == 994) ~ 1),
  
  unmet = NA_real_,
  
  #Unmet need - MICS way 
  unmet_need_calc = case_when(
    #Group 1 - any contraceptive users
    is.na(unmet) & CP2 == 1 ~ 3,
    CP2 == 1 & (CP4A == 'A' | CP4B == 'B' | UN7 %in% c(2, 3) | UN8N == 94 | UN8N == 994) ~ 4, 
    #Group 2 - PPA women 
    pregPPA24 == 1 & wantedlast == 1 ~ 7, 
    pregPPA24 == 1 & wantedlast == 2 ~ 1, 
    pregPPA24 == 1 & wantedlast == 3 ~ 2, 
    #Group 3 - Infecund
    infec == 1 ~ 9, 
    #Group 4 - Fecund 
    #wants within 2 years
    UN7 == 1 & UN8U == 1 & UN8N <24 ~ 7, 
    UN7 == 1 & UN8U == 2 & UN8N <2 ~ 7, 
    UN7 == 1 & UN8N %in% c(93, 993) ~ 7,  
    #2+ years or unsure
    UN7 == 1 & UN8N <= 90 ~ 1, 
    UN7 == 1 & UN8N %in% c(95, 96, 98, 995, 996, 998) ~ 1, 
    UN7 == 8 ~ 1, 
    #wants no more 
    UN7 == 2 ~ 2), 
  
  #unmet need as one var
  unmet_need = case_when(
    unmet_need_calc %in% c(1,2) ~ TRUE, 
    unmet_need_calc %in% c(3,4,7) ~ FALSE),
  
  #calculate total demand 
  totdem = unmet_need_calc %in% c(1,2,3,4), 
  
  #% demand satisfied
  satisf = case_when(
    unmet_need_calc >=1 | unmet_need_calc <= 4 ~ 0, 
    unmet_need_calc %in% c(3,4) ~ 100), 
  
  #Satistfied by modern methods
  satisfM = case_when(
    unmet_need_calc >=1 | unmet_need_calc <= 4 ~ 0, 
    unmet_need_calc %in% c(3,4) & anyModern == TRUE ~ 100)
  
)
