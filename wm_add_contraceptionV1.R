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
  
  living_kids = case_when(
    kids <= 0 ~ 0, 
    kids == 1 ~ 1, 
    kids == 2 ~ 2, 
    kids == 3 ~ 3, 
    kids >=4 ~ 4),  #this is really 4 or more
  
  #Marital status
  married = case_when(
    MA1 %in% c(1,2) ~ TRUE, 
    MA1 == 3 ~ FALSE),
  
  #Union for reporting if in union or not, since differs from marriage
  union = case_when(
    MA1 %in% c(1,2) ~ TRUE, 
    (MA1 == 3 | SB2U <28) ~ FALSE),
  
  #sexual activity 
  sex_activity = case_when(
    (SB2U %in% c(1,2) | MA1 %in% c(1,2)) ~ TRUE, 
    SB2U %in% c(3,4) ~ FALSE), 
  
  #infecund
  #neither pregnant (CP1<>1) nor postpartum amenorrheic, and:
  infecund_0 = CP1 !=1 & UN12F != 'F',
  #1. (a) has not had menstruation for at least six months, OR 
  #   (b) never menstruated, OR 
  #   (c) her last menstruation occurred before her last birth, OR 
  #   (d) is in menopause or has had hysterectomy 
  infecund_1 = UN14U == 3 | UN14N==93 | UN14N==993 | UN14N==94 | UN14N==994 | UN14N==95 | UN14N==995,
  #2. She declares that she has had hysterectomy, 
  #   OR that she has never menstruated OR that she is menopausal, 
  #   OR that she has been trying to get pregnant for 2 or more years without result in response to questions on why she thinks she is not physically able to get pregnant at the time of survey (UN12="B" OR UN12="C" or UN12="D" or UN12="E")
  infecund_2 = UN12D == 'D' | UN12C == 'C' | UN12B == 'B' | UN12E == 'E',
  #3. She declares she cannot get pregnant when asked about desire for future birth (UN7 = 3 or UN8 = 994) 
  infecund_3 = UN7==3 | UN8N==94,
  #4. She has not had a birth in the preceding 5 years (WM6-BH4 (most recent birth)> 5 years), 
  #   never used contraception (CP3<>1) and 
  #   is currently married/in union and 
  #   was continuously married/in union during the last 5 years (MA1=1 or 2 and MA7=1 and WM6-MA8>5 years or WB2-MA11>5). 
  #   SR removed WB2 because it doesn't exist and is based on education variable
  infecund_4 = (WM6Y-BH4Y>5) & CP3 !=1 & ((MA1 == 1 | MA1 == 2) & MA7==1 & WM6Y-MA8Y>5),
  infecund = infecund_0 & infecund_1 & infecund_2 & infecund_3 & infecund_4,
  
  # infecund = case_when(
  #   #neither pregnant (CP1<>1) nor postpartum amenorrheic, and:
  #   (CP1 !=1 | UN12F != 'F') & 
  #     #1. (a) has not had menstruation for at least six months, OR (b) never menstruated, OR #(c) her last menstruation occurred before her last birth, OR (d) is in menopause or has had hysterectomy 
  #     ((UN14U == 3 | UN14N==93 | UN14N==94 | UN14N==95) |
  #        #2. She declares that she has had hysterectomy, OR that she has never menstruated OR that she is menopausal, OR that she has been trying to get pregnant for 2 or more years without result in response to questions on why she thinks she is not physically able to get pregnant at the time of survey (UN12="B" OR UN12="C" or UN12="D" or UN12="E")
  #        (UN12D %in% c('D') | UN12C %in% c('C') | UN12B %in% c('B') | UN12E %in% c('E')) |
  #        #3. She declares she cannot get pregnant when asked about desire for future birth (UN7 = 3 or UN8 = 994) 
  #        (UN7==3 |UN8N==94) | 
  #        #4. She has not had a birth in the preceding 5 years (WM6-BH4 (most recent birth)> 5 years), never used contraception (CP3<>1) and is currently married/in union and was continuously married/in union during the last 5 years 
  #        # (MA1=1 or 2 and MA7=1 and WM6-MA8>5 years or WB2-MA11>5). # SR removed WB2 because it doesn't exist and is based on education variable
  #        ((WM6Y-BH4Y>5) & CP3==1 & (MA1 %in% c(1,2) & MA7==1 & WM6Y-MA8Y>5))) ~ TRUE), 
  
  #any contraception 
  any_contraceptive = case_when(
    MA1 %in% c(1,2) & CP2 == 1 ~ TRUE, 
    MA1 %in% c(1,2) & CP2 == 2 ~ FALSE, 
    MA1 %in% c(1,2) & CP2 == 9 ~ NA_real_), 
  
  # #modern method 
  # modern_method = case_when(
  #   (CP4A == 'A'| CP4B == 'B' | CP4C == 'C' | CP4D == 'D' |
  #      CP4E == 'E'| CP4F == 'F' | CP4G == 'G' | CP4H == 'H' |
  #      CP4I == 'I'| CP4J == 'J' | CP4K == 'K') & CP2 == 1 ~ TRUE, 
  #   (CP4A != 'A' & CP4B != 'B' & CP4C != 'C' & CP4D != 'D' &
  #      CP4E != 'E' & CP4F != 'F' & CP4G != 'G' & CP4H != 'H' &
  #      CP4I != 'I' & CP4J != 'J' & CP4K != 'K') & CP2 == 1 ~ FALSE), 
  # 
  # #traditional method
  # trad_method = case_when(
  #   (CP4L == 'L'| CP4M == 'M' | CP4X == 'X') ~ TRUE, 
  #   (CP4L != 'L' & CP4M != 'M' & CP4X != 'X') ~ FALSE),
  
#Separate the types of contrception  
  method = case_when(
    #modern method 
    (CP2 == 1 & (CP4A == 'A'| CP4B == 'B' | CP4C == 'C' | CP4D == 'D' |
                      CP4E == 'E'| CP4F == 'F' | CP4G == 'G' | CP4H == 'H' |
                      CP4I == 'I'| CP4J == 'J' | CP4K == 'K')) ~ 1,
    #traditional methods
    (CP2 == 1 & (CP4L == 'L'| CP4M == 'M' | CP4X == 'X')) ~ 2, 
  
    #no method 
    CP2 == 2 ~ 0), 
  
  # create modern_method
  modern_method = case_when(
    method == 1 ~ TRUE, 
    method %in% c(2,0) ~ FALSE), 

  #create traditional_method
  trad_method = case_when(
    method == 2 ~ TRUE, 
    method %in% c(1,0) ~ FALSE), 
  
  #female sterilization
  fem_sterile = case_when(
    CP4A %in% c('A') ~ TRUE, 
    CP4A %in% c('?', '') ~ FALSE), 
  
  #Met need for family planning 
  met_need = case_when(
    any_contraceptive == TRUE & (UN7 == 2 | (fem_sterile == TRUE | CP4B =='B') | infecund == TRUE) ~ TRUE,
    any_contraceptive == FALSE ~ FALSE), 
  
  #Unmet need for family planning - check false
  # unmet_need = case_when(
  #   #the percentage of women who are not using a method of contraception (CP2<>1) and:
  #   (any_contraceptive == FALSE & 
  #      #UNMET NEED FOR SPACING
  #      #(a) are pregnant (CP1=1) and say that the pregnancy was mistimed and would have wanted to wait (CP1=1 and UN4=1) OR
  #      ((CP1 == 1 & UN4 == 1) | 
  #         #(b) are postpartum amenorrheic (had a birth in last two years (WM6-BH4 (most recent birth)>2 years) and 
  #         #    is not currently pregnant (CP1<>1), and 
  #         #    whose menstrual period has not returned since the birth of the last child (MN35=2))) and 
  #         #    say that the birth was mistimed: would have wanted to wait (DB4=1) OR
  #         #(UN12F == 'F' & CM17 == 1 & CP1 != 1  & MN35 == 2 & DB4 == 1) | 
  #         (UN12F == 'F' & (WM6Y - BH4Y > 2) & CP1 != 1  & MN35 == 2 & DB4 == 1) | 
  #         #(c) are not pregnant and not postpartum amenorrheic and are fecund and say they want to wait two or more years for their next birth (UN8>=2 years) OR
  #         (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN8N >= 2) |
  #         #(d) are not pregnant and not postpartum amenorrheic and are fecund and unsure whether they want another child (UN7=8) OR
  #         (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN7 == 8)) |
  #      #UNMET NEED FOR LIMITING
  #      #(e) are not pregnant (CP1<>1) and not postpartum amenorrheic and are fecund and say they do not want any more children (UN7=2),  OR
  #      (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN7 == 2) |
  #      #(f) are pregnant (CP1=1) and say they didn't want to have a child (UN4=2), OR
  #      (CP1==1 & UN4==2) |
  #      #(g) are postpartum amenorrheic and say that they didn't want the birth (DB2=2). 
  #      (UN12F == 'F' & DB2==2))  ~ TRUE, 
  #   !((any_contraceptive == FALSE & 
  #        #UNMET NEED FOR SPACING
  #        #(a) are pregnant (CP1=1) and say that the pregnancy was mistimed and would have wanted to wait (CP1=1 and UN4=1) OR
  #        ((CP1 == 1 & UN4 == 1) | 
  #           #(b) are postpartum amenorrheic (had a birth in last two years (WM6-BH4 (most recent birth)>2 years) and is not currently pregnant (CP1<>1), and whose menstrual period has not returned since the birth of the last child (MN35=2))) and say that the birth was mistimed: would have wanted to wait (DB4=1) OR
  #           (UN12F == 'F' & CM17 == 1 & CP1 != 1  & MN35 == 2 & DB4 == 1) | 
  #           #(c) are not pregnant and not postpartum amenorrheic and are fecund and say they want to wait two or more years for their next birth (UN8>=2 years) OR
  #           (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN8N >= 2) |
  #           #(d) are not pregnant and not postpartum amenorrheic and are fecund and unsure whether they want another child (UN7=8) OR
  #           (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN7 == 8)) |
  #        #UNMET NEED FOR LIMITING
  #        #(e) are not pregnant (CP1<>1) and not postpartum amenorrheic and are fecund and say they do not want any more children (UN7=2),  OR
  #        (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN7 == 2) |
  #        #(f) are pregnant (CP1=1) and say they didn't want to have a child (UN4=2), OR
  #        (CP1==1 & UN4==2) |
  #        #(g) are postpartum amenorrheic and say that they didn't want the birth (DB2=2). 
  #        (UN12F == 'F' & DB2==2))) ~ FALSE ),
  
  unmet_need = 
    #the percentage of women who are not using a method of contraception (CP2<>1) and:
    any_contraceptive == FALSE & sex_activity == TRUE & 
    #UNMET NEED FOR SPACING
    #(a) are pregnant (CP1=1) and say that the pregnancy was mistimed and would have wanted to wait (CP1=1 and UN4=1) OR
    ((CP1 == 1 & UN4 == 1) | 
       #(b) are postpartum amenorrheic (had a birth in last two years (WM6-BH4 (most recent birth)>2 years) and 
       #    is not currently pregnant (CP1<>1), and 
       #    whose menstrual period has not returned since the birth of the last child (MN35=2))) and 
       #    say that the birth was mistimed: would have wanted to wait (DB4=1) OR
       #(UN12F == 'F' & CM17 == 1 & CP1 != 1  & MN35 == 2 & DB4 == 1) | 
       (UN12F == 'F' & (WM6Y - BH4Y > 2) & CP1 != 1  & MN35 == 2 & DB4 == 1) | 
       #(c) are not pregnant and not postpartum amenorrheic and are fecund and say they want to wait two or more years for their next birth (UN8>=2 years) OR
       (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN8N >= 2) |
       #(d) are not pregnant and not postpartum amenorrheic and are fecund and unsure whether they want another child (UN7=8) OR
       (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN7 == 8)) |
    #UNMET NEED FOR LIMITING
    #(e) are not pregnant (CP1<>1) and not postpartum amenorrheic and are fecund and say they do not want any more children (UN7=2),  OR
    (CP1 != 1 & UN12F != 'F' & infecund != TRUE & UN7 == 2) |
    #(f) are pregnant (CP1=1) and say they didn't want to have a child (UN4=2), OR
    (CP1==1 & UN4==2) |
    #(g) are postpartum amenorrheic and say that they didn't want the birth (DB2=2). 
    (UN12F == 'F' & DB2==2))


dat_wm <- dat_wm %>% mutate(  
  #Total demand for family planning - NOT WORKING @SHANQUAN,checked
  demand = case_when(
    any_contraceptive == TRUE & unmet_need == TRUE ~ 1, 
    any_contraceptive == FALSE | unmet_need == FALSE ~ 0), 
  
  any_contr = case_when(
    any_contraceptive == TRUE ~ 1, 
    any_contraceptive == FALSE ~ 0), 
  
  mod_meth = case_when(
    modern_method == TRUE ~ 1, 
    modern_method == FALSE ~ 0), 
  
  #Demand by any method -NOT WORKING @SHANQUAN,checked
  # demand_any = case_when(
  #   any_contr/demand == 1 ~ TRUE, 
  #   any_contr/demand %in% c(0, 'Inf') ~ FALSE), 
  demand_any = any_contr == 1 & demand == 1, 
  
  #Demand modern - NOT WORKING @SHANQUAN,checked
  # demand_modern = case_when(
  #   mod_meth/demand == 1 ~ TRUE, 
  #   mod_meth/demand %in% c(0, 'Inf') ~ FALSE)
  demand_modern = mod_meth == 1 & demand == 1
)
