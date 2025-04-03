dat_wm <- dat_wm %>% mutate(
  # Mother's functional disabilities (age 18-49 years): --------------
  #1: Has functional difficulty; 2: Has no functional difficulty; 7: No information
  # caretake_functional_dis_1 = case_when(
  #   disability == 1 ~ TRUE,
  #   disability == 2 ~ FALSE,
  #   TRUE ~ NA
  # ),
  # caretake_functional_dis = case_when(
  #   mdisability == 1 ~ TRUE,
  #   mdisability == 2 ~ FALSE,
  #   TRUE ~ NA
  # ),
  # 
  # Functional difficulty-------
  #1: Has functional difficulty; 2: Has no functional difficulty; 9: Missing
  functional = case_when(
    disability == 1 ~ TRUE,
    disability == 2 ~ FALSE,
    TRUE ~ NA
  ),
  # functional_2 = case_when(
  #   mdisability == 1 ~ TRUE,
  #   mdisability == 2 ~ FALSE,
  #   TRUE ~ NA
  # ),
  
  # Seeing------------
  # Use glasses or contact lenses
  # 1: YES; 2: NO; 9: NO RESPONSE
  seeing_1 = case_when(
    AF2 %in% c(2) ~ FALSE,
    AF2 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  # Difficulty seeing, even if wearing glasses or contact lenses
  #1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT SEE AT ALL; 8: DON'T KNOW; 9: No Response
  seeing_2 = case_when(
    AF6 %in% c(1, 2) ~ FALSE,
    AF6 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Hearing------------
  # Use hearing aid
  #1: YES; 2: NO; 9: NO RESPONSE
  hearing_1 = case_when(
    AF3 %in% c(2) ~ FALSE,
    AF3 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Difficulty hearing, even if using a hearing aid
  #1: NO DIFFICULTY HEARING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT HEAR AT ALL; 8: DON'T KNOW; 9: No Response
  hearing_2 = case_when(
    AF8 %in% c(1, 2) ~ FALSE,
    AF8 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Walking ------------
  #1: NO DIFFICULTY WALKING OR CLIMBING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT WALK OR CLIMB AT ALL; 8: DON'T KNOW; 9: No Response
  #Difficulty walking or climbing steps
  walking = case_when(
    AF9 %in% c(1, 2) ~ FALSE,
    AF9 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  # Remembering or concentrating------------
  # Difficulty remembering or concentrating
  #1: NO DIFFICULTY REMEMBERING/ CONCENTRATING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT REMEMBER/CONCENTRATE AT ALL; 8: DON'T KNOW; 9: No Response
  remember_conc = case_when(
    AF10 %in% c(1, 2) ~ FALSE,
    AF10 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Self-care------------------------
  # Difficulty with self-care, such as washing all over or dressing
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT CARE FOR SELF AT ALL; 9: NO RESPONSE
  self = case_when(
    AF11 %in% c(1, 2) ~ FALSE,
    AF11 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Communication------------
  #1: NO DIFFICULTY COMMUNICATING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT COMMUNICATE AT ALL; 8: DON'T KNOW; 9: No Response
  #Difficulty communicating
  comm = case_when(
    AF12 %in% c(1, 2) ~ FALSE,
    AF12 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ))

#combine hearing, seeing, caretake_functional_dis, functional-----------------
dat_wm <- dat_wm %>% mutate(
  hearing = hearing_2, #only consider AF8
  seeing = seeing_2 #only consider AF6
  #caretake_functional_dis = mapply(any, caretake_functional_dis_1, caretake_functional_dis_2),
  #functional = mapply(any, functional_1, functional_2)
)
dat_wm <- dat_wm %>% mutate(
  disability_wm = mapply(any_by_row, seeing, hearing, walking, comm, self, remember_conc)
)