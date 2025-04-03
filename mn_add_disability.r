dat_mn <- dat_mn %>% mutate(
  # Mother's functional disabilities (age 18-49 years): --------------
  #1: Has functional difficulty; 2: Has no functional difficulty; 7: No information
  # caretake_functional_dis_1 = case_when(
  #   disability == 1 ~ TRUE,
  #   disability == 2 ~ FALSE,
  #   TRUE ~ NA
  # ),
  caretake_functional_dis_2 = case_when(
    mdisability == 1 ~ TRUE,
    mdisability == 2 ~ FALSE,
    TRUE ~ NA
  ),
  caretake_functional_dis_3 = case_when(
    mdisability59 == 1 ~ TRUE,
    mdisability59 == 2 ~ FALSE,
    TRUE ~ NA
  ),
  
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
  # functional_3 = case_when(
  #   mdisability59 == 1 ~ TRUE,
  #   mdisability59 == 2 ~ FALSE,
  #   TRUE ~ NA
  # ),
  
  # Seeing------------
  # Use glasses or contact lenses
  # 1: YES; 2: NO; 9: NO RESPONSE
  seeing_1 = case_when(
    MAF2 %in% c(2) ~ FALSE,
    MAF2 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  # Difficulty seeing, even if wearing glasses or contact lenses
  #1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT SEE AT ALL; 8: DON'T KNOW; 9: No Response
  seeing_2 = case_when(
    MAF6 %in% c(1, 2) ~ FALSE,
    MAF6 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Hearing------------
  # Use hearing aid
  #1: YES; 2: NO; 9: NO RESPONSE
  hearing_1 = case_when(
    MAF3 %in% c(2) ~ FALSE,
    MAF3 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Difficulty hearing, even if using a hearing aid
  #1: NO DIFFICULTY HEARING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT HEAR AT ALL; 8: DON'T KNOW; 9: No Response
  hearing_2 = case_when(
    MAF8 %in% c(1, 2) ~ FALSE,
    MAF8 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Walking ------------
  #1: NO DIFFICULTY WALKING OR CLIMBING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT WALK OR CLIMB AT ALL; 8: DON'T KNOW; 9: No Response
  #Difficulty walking or climbing steps
  walking = case_when(
    MAF9 %in% c(1, 2) ~ FALSE,
    MAF9 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  # Remembering or concentrating------------
  # Difficulty remembering or concentrating
  #1: NO DIFFICULTY REMEMBERING/ CONCENTRATING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT REMEMBER/CONCENTRATE AT ALL; 8: DON'T KNOW; 9: No Response
  remember_conc = case_when(
    MAF10 %in% c(1, 2) ~ FALSE,
    MAF10 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Self-care------------------------
  # Difficulty with self-care, such as washing all over or dressing
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT CARE FOR SELF AT ALL; 9: NO RESPONSE
  self = case_when(
    MAF11 %in% c(1, 2) ~ FALSE,
    MAF11 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Communication------------
  #1: NO DIFFICULTY COMMUNICATING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT COMMUNICATE AT ALL; 8: DON'T KNOW; 9: No Response
  #Difficulty communicating
  comm = case_when(
    MAF12 %in% c(1, 2) ~ FALSE,
    MAF12 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ))

#combine hearing, seeing, caretake_functional_dis, functional-------------
dat_mn <- dat_mn %>% mutate(
  hearing = hearing_2, #only consider mAF8
  seeing = seeing_2, #only consider mAF6
  caretake_functional_dis = mapply(any, #caretake_functional_dis_1, 
                                   caretake_functional_dis_2, caretake_functional_dis_3)
  #functional = mapply(any, functional_1, functional_2, functional_3)
)

dat_mn <- dat_mn %>% mutate(
  disability_mn = mapply(any_by_row, seeing, hearing, walking, comm, self, remember_conc)
)