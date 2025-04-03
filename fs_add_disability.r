dat_fs <- dat_fs %>% mutate(
  # Mother's functional disabilities (age 18-49 years): --------------
  #1: Has functional difficulty; 2: Has no functional difficulty; 7: No information
  caretake_functional_dis = case_when(
    caretakerdis == 1 ~ TRUE,
    caretakerdis == 2 ~ FALSE,
    TRUE ~ NA
  ),
  
  # Functional difficulty-------
  #1: Has functional difficulty; 2: Has no functional difficulty; 9: Missing
  functional = case_when(
    fsdisability == 1 ~ TRUE,
    fsdisability == 2 ~ FALSE,
    TRUE ~ NA
  ),
  
  # Seeing------------
  #seeing_1 and seeing_2 co-exist
  #Child wear glasses or contact lenses
  # 1: YES; 2: NO; 9: NO RESPONSE
  seeing_1 = case_when(
    FCF1 %in% c(2) ~ FALSE,
    FCF1 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  #Child has difficulty seeing
  #1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT SEE AT ALL; 8: DON'T KNOW; 9: No Response
  seeing_2 = case_when(
    FCF6 %in% c(1, 2) ~ FALSE,
    FCF6 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_see = FCF6,
  
  # Hearing------------
  #hearing_1 and hearing_2 co-exist
  # Child uses hearing aid
  #1: YES; 2: NO; 9: NO RESPONSE
  hearing_1 = case_when(
    FCF2 %in% c(2) ~ FALSE,
    FCF2 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Child has difficulty hearing sounds like people voices or music
  #1: NO DIFFICULTY HEARING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT HEAR AT ALL; 8: DON'T KNOW; 9: No Response
  hearing_2 = case_when(
    FCF8 %in% c(1, 2) ~ FALSE,
    FCF8 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_hear = FCF8,
  # Walking ------------
  # walking 1 and 3 co-exist
  # walking 2 and 4 not co-exist
  # waling 5 and 6 prive most of the information
  #1: NO DIFFICULTY WALKING OR CLIMBING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT WALK OR CLIMB AT ALL; 8: DON'T KNOW; 9: No Response
  # if use equipment
  equip_use = case_when(
    FCF3 %in% c(1) ~ TRUE,
    FCF3 %in% c(2) ~ FALSE,
    TRUE ~ NA,
  ),
  equip_nouse = case_when(
    FCF3 %in% c(2) ~ TRUE,
    FCF3 %in% c(1) ~ FALSE,
    TRUE ~ NA,
  ),
  #Without using equipment or assistance child has difficulty walking 100 yards
  walking_1 = case_when(
    FCF10 %in% c(1, 2) ~ FALSE,
    FCF10 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #Without using equipment or assistance child has difficulty walking 500 yards
  walking_2 = case_when(
    FCF11 %in% c(1, 2) ~ FALSE,
    FCF11 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #When using equipment or assistance child has difficulty walking 100 yards
  walking_3 = case_when(
    FCF12 %in% c(1, 2) ~ FALSE,
    FCF12 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #When using equipment or assistance child has difficulty walking 500 yards
  walking_4 = case_when(
    FCF13 %in% c(1, 2) ~ FALSE,
    FCF13 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #Compared with children of the same age, child has difficulty walking 100 yards
  walking_5 = case_when(
    FCF14 %in% c(1, 2) ~ FALSE,
    FCF14 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #Compared with children of the same age, child has difficulty walking 500 yards
  walking_6 = case_when(
    FCF15 %in% c(1, 2) ~ FALSE,
    FCF15 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_walk_1 = FCF10,
  dis_walk_2 = FCF11,
  dis_walk_3 = FCF14,
  dis_walk_4 = FCF15,
  # Self-care------------------------
  # Child has difficulty with self-care such as feeding or dressing
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT CARE FOR SELF AT ALL; 9: NO RESPONSE
  self = case_when(
    FCF16 %in% c(1, 2) ~ FALSE,
    FCF16 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_self = FCF16,
  # Communication------------
  #1: NO DIFFICULTY COMMUNICATING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT COMMUNICATE AT ALL; 8: DON'T KNOW; 9: No Response
  #Child has difficulty being understood by people inside of this household
  comm_1 = case_when(
    FCF17 %in% c(1, 2) ~ FALSE,
    FCF17 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #Child has difficulty being understood by people outside of this household
  comm_2 = case_when(
    FCF18 %in% c(1, 2) ~ FALSE,
    FCF18 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_comm_1 = FCF17,
  dis_comm_2 = FCF18,
  # Learning-------------------
  #Compared with children of the same age, child has difficulty learning things
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT LEARN THINGS  AT ALL; 9: NO RESPONSE
  learn = case_when(
    FCF19 %in% c(1, 2) ~ FALSE,
    FCF19 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_learn = FCF19,
  # Remembering-----------------
  # Compared with children of the same age, child has difficulty remembering things
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT REMEMBER THINGS AT ALL; 9: NO RESPONSE
  remember = case_when(
    FCF20 %in% c(1, 2) ~ FALSE,
    FCF20 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_remember = FCF20,
  # Concentrating-------------------
  # Child has difficulty concentrating on an activity that he/she enjoys
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT CONCENTRATE AT ALL; 9: NO RESPONSE
  conc = case_when(
    FCF21 %in% c(1, 2) ~ FALSE,
    FCF21 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_conc = FCF21,
  # Accepting change------------------
  # Child has difficulty accepting changes in his/her routine
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT ACCEPT CHANGES AT ALL; 9: NO RESPONSE
  change = case_when(
    FCF22 %in% c(1, 2) ~ FALSE,
    FCF22 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_change = FCF22,
  # Behaviour, Controlling behaviour -------------------
  # Compared with children of the same age, child have difficulty controlling his/her behaviour
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT CONTROL BEHAVIOUR AT ALL; 9: NO RESPONSE
  behav = case_when(
    FCF23 %in% c(1, 2) ~ FALSE,
    FCF23 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_behav = FCF23,
  # Making friends ------------------------
  # Child has difficulty making friends
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT MAKE FRIENDS AT ALL; 9: NO RESPONSE
  friend = case_when(
    FCF24 %in% c(1, 2) ~ FALSE,
    FCF24 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_friend = FCF24,
  # Anxiety --------------------
  # How often child seems very anxious, nervous or worried
  # 1: DAILY; 2: WEEKLY; 3: MONTHLY; 4: A FEW TIMES A YEAR; 5: NEVER; 9: NO RESPONSE
  anxiety = case_when(
    FCF25 %in% c(2, 3, 4, 5) ~ FALSE,
    FCF25 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  dis_anxiety = FCF25,
  # Depression -----------------------
  # How often child seems very sad or depressed
  # 1: DAILY; 2: WEEKLY; 3: MONTHLY; 4: A FEW TIMES A YEAR; 5: NEVER; 9: NO RESPONSE
  depression = case_when(
    FCF26 %in% c(2, 3, 4, 5) ~ FALSE,
    FCF26 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  dis_depression = FCF26)
#combine hearing, seeing, waling, comm, remember_conc--------------
dat_fs <- dat_fs %>% mutate(
  # hearing = mapply(any_by_row, hearing_1, hearing_2),
  # seeing = mapply(any_by_row, seeing_1, seeing_2),
  hearing = hearing_2,
  seeing = seeing_2,
  walking_equip = mapply(any, walking_1, walking_2),
  walking_noequip = mapply(any, walking_5, walking_6),
  comm = mapply(any, comm_1, comm_2),
  remember_conc = mapply(any, remember, conc)
)

dat_fs <- dat_fs %>% mutate(
  walking_f_1 = mapply(all, equip_use, walking_equip),
  walking_f_2 = mapply(all, equip_nouse, walking_noequip)
)

dat_fs <- dat_fs %>% mutate(
  walking = mapply(any, walking_f_1, walking_f_2)
)

dat_fs <- dat_fs %>% mutate(
  disability_fs = mapply(any_by_row, seeing, hearing, walking, self, comm, learn, 
                         remember, conc, change, behav, friend, anxiety, depression),
  disability_nomh = mapply(any_by_row,
                           hearing, seeing, comm, walking, remember, conc, self, 
                           behav, learn, friend, change))
