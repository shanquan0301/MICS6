dat_ch <- dat_ch %>% mutate(
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
    cdisability == 1 ~ TRUE,
    cdisability == 2 ~ FALSE,
    TRUE ~ NA
  ),
  
  # Seeing------------
  #Child wears glasses
  # 1: YES; 2: NO; 9: NO RESPONSE
  seeing_1 = case_when(
    UCF2 %in% c(2) ~ FALSE,
    UCF2 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  #Child has difficulty seeing
  #1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT SEE AT ALL; 8: DON'T KNOW; 9: No Response
  seeing_2 = case_when(
    UCF7 %in% c(1, 2) ~ FALSE,
    UCF7 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Hearing------------
  # Child uses hearing aid
  #1: YES; 2: NO; 9: NO RESPONSE
  hearing_1 = case_when(
    UCF3 %in% c(2) ~ FALSE,
    UCF3 %in% c(1) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Child has difficulty hearing sounds like people voices or music
  #1: NO DIFFICULTY HEARING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT HEAR AT ALL; 8: DON'T KNOW; 9: No Response
  hearing_2 = case_when(
    UCF9 %in% c(1, 2) ~ FALSE,
    UCF9 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Walking ------------
  #1: NO DIFFICULTY WALKING OR CLIMBING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT WALK OR CLIMB AT ALL; 8: DON'T KNOW; 9: No Response
  #Without using equipment or assistance child has difficulty walking
  walking_1 = case_when(
    UCF11 %in% c(1, 2) ~ FALSE,
    UCF11 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #When using equipment or assistance child has difficulty walking
  walking_2 = case_when(
    UCF12 %in% c(1, 2) ~ FALSE,
    UCF12 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #Compared with children of the same age, child has difficulty walking
  walking_3 = case_when(
    UCF13 %in% c(1, 2) ~ FALSE,
    UCF13 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Fine motor ---------------
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT PICK UP AT ALL; 9: NO RESPONSE
  #Compared with children of the same age, child has difficulty picking upsmall objects with his/her hand
  fine_motor = case_when(
    UCF14 %in% c(1, 2) ~ FALSE,
    UCF14 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Communication------------
  #1: NO DIFFICULTY COMMUNICATING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT COMMUNICATE AT ALL; 8: DON'T KNOW; 9: No Response
  #Child has difficulty understanding parent/caretaker
  comm_1 = case_when(
    UCF15 %in% c(1, 2) ~ FALSE,
    UCF15 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  #Child has difficulty being understood by parent/caretaker when speaks
  comm_2 = case_when(
    UCF16 %in% c(1, 2) ~ FALSE,
    UCF16 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Learning-------------------
  #Compared with children of the same age, child has difficulty learning things
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT LEARN THINGS  AT ALL; 9: NO RESPONSE
  learn = case_when(
    UCF17 %in% c(1, 2) ~ FALSE,
    UCF17 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Playing-------------------
  #Compared with children of the same age, child has difficulty playing
  # 1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT LEARN THINGS  AT ALL; 9: NO RESPONSE
  play = case_when(
    UCF18 %in% c(1, 2) ~ FALSE,
    UCF18 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  # Behaviour-------------------
  #Compared with children of the same age, how much child kick, bite or hit other children or adults
  #1: NOT AT ALL; 2: LESS; 3: THE SAME; 4: MORE; 5: A LOT MORE; 9: NO RESPONSE
  behav = case_when(
    UCF19 %in% c(1, 2, 3, 4) ~ FALSE,
    UCF19 %in% c(5) ~ TRUE,
    TRUE ~ NA
  ))
#combine hearing, seeing, waling, comm--------
dat_ch <- dat_ch %>% mutate(
  hearing = hearing_2,
  seeing = seeing_2,
  walking = mapply(any_by_row, walking_1, walking_2, walking_3),
  comm = mapply(any, comm_1, comm_2), 
)

dat_ch <- dat_ch %>% mutate(
  disability_ch = mapply(any_by_row, seeing, hearing, walking, fine_motor, comm, learn, play, behav),
)

