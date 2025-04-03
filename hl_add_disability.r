dat_hl <- dat_hl %>% mutate(
  # Mother's functional disabilities (age 18-49 years): -----------------
  #1: Has functional difficulty; 2: Has no functional difficulty; 7: No information
  # caretake_functional_dis = case_when(
  #   caretakerdis == 1 ~ TRUE,
  #   caretakerdis == 2 ~ FALSE,
  #   TRUE ~ NA
  # ),
  
  # Functional difficulty-------
  #1: Has functional difficulty; 2: Has no functional difficulty; 9: Missing
  functional = ifelse(is.na(disability), as.numeric(disability2), disability),
  functional = case_when(
    functional == 1 ~ TRUE,
    functional == 2 ~ FALSE,
    TRUE ~ NA
  ),
  
  # Seeing------------
  #1: NO DIFFICULTY; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT SEE AT ALL; 8: DON'T KNOW; 9: No Response
  # with glasses
  seeing_1 = case_when(
    DA5 %in% c(1, 2) ~ FALSE,
    DA5 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  # without glasses
  seeing_2 = case_when(
    DA6 %in% c(1, 2) ~ FALSE,
    DA6 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  dis_see = ifelse(is.na(DA6), DA5, DA6),
  # Hearing------------
  #1: NO DIFFICULTY HEARING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT HEAR AT ALL; 8: DON'T KNOW; 9: No Response
  # with hearing aid
  hearing_1 = case_when(
    DA8 %in% c(1, 2) ~ FALSE,
    DA8 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  # without hearing aid
  hearing_2 = case_when(
    DA9 %in% c(1, 2) ~ FALSE,
    DA9 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  
  dis_hear = ifelse(is.na(DA9), DA8, DA9),
  # Communication------------
  #1: NO DIFFICULTY COMMUNICATING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT COMMUNICATE AT ALL; 8: DON'T KNOW; 9: No Response
  comm = case_when(
    DA10 %in% c(1, 2) ~ FALSE,
    DA10 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_comm = DA10,
  # Remembering or concentrating------------
  #1: NO DIFFICULTY REMEMBERING/ CONCENTRATING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT REMEMBER/CONCENTRATE AT ALL; 8: DON'T KNOW; 9: No Response
  remember_conc = case_when(
    DA11 %in% c(1, 2) ~ FALSE,
    DA11 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_remember_conc = DA11,
  # Walking or climbing steps------------
  #1: NO DIFFICULTY WALKING OR CLIMBING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT WALK OR CLIMB AT ALL; 8: DON'T KNOW; 9: No Response
  walking = case_when(
    DA12 %in% c(1, 2) ~ FALSE,
    DA12 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_walk = DA12,
  # (self_care)Washing all over or dressing------------
  #1: NO DIFFICULTY WASHING OR DRESSING; 2: SOME DIFFICULTY; 3: A LOT OF DIFFICULTY; 4: CANNOT WASH OR DRESS AT ALL; 8: DON'T KNOW; 9: No Response
  self = case_when(
    DA13 %in% c(1, 2) ~ FALSE,
    DA13 %in% c(3, 4) ~ TRUE,
    TRUE ~ NA
  ),
  dis_self = DA13)

#combine hearing and seeing-------
dat_hl <- dat_hl %>% mutate(
  hearing = mapply(any, hearing_1, hearing_2),
  seeing = mapply(any, seeing_1, seeing_2)
)

dat_hl <- dat_hl %>% mutate(
  disability_hl = mapply(any_by_row, seeing, hearing, walking, comm, self, remember_conc)
)


