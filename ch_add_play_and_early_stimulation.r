dat_ch <- dat_ch %>% mutate(
  #BOOK--------------
  book_mother = case_when(
    EC5AA == "A" ~ TRUE,
    EC5AA == "" ~ FALSE,
    TRUE ~ NA
  ),
  book_father = case_when(
    EC5AB == "B" ~ TRUE,
    EC5AB == "" ~ FALSE,
    TRUE ~ NA
  ),
  book_other = case_when(
    EC5AX == "X" ~ TRUE,
    EC5AX == "" ~ FALSE,
    TRUE ~ NA
  ),
  book_num = ifelse(EC1 > 50, NA, EC1),
  #more than three books - 
  three_books = case_when(
    book_num >= 3 ~ TRUE,
    book_num < 3 ~ FALSE
  ), 
  ten_books = case_when(
    book_num >= 10 ~ TRUE, 
    book_num <10 ~ FALSE
  ),
  #story--------------
  story_mother = case_when(
    EC5BA == "A" ~ TRUE,
    EC5BA == "" ~ FALSE,
    TRUE ~ NA
  ),
  story_father = case_when(
    EC5BB == "B" ~ TRUE,
    EC5BB == "" ~ FALSE,
    TRUE ~ NA
  ),
  story_other = case_when(
    EC5BX == "X" ~ TRUE,
    EC5BX == "" ~ FALSE,
    TRUE ~ NA
  ),
  #SONG--------------
  song_mother = case_when(
    EC5CA == "A" ~ TRUE,
    EC5CA == "" ~ FALSE,
    TRUE ~ NA
  ),
  song_father = case_when(
    EC5CB == "B" ~ TRUE,
    EC5CB == "" ~ FALSE,
    TRUE ~ NA
  ),
  song_other = case_when(
    EC5CX == "X" ~ TRUE,
    EC5CX == "" ~ FALSE,
    TRUE ~ NA
  ),
  #OUTSIDE--------------
  outside_mother = case_when(
    EC5DA == "A" ~ TRUE,
    EC5DA == "" ~ FALSE,
    TRUE ~ NA
  ),
  outside_father = case_when(
    EC5DB == "B" ~ TRUE,
    EC5DB == "" ~ FALSE,
    TRUE ~ NA
  ),
  outside_other = case_when(
    EC5DX == "X" ~ TRUE,
    EC5DX == "" ~ FALSE,
    TRUE ~ NA
  ),
  #PLAY WITH--------------
  play_with_mother = case_when(
    EC5EA == "A" ~ TRUE,
    EC5EA == "" ~ FALSE,
    TRUE ~ NA
  ),
  play_with_father = case_when(
    EC5EB == "B" ~ TRUE,
    EC5EB == "" ~ FALSE,
    TRUE ~ NA
  ),
  play_with_other = case_when(
    EC5EX == "X" ~ TRUE,
    EC5EX == "" ~ FALSE,
    TRUE ~ NA
  ),
  #Named counted or drew things with--------------
  named_with_mother = case_when(
    EC5FA == "A" ~ TRUE, 
    EC5FA == "" ~ FALSE,
    TRUE ~ NA
  ), 
  named_with_father = case_when(
    EC5FB == "B" ~ TRUE, 
    EC5FB == "" ~ FALSE,
    TRUE ~ NA
  ), 
  named_with_other = case_when(
    EC5FX == "X" ~ TRUE, 
    EC5FX == "" ~ FALSE,
    TRUE ~ NA
  ), 
  #TOY------------
  toy_home = case_when(
    EC2A == 1 ~ TRUE,
    EC2A == 2 ~ FALSE,
    EC2A %in% c(8, 9) ~ NA
  ),
  toy_shop = case_when(
    EC2B == 1 ~ TRUE,
    EC2B == 2 ~ FALSE,
    EC2B %in% c(8, 9) ~ NA
  )
)

#combine book, story, song, outside, play_with, named_with, toy------------
dat_ch <- dat_ch %>% mutate(
  book = mapply(any, book_mother, book_father, book_other),
  story = mapply(any, story_mother, story_father, story_other),
  song = mapply(any, song_mother, song_father, song_other),
  outside = mapply(any, outside_mother, outside_father, outside_other),
  play_with = mapply(any, play_with_mother, play_with_father, play_with_other),
  named_with = mapply(any, named_with_mother, named_with_father, named_with_other),
  toy = mapply(any, toy_home, toy_shop)
)
# add MICS early stimulation variable--------
#Percentage of children with whom adult household members have engaged in four or more activities
dat_ch <- dat_ch %>% mutate(
  early_stim_mother = mapply(sum_by_row, story_mother, song_mother, outside_mother, play_with_mother), #book_mother,, named_with_mother
  early_stim_father = mapply(sum_by_row, story_father, song_father, outside_father, play_with_father), #book_father, named_with_father
  early_stim_other = mapply(sum_by_row, story_other, song_other, outside_other, play_with_other)# book_other, , named_with_other
)

dat_ch <- dat_ch %>% mutate(
  early_stim = mapply(sum_by_row, story, song, outside, play_with)) #book, , named_with

dat_ch <- dat_ch %>% mutate(
  early_stim_mother_4 = early_stim_mother >= 4,
  early_stim_father_4 = early_stim_father >= 4,
  early_stim_other_4 = early_stim_other >=4,
  early_stim_4 = early_stim >= 4
)

