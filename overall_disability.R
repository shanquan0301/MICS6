# overall disability----------------
dat_all <- dat_all %>% mutate(
  disability_fs = mapply(any,
                         hearing, seeing, comm, walking, remember, conc, self, behav, 
                         learn, friend, change, anxiety, depression), 
  
  disability_nomh = mapply(any_by_row,
                           hearing, seeing, comm, walking, remember, conc, self, behav, 
                           learn, friend, change),
  
  disability_ch = mapply(any,
                         behav, play, learn, comm, fine_motor, walking, hearing, seeing), 
  
  disability2_ch = mapply(any_by_row,
                          behav, play, learn, comm, fine_motor, walking, hearing, seeing)
)

#adjustments to vars
dat_all <- dat_all %>% mutate(
  #only in CH, if disablilty is na, then use disability2
  disability_ch = ifelse(is.na(disability_ch), disability2_ch, disability_ch),
  #make overall disability
  disability = mapply(any, disability_fs, disability_ch))