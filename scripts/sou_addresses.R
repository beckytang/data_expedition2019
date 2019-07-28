#######################################################################
#'
#' Format data for State of Union addresses 
#' 
#######################################################################

#############
### Setup ###
#############
setwd("~/data_expedition2019/data/state_of_union")
require(tidyverse)

sou_complete <- data.frame()
files <- list.files(pattern=".txt")
for (f in files){
  name <- f
  year<- substring(name, first = 1, last = 4)
  prez <- substring(name, first = 5, last = nchar(name)-4)
  temp <- read.table(f, header = F, sep = "\n", quote = "")
  temp <- temp %>% as.data.frame() %>% 
    rename("text" = 1) %>% 
    mutate(text = as.character(text)) %>%
    mutate(paragraph = row_number()) %>%
    mutate(president = prez, year = year)
  sou_complete <- rbind(sou_complete, temp)
}
