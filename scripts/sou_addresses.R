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
require(readxl)

sou_complete <- data.frame()
files <- list.files(pattern=".txt")
for (f in files){
  name <- f
  year<- as.numeric(substring(name, first = 1, last = 4))
  temp <- read.table(f, header = F, sep = "\n", quote = "")
  temp <- temp %>% as.data.frame() %>% 
    rename("text" = 1) %>% 
    mutate(text = as.character(text)) %>%
    mutate(paragraph = row_number()) %>%
    mutate(Year = year)
  sou_complete <- rbind(sou_complete, temp)
}

tab <- read_xlsx("stateofunion.xlsx")
left_join(sou_complete, tab, by = "Year")
