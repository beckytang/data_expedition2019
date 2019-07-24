#######################################################################
#'
#' Format data for transcripts of the first democratic debates 
#' 
#######################################################################


###############
### Night 1 ###
###############

n1 <- read.delim("data/dem_debate_night1.txt",header = F,col.names = "text",stringsAsFactors = F) 

#the NYT site has some adds that get included with the download
ad_lines <- c(6:7,11:13,23:25)
n1 <- n1 %>% 
  filter(!(row_number() %in% ad_lines))

#extract speaker data and format dataframe 
n1 <- n1 %>% 
  mutate(speach_block = row_number(),
         speaker = str_extract(text,"^(.)*: "),
         text = str_remove(text,speaker),
         speaker = speaker %>% str_sub(1,-3),
         night = 1) %>% 
  select(night,speach_block,speaker,text) %>% 
  as_tibble()
n1

###############
### Night 2 ###
###############

n2 <- read.delim("data/dem_debate_night2.txt",header = F,col.names = "text",stringsAsFactors = F) 

ad_lines <- c(9:10,15:17,40:42)
n2 <- n2 %>% 
  filter(!(row_number() %in% ad_lines))

#' data are formated with the speaker's name on a separate line, 
#' speach_block identifies lines from the same speaker without interruption 
n2 <- n2 %>% 
  mutate(author_lines = text %>% str_detect(":$"),
         speach_block = cumsum(author_lines)) %>% 
  group_by(speach_block) %>% 
  summarise(speaker = first(text),
            text = str_c(text[2:n()],collapse = " ")) %>% 
  mutate(speaker = str_remove(speaker,":"),
         night = 2) %>% 
  ungroup() %>% 
  select(night,speach_block,speaker,text)

############
### Save ###
############

write_csv(rbind(n1,n2),"data/dem_debates.csv")
