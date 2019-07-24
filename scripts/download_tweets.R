######################################
#'
#' Collect tweets from various accounts 
#' for text analysis
#' 
######################################

#############
### Setup ###
#############

require(rtweet)
require(tidyverse)

######################
### Dem Candidates ###
######################

handles <- c("JoeBiden","BernieSanders","ewarren","KamalaHarris") #"SenKamalaHarris" is also used by Harris

dem.cand.tweets <- get_timelines(handles,n = 1200)
table(dem.cand.tweets$screen_name)

#check what rate limits are left
rate_limit() %>% 
  mutate(used = limit - remaining) %>% 
  arrange(-(used != 0)) 

#write_as_csv(dem.cand.tweets,"data/dem_cand_tweets.csv")

################
### Hashtags ###
################

#example code for how to get tweets by hashtag (or any other phrase)
hashtags <- c("#rstats","#USWNT")
ht.data <- map(hashtags,~search_tweets(.,n=100,include_rts = FALSE,type="popular")) %>% 
  do.call(what="rbind")

#write_as_csv(ht.data,"data/hashtag_tweets.csv")
