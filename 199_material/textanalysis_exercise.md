Text Analysis
================
Your name here
October 3, 2019

In class exercise
=================

### Load the data

``` r
tweets <- tweets %>%
  filter(!str_detect(text, "^RT")) %>%   #get rid of retweets
  mutate(timestamp = ymd_hms(created_at)) %>%
  select("timestamp","screen_name", "text")
tweets %>%
  slice(1:3)
```

    ##             timestamp screen_name
    ## 1 2019-10-02 15:28:06    JoeBiden
    ## 2 2019-10-02 02:34:51    JoeBiden
    ## 3 2019-10-02 01:02:00    JoeBiden
    ##                                                                                                                                                                                                                                                                         text
    ## 1 .@DrBiden and I are sending our best wishes to @BernieSanders, Jane, and the whole Sanders family. Anyone who knows Bernie understands what a force he is. We are confident that he will have a full and speedy recovery and look forward to seeing him on the trail soon.
    ## 2                                                                                                                                                                                              President Trump is morally unfit to lead our country. https://t.co/r3SKYqK26K
    ## 3     President Trump asked a foreign government to interfere in our elections, and now he's spending $10 million on attack ads against me. It's clear that he is trying to influence the primary and pick his opponent.\n\nWhy? Because he knows I’ll beat him like a drum.

### Remove symbols and unnest into one-word tokens

``` r
remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- tweets %>%
  mutate(text = str_remove_all(text, remove_reg))%>%
  unnest_tokens(word, text, token = "tweets")
tidy_tweets %>%
  slice(1:5)
```

    ##             timestamp screen_name    word
    ## 1 2019-10-02 15:28:06    JoeBiden drbiden
    ## 2 2019-10-02 15:28:06    JoeBiden     and
    ## 3 2019-10-02 15:28:06    JoeBiden       i
    ## 4 2019-10-02 15:28:06    JoeBiden     are
    ## 5 2019-10-02 15:28:06    JoeBiden sending

``` r
counts <- tidy_tweets %>%
  count(word, sort = T)
counts %>%
  slice(1:10)
```

    ## # A tibble: 10 x 2
    ##    word      n
    ##    <chr> <int>
    ##  1 the    6213
    ##  2 to     5955
    ##  3 and    4145
    ##  4 a      2918
    ##  5 of     2794
    ##  6 in     2616
    ##  7 we     2421
    ##  8 for    2418
    ##  9 is     1841
    ## 10 our    1840

``` r
tidy_tweets <- tidy_tweets %>%
  filter(!word %in% stop_words $ word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))
counts <- tidy_tweets %>%
  count(word, sort = T)
counts %>%
  slice(1:10)
```

    ## # A tibble: 10 x 2
    ##    word          n
    ##    <chr>     <int>
    ##  1 people      683
    ##  2 president   670
    ##  3 trump       532
    ##  4 fight       461
    ##  5 care        457
    ##  6 country     452
    ##  7 health      446
    ##  8 plan        387
    ##  9 time        387
    ## 10 change      312

``` r
frequency_all <-  tidy_tweets %>%
  count(word, sort = T) %>%
  mutate(freq = n / sum(n)) 

ggplot(frequency_all %>% top_n(10, freq) %>%
         mutate(word = reorder(word, freq)), aes(x = word, y = freq))+
  geom_col()+ 
  coord_flip() 
```

![](textanalysis-ex_files/figure-markdown_github/word-freq-1.png)

``` r
## frequencies by candidate
frequency <- tidy_tweets %>%
  group_by(screen_name) %>%
  count(word, sort = T) %>%
  left_join(tidy_tweets %>%
              group_by(screen_name) %>%
              summarise(total = n()),
            by = "screen_name") %>%
  mutate(freq = n / total)


ggplot(frequency %>% 
         # choose candidate here
         filter(screen_name == "JoeBiden") %>% 
         top_n(10, freq) %>%
         mutate(word = reorder(word, freq)),
       aes(x = word, y = freq))+
  geom_col()+ 
  coord_flip() 
```

![](textanalysis-ex_files/figure-markdown_github/word-freq-2.png)

### Sentiment analysis

``` r
tidy_tweets %>%
  # choose candidate here
  filter(screen_name == "ewarren") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(sentiment) %>%
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative)
```

    ## # A tibble: 1 x 3
    ##   negative positive sentiment
    ##      <int>    <int>     <int>
    ## 1     1118     1130        12

``` r
bing_counts <- tidy_tweets %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = T) 
bing_counts %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x= word, y = n, fill = sentiment))+
  geom_col(show.legend = F) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y = "Contribution to sentiment", x = NULL) + 
  coord_flip()
```

![](textanalysis-ex_files/figure-markdown_github/bing-sentiment-1.png)

### Wordclouds

``` r
tidy_tweets %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 60,scale=c(3,.5)))
```

![](textanalysis-ex_files/figure-markdown_github/wordcloud-1.png)

### Comparing candidates: word frequency

``` r
frequency <- frequency %>%
  select(screen_name, word, freq) %>%
  spread(screen_name, freq) %>%
  arrange(BernieSanders, ewarren, JoeBiden, KamalaHarris)

ggplot(frequency, aes(JoeBiden, ewarren))+
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = T, vjust = 1.5)+
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels = percent_format())+
  geom_abline(color = "blue")
```

    ## Warning: Removed 11769 rows containing missing values (geom_point).

    ## Warning: Removed 11769 rows containing missing values (geom_text).

![](textanalysis-ex_files/figure-markdown_github/cand-freqs-1.png)

### Comparing candidates: word probabilities

``` r
# choose candidates
cands <- c("JoeBiden", "ewarren")

# create dataframe of log odds ratios
word_ratios <- tidy_tweets %>%
  filter(screen_name %in% cands) %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, screen_name) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>% # only consider more frequently uses words
  ungroup() %>%
  spread(screen_name, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. +1) / (sum(.) + 1))) %>%
  mutate(logratio = log(eval(parse(text = cands[1]))/eval(parse(text = cands[2])))) %>%
  select(word, logratio)%>%
  arrange(desc(logratio))

word_ratios 
```

    ## # A tibble: 691 x 2
    ##    word      logratio
    ##    <chr>        <dbl>
    ##  1 biden         4.38
    ##  2 #teamjoe      3.82
    ##  3 obamacare     3.49
    ##  4 rsvp          3.49
    ##  5 folks         3.32
    ##  6 soul          3.32
    ##  7 vision        3.18
    ##  8 de            3.06
    ##  9 code          3.02
    ## 10 educators     3.02
    ## # … with 681 more rows

``` r
word_ratios %>% 
  group_by(logratio < 0) %>%
  top_n(20, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio))%>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = F) +
  coord_flip() +
  ylab(paste("log odds ratio", cands[1], "/", cands[2])) +
  scale_fill_discrete(name = "", labels = c(cands[1], cands[2]))
```

![](textanalysis-ex_files/figure-markdown_github/word-usage-diff-1.png)

### n-grams

``` r
tweet_bigrams <- tweets %>%
  select(screen_name, text) %>%
  mutate(text = str_remove_all(text, remove_reg))%>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
```

``` r
my_stop_words <- tibble(word = c("http", "https"), lexicon = c("custom"))
custom_stop_words <- bind_rows(my_stop_words, stop_words)

bigrams_sep <- tweet_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") 
bigrams_filtered <- bigrams_sep %>%
  filter(!word1 %in% custom_stop_words$word,
         !word1 %in% str_remove_all(custom_stop_words$word, "'"),
         str_detect(word1, "[a-z]"), 
         !word2 %in% custom_stop_words$word,
         !word2 %in% str_remove_all(custom_stop_words$word, "'"),
         str_detect(word2, "[a-z]"))
```

``` r
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = T)
bigram_counts 
```

    ## # A tibble: 21,107 x 3
    ##    word1      word2        n
    ##    <chr>      <chr>    <int>
    ##  1 health     care       319
    ##  2 donald     trump      170
    ##  3 gun        violence   147
    ##  4 climate    change     144
    ##  5 middle     class      104
    ##  6 it’s       time        97
    ##  7 white      house       94
    ##  8 climate    crisis      75
    ##  9 structural change      75
    ## 10 fossil     fuel        73
    ## # … with 21,097 more rows

On your own
===========

``` r
sou <- sou %>%
  mutate(Text = as.character(Text))
```

1.  Finish the code below to create a tidytext dataframe called `sou_tidy`.

``` r
# sou_tidy <- sou %>%
#   unnest_tokens(output = word, input = ?)
```

Before we start calculating word counts or frequencies, let's go ahead and remove the annoying stop words. With the Twitter data, we had to do a little extra work to remove the stop words due to the format of tweets. With regular text, we can use the simple `anti_join()` function to remove the stop words from the tidytext: **The code below will not run until you finish the above sections. When you do, change eval=F to eval=T and execute it.**

``` r
sou_tidy <- sou_tidy %>%
  anti_join(stop_words, by = "word")
```

1.  What are the five words that are used most frequently across the two presidents?

``` r
# sou_tidy %>%
#   count(word, sort = T)
```

1.  Interestingly, the word "applause" is used a lot in the State of the Union addresses. That is quite odd, but there is a reason for it! Run the following code and explain why that is:

``` r
sou %>%
  filter(str_detect(Text,"Applause")) %>%
  select(Text) %>%
  slice(1:5)
```

    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Text
    ## 1                                                                                                                                                             Our economy is stronger when we harness the talents and ingenuity of striving, hopeful immigrants. And right now leaders from the business, labor, law enforcement, faith communities, they all agree that the time has come to pass comprehensive immigration reform. Now is the time to do it. Now is the time to get it done. [Applause] Now is the time to get it done.
    ## 2                                                                                                                                              In other words, we know what needs to be done. And as we speak, bipartisan groups in both Chambers are working diligently to draft a bill, and I applaud their efforts. So let's get this done. Send me a comprehensive immigration reform bill in the next few months, and I will sign it right away. And America will be better for it. Let's get it done. [Applause] Let's get it done.
    ## 3         Hadiya's parents, Nate and Cleo, are in this Chamber tonight, along with more than two dozen Americans whose lives have been torn apart by gun violence. They deserve a vote. They deserve a vote. [Applause] They deserve a vote. Gabby Giffords deserves a vote. The families of Newtown deserve a vote. The families of Aurora deserve a vote. The families of Oak Creek and Tucson and Blacksburg, and the countless other communities ripped open by gun violence, they deserve a simple vote. They deserve a simple vote.
    ## 4 We should follow the example of a North Miami woman named Desiline Victor. When Desiline arrived at her polling place, she was told the wait to vote might be 6 hours. And as time ticked by, her concern was not with her tired body or aching feet, but whether folks like her would get to have their say. And hour after hour, a throng of people stayed in line to support her, because Desiline is 102 years old. And they erupted in cheers when she finally put on a sticker that read, "I voted." [Applause] There's Desiline.
    ## 5                              As usual, our First Lady sets a good example. [Applause] Well—[applause]. Michelle's "Let's Move!" partnership with schools, businesses, local leaders has helped bring down childhood obesity rates for the first time in 30 years. And that's an achievement that will improve lives and reduce health care costs for decades to come. The Joining Forces alliance that Michelle and Jill Biden launched has already encouraged employers to hire or train nearly 400,000 veterans and military spouses.

1.  So we should remove the word "applause" from our tidytext. For similar reasons, we should remove the word "laughter". Create a custom list of stop words, and use it to create a new `sou_tidy` tidytext. Finally, display the ten most used words across the two presidents.

``` r
# my_stop_words <- tibble(word = c(?, ?), lexicon = "custom")
# custom_stop_words <- bind_rows(my_stop_words, stop_words)
# sou_tidy <- ?
```

The following code calculates the word frequencies of Obama vs the word frequencies of Trump in their State of the Union addresses. **The code below will not run until you finish the above sections. When you do, change eval=F to eval=T and execute it.**

``` r
pres_frequency <- sou_tidy %>%
  group_by(President) %>%
  count(word, sort = T) %>%
  left_join(sou_tidy %>%
              group_by(President) %>%
              summarise(total = n()),
            by = "President") %>%
  mutate(freq = n / total)
pres_frequency %>%
  slice(1:5)
```

1.  Using `pres_frequency`, create a plot of the top 20 most frequently used terms of Obama, and another plot of the top 20 most frequently used terms of Trump. Comment on the similarities/differences.

2.  Using the `bing` lexicon, analyze the sentiment of the State of the Unions across the two presidents. Looking at all the addresses, what is the most commonly used negative word, and what is the most commonly used positive word?

3.  Calculate the overall sentiment of Obama's addresses, and also calculate the overall sentiment of Trump's addresses. Which president is more positive/negative in sentiment? (Hint: use code similar to the Twitter data, but consider using the `group_by()` function instead of `filter()`.)

4.  Make a word cloud for the top 50 words used in all the State of the Union addresses. (Note, the first time you knit this, it may take awhile.)

5.  Lastly, we would appreciate any feedback you may have for us! Positive or negative, we welcome it all!
