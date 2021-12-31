#####TEXT MINING

###Used Packages #####
library(purrr)
library(rvest)
library(dplyr)
library(rlang)
library(rvest)
library(tidyverse)
library(stringr)
library(purrr)
library(dplyr)
library(Rcrawler)
library(data.table)
library(tidytext)
library(tibble)
library(ggplot2)
library(tidyr)
library(textdata)
library(wordcloud)
library(reshape2)
library(scales)
library(udpipe)
library(xlsx)

##### variable explanation
# GN: Table with General Newspapers (incl. Title, Date, Content, Link, Source..)
# GNWords: Long table with every word in separate row
# GNTitle: Long table with every word from title in separate row

#### 
#### Remove stop words Custom Stop Words  # needs to be run ####
customwords <- c("january",
                 "february",
                 "march",
                 "april",
                 "may",
                 "june",
                 "july",
                 "aug",
                 "august",
                 "september",
                 "minute",
                 "hour",
                 "p.m",
                 "day",
                 "weeks",
                 "month",
                 "year",
                 "billion",
                 "percent",
                 "million",
                 "getty",
                 "images",
                 "styln",
                 "advertisement",
                 "font",
                 "px",
                 "image",
                 "copyright",
                 "rem",
                 "share",
                 "width",
                 "caption",
                 "bottom",
                 "video",
                 "news",
                 "data",
                 "facebook",
                 "twitter",
                 "whatsapp",
                 "line",
                 "css",
                 "min",
                 "briefing",
                 "height",
                 "dr",
                 "covid",
                 "coronavirus",
                 "corona",
                 "covid-19",
                 "chains",
                 "pandemic",
                 "positive", 
                 "virus", "block", "helvetica", "arial", "sans", "parcel","select","magazine", "newspaper", "follow")




custom_stop_words <- bind_rows(tibble(word = customwords, 
                                      lexicon = c("custom")), 
                               stop_words)




# GN
GNwSW <- GNWords %>% # GN without StopWords incl. custom Stop Words
  anti_join(custom_stop_words) # Delete Stop Words

SortedGN <- GNwSW %>%
  count(word, sort = TRUE) 

GN_1_wSW <- GNFirstWords %>% 
  anti_join(custom_stop_words)
GN_2_wSW <- GNSecondWords %>% 
  anti_join(custom_stop_words)
GN_3_wSW <- GNThirdWords %>% 
  anti_join(custom_stop_words)

#### Frequency Plot Function ####


freq <- function(freq,i){
  dev.new(width=5, height=12)
  freq %>% # Plot most frequent words of all texts
    count(word, sort = TRUE) %>%
    top_n(i) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n)) +
    geom_col() +
    geom_text(aes(label=n),hjust=-0.18)+
    xlab(NULL) +
    coord_flip()
}

#### Frequency Plots ####
freq(SortedGN, 15) ## 
freq(GN_1_wSW  ,15) ## 
freq(GN_2_wSW  ,15) ## 
freq(GN_3_wSW  ,15) ## 

###### Word Cloud ####


dev.new(width=5, height=4)
GNwSW %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 50)) #displays top 50 results

dev.new(width=5, height=4)
GN_1_wSW %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

dev.new(width=5, height=4)
GN_2_wSW %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))

dev.new(width=5, height=4)
GN_3_wSW %>% 
  count(word) %>%
  with(wordcloud(word, n, max.words = 50))


###### Word Clouds with sentiments ######
## 100 words ####


dev.new(width=5, height=4)
GNwSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 100)

dev.new(width=5, height=4)
GNwSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 100)

dev.new(width=5, height=4)
GN_1_wSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 100)

dev.new(width=5, height=4)
GN_2_wSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 100)

dev.new(width=5, height=4)
GN_3_wSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 100)


#### 50 words ####


dev.new(width=5, height=4)
GNwSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 50)

dev.new(width=5, height=4)
GN_1_wSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 50)

dev.new(width=5, height=4)
GN_2_wSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 50)

dev.new(width=5, height=4)
GN_3_wSW %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray60"),
                   max.words = 50)


##### Sentiments. Merging time as as one source as opposed to 3 seperate ones  ####
GNFirstWords$time <- 1
GNSecondWords$time <- 1
GNThirdWords$time <- 1
GNall <- rbind(GNFirstWords, GNSecondWords, GNThirdWords)
GNall$week <- strftime(GNall$date, format = "%V")

GNall$paper <- c("GN")


#### Sentiment Analysis with Bing by Date #####


GN_sentiment <- GNall %>%
  inner_join(get_sentiments("bing")) %>%
  count(time, index = date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

GN_sentiment$paper <- c("General Newspaper")
dev.new(width=5, height=4)
ggplot(GN_sentiment, aes(index, sentiment, fill = time)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~paper, ncol = 2, scales = "free_x")




#### Sentiment Analysis with Bing by Week #####

GN_sentiment <- GNall %>%
  inner_join(get_sentiments("bing")) %>%
  count(time, index = week, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

GN_sentiment$paper <- c("General Newspaper")
dev.new(width=5, height=4)
ggplot(GN_sentiment, aes(index, sentiment, fill = time)) +
  geom_col(show.legend = TRUE) +
  facet_wrap(~paper, ncol = 2, scales = "free_x")

#### Sentiment Analysis with Bing by Continent #####

GN_sentiment <- GNall %>%
  inner_join(get_sentiments("bing")) %>%
  count(source, index = time, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
write.xlsx(GN_sentiment,'GNSentimentsbySource.xlsx')

GN_sentiment_americas <- subset(GN_sentiment, source %in% c("USA TODAY", "Los Angeles Times", "Toronto Star", "National Post"))
GN_sentiment_europe <- subset(GN_sentiment, source %in% c("The Irish Times", "TheJournal.ie", "BBC News", "The Guardian"))
GN_sentiment_australasia <- subset(GN_sentiment, source %in% c("Stuff.co.nz", "New Zealand Herald", "The Sydney Morning Herald", "9News", "The Indian Express", "The Hindu"))


dev.new(width=5, height=4)
ggplot(GN_sentiment_americas, aes(index, sentiment, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 3, scales = "free_x")

dev.new(width=5, height=4)
ggplot(GN_sentiment_europe, aes(index, sentiment, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 3, scales = "free_x")

dev.new(width=5, height=4)
ggplot(GN_sentiment_australasia, aes(index, sentiment, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~source, ncol = 3, scales = "free_x")



#### Sentiment Analysis different ways comparison. Not used ####

Yes1

bing_and_nrc <- bind_rows(GNall %>% 
                            inner_join(get_sentiments("bing")) %>%
                            mutate(method = "Bing et al."),
                          GNWords %>% 
                            inner_join(get_sentiments("nrc") %>% 
                                         filter(sentiment %in% c("positive", 
                                                                 "negative"))) %>%
                            mutate(method = "NRC")) %>%
  count(method, date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

dev.new(width=5, height=4)

bind_rows(afinn, 
          bing_and_nrc) %>%
  ggplot(aes(date, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


### Most common pos. & neg. words ####

posnegwords <- function(Data){
bing_word_counts <- Data %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

dev.new(width=6, height=5)
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n),hjust=-0.3)+
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()
}

posnegwords(GNwSW)
posnegwords(GN_1_wSW)
posnegwords(GN_2_wSW)
posnegwords(GN_3_wSW)

#### bigrams ####

bigramfunction <- function(GN){
  GN_wNB <- GN
  GN_wNB$content <- gsub("\\d+", "", GN_wNB$content)
  GN_wNB$content <- gsub("Los Angeles Times", "", GN_wNB$content)
  GN_bigrams <- GN_wNB %>%
    unnest_tokens(bigram, content, token = "ngrams", n = 2)
  
  bigrams_separated <- GN_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")
  
  bigrams_filtered <- bigrams_separated %>%
    filter(!word1 %in% custom_stop_words$word) %>%
    filter(!word2 %in% custom_stop_words$word)
  
  GN_bigrams <- bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")
  
  return(GN_bigrams)}

#### freq function for bigrams ####
freq2 <- function(freq,i){
  dev.new(width=3, height=6)
  freq %>% # Plot most frequent words of all texts
    count(bigram, sort = TRUE) %>%
    top_n(i) %>%
    mutate(bigram = reorder(bigram, n)) %>%
    ggplot(aes(bigram, n)) +
    geom_text(aes(label=n),hjust=-0.19)+
    geom_col() +
    xlab(NULL) +
    coord_flip()
}

GN_bigrams<-bigramfunction(GN)
freq2(GN_bigrams,15)
GN_1_bigrams<-bigramfunction(GNFirst)
freq2(GN_1_bigrams,15)
GN_2_bigrams<-bigramfunction(GNSecond)
freq2(GN_2_bigrams,15)
GN_3_bigrams<-bigramfunction(GNThird)
freq2(GN_3_bigrams,15)




