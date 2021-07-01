# Text Pre Processing and Exploration US Data
# Henrique Sposito

# Let's load some packages and helper functions first

library(dplyr)
library(ggplot2)
library(ggthemes)
library(tm)
library(wordcloud)
library(wordcloud2)
library(mgsub)
library(pbapply)
library(skmeans)
library(tidytext)
library(clue)
library(cluster)
library(lexicon)
library(radarchart)
library(readr)

tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  #corpus <- tm_map(corpus, content_transformer(qdap::replace_contraction))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# First, let's load datasets for US
ucamp <- US_campaign
udeb <- US_debates
uora <- US_oral
uint <- US_interviews

# Get variable names standardized and drop source
ucamp <- ucamp %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
# create a corpus from data frame
camp_corpus <- VCorpus(DataframeSource(ucamp))
# clean corpus = remove stopwords, punctuations and lower case
stops <- stopwords('english')
camp_corpus <- cleanCorpus(camp_corpus, stops)

# Repeat the same operations for all datasets
udeb <- udeb %>% rename(doc_id = Title, text = Text, speaker = Speakers, date = Date) %>%
  arrange(doc_id, text, speaker, date)
deb_corpus <- VCorpus(DataframeSource(udeb))
deb_corpus <- cleanCorpus(deb_corpus, stops)

uora <- uora %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
ora_corpus <- VCorpus(DataframeSource(uora))
ora_corpus <- cleanCorpus(ora_corpus, stops)

uint <- uint %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
int_corpus <- VCorpus(DataframeSource(uint))
int_corpus <- cleanCorpus(int_corpus, stops)

# Let's explore word frequencies of the datasets as a whole, but first we need to get term document matrixes for all datasets
camp_DTM  <- DocumentTermMatrix(camp_corpus)
camp_DTM <- as.matrix(camp_DTM)

deb_DTM  <- DocumentTermMatrix(deb_corpus)
deb_DTM <- as.matrix(deb_DTM)

ora_DTM  <- DocumentTermMatrix(ora_corpus)
ora_DTM <- as.matrix(ora_DTM) # Issue with size here
