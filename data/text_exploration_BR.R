# Text Pre Processing and Exploration Brazil Data
# Henrique Sposito

# Let's load some packages

library(dplyr)
library(ggplot2)
library(ggthemes)
library(tm)
library(wordcloud)
library(wordcloud2)
library(mgsub)
library(pbapply)library(skmeans)
library(tidytext)
library(clue)
library(cluster)
library(lexicon)
library(radarchart)
library(readr)

# Some helper functions and options
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

pal <- brewer.pal(8,"Dark2")
stops <- stopwords('portuguese')

### Campaign

# Load data
load("~/GitHub/poldis/data/BR_campaign.rda")
# Rename variables
bcamp <- BR_Campaign %>% rename(doc_id = Title, text = Text) %>%
  arrange(doc_id, text, Speaker, Date)
# create a corpus from data frame
bcamp_corpus <- VCorpus(DataframeSource(bcamp))
# clean corpus = remove stopwords, punctuations and lower case
bcamp_corpus <- cleanCorpus(bcamp_corpus, stops)
# Get a DTM
bcamp_DTM  <- DocumentTermMatrix(bcamp_corpus)
bcamp_DTMm <- as.matrix(bcamp_DTM)
# Get most frequent terms
bcamp_WFM <- data.frame(term = names(colSums(bcamp_DTMm)),
                       freq = colSums(bcamp_DTMm))
bcamp_WFM <- bcamp_WFM[order(bcamp_WFM$freq, decreasing = T),]
rownames(bcamp_WFM) <- NULL
head(bcamp_WFM, 30)
# Not very helpful as expected
# Let's get a bigram wordcloud
bcampt <- VCorpus(DataframeSource(bcamp))
bcampt <- cleanCorpus(bcampt, stops)
bcampt <- tidy(bcampt)
bcampt <- bcampt %>%
  unnest_tokens(bigram, text , token = "ngrams", n = 3, n_min = 2) %>%
  dplyr::count(bigram, sort = TRUE) %>%
  ungroup()
# Let's plot the 50 most common bigrams
bcampt %>% with(wordcloud(bigram, n, random.order = FALSE, max.words = 50, colors=pal))
# Interesting, some states, some president and candidates names and so on.
# Shall we look at sentiment for once?
# Let's look by speaker for the last 2 elections
br_camp_sp <- bcamp %>% filter(Date > "2013")
summary(as.factor(br_camp_sp$Speaker))
# Four speakers Bolsonaro, Haddad, Dilma and Aecio
# Let's aggregate by speakers and transform into a corpus
br_camp <-  aggregate(br_camp_sp$text, list(br_camp_sp$Speaker), paste, collapse =" ")
br_camp <- rename(br_camp, doc_id = "Group.1", text = "x")
br_camp_corpus <- VCorpus(DataframeSource(br_camp))
br_camp_corpus <- tm_map(br_camp_corpus, content_transformer(tryTolower))
br_camp_corpus <- tidy(br_camp_corpus)
# get word frequencies
br_camp_corpus_wf <- br_camp_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Inner join with NRC sentiment lexicon, however, lexicon is in English...
# The authors argue that it does a good job in other languages when translated, so, let's try.
# I download the NRC dictionary data from the syuzhet package here
# (https://github.com/mjockers/syuzhet/blob/master/R/sysdata.rda).
# Then, I keep only portuguese observations and add as internal data in the package.
load("~/GitHub/poldis/R/sysdata.rda")
br_camp_corpus_sent <- inner_join(br_camp_corpus_wf, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
# normalized values by numbers of documents for speaker in dataset
summary(as.factor(br_camp_sp$Speaker))
norm_value_aecio <- data.frame(norm_value = br_camp_corpus_sent$value[1:10]/27)
norm_value_bolsonaro <- data.frame(norm_value = br_camp_corpus_sent$value[11:20]/29)
norm_value_dilma <- data.frame(norm_value = br_camp_corpus_sent$value[21:30]/23)
norm_value_haddad <- data.frame(norm_value = br_camp_corpus_sent$value[31:40]/22)
norm_value <- rbind(norm_value_aecio, norm_value_bolsonaro, norm_value_dilma, norm_value_haddad)
br_camp_corpus_sent <- cbind(br_camp_corpus_sent, norm_value)
# plot values
ggplot(br_camp_corpus_sent, aes(id, norm_value, fill = sentiment)) +
  geom_bar(position="stack", stat="identity")
# Bolsonaro does use some carried words more than othrs in all senses.

### Intreviews

# Load data


