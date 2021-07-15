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

# Get lexicons for sentiment
# Both NRC and Afinn are in English...
# The for NRC authors argue that it does a good job in other
# languages when translated, so, let's try.
# I download the NRC dictionary data from the syuzhet package here
# (https://github.com/mjockers/syuzhet/blob/master/R/sysdata.rda).
# Then, I keep only portuguese observations and add as internal data in the package.
load("~/GitHub/poldis/R/sysdata.rda")

# Shall we try to translate Afinn to portuguese even if it might leed to some biasis?
# The translations was done with the str_translate() function.
# The Afinn_pt lexicon in portuguese was added to the package as internal data.
load("~/GitHub/poldis/R/sysdata.rda")

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
# I will try to use the Afinn lexicon because some camapign texts are short and others long,
# so I would like to get simply positive versus negative variations among speakers.
# Let's aggregate data by speaker
bcamp$Speaker <- paste0(bcamp$Speaker, "_", bcamp$Date) # get speaker year
bcamp_sent <-  aggregate(bcamp$text, list(bcamp$Speaker), paste, collapse =" ")
# Clean and transfrom into a corpus
bcamp_sent <- rename(bcamp_sent, doc_id = "Group.1", text = "x")
bcamp_sent_corpus <- VCorpus(DataframeSource(bcamp_sent))
# Get into tidy and get some word frequencies
bcamp_sent_tidy <- tidy(bcamp_sent_corpus)
bcamp_sent_wf <- bcamp_sent_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Now, the clear issue, the "Afinn" lexicon is in English, but ...
bcamp_sent <- inner_join(bcamp_sent_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
bcamp_sent$obs <- unname(summary(as.factor(bcamp$Speaker))) # get number of obs for each for normalization
bcamp_sent$n_value <- bcamp_sent$value/bcamp_sent$obs  # normalized by obs in dataset
bcamp_sent$date <- paste0(stringr::str_extract(bcamp_sent$id, "[0-9]{4}"))
bcamp_sent$id <- stringr::str_replace_all(bcamp_sent$id, "_[0-9]{4}", "")

# Let's make a pretty plot
camp_af <- ggplot(bcamp_sent, aes(x = date, y = n_value , fill = id)) +
  geom_line(aes(group = id)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Campaign") +
  theme_fivethirtyeight()

# In case you want explore a bit further with NRC
bcamp_sent_nrc <- inner_join(bcamp_sent_wf, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
obs <- summary(as.factor(bcamp$Speaker)) # get number of obs for each for normalization
# Get obs for each sentiment and speaker to work properly (10 sentiments for each speaker year)
rep_obs <- data.frame(rep("", 160))
rep_obs$obs[1:10] <- unname(summary(as.factor(bcamp$Speaker)))[1]
rep_obs$obs[11:20] <- unname(summary(as.factor(bcamp$Speaker)))[2]
rep_obs$obs[21:30] <- unname(summary(as.factor(bcamp$Speaker)))[3]
rep_obs$obs[31:40] <- unname(summary(as.factor(bcamp$Speaker)))[4]
rep_obs$obs[41:50] <- unname(summary(as.factor(bcamp$Speaker)))[5]
rep_obs$obs[51:60] <- unname(summary(as.factor(bcamp$Speaker)))[6]
rep_obs$obs[61:70] <- unname(summary(as.factor(bcamp$Speaker)))[7]
rep_obs$obs[71:80] <- unname(summary(as.factor(bcamp$Speaker)))[8]
rep_obs$obs[81:90] <- unname(summary(as.factor(bcamp$Speaker)))[9]
rep_obs$obs[91:100] <- unname(summary(as.factor(bcamp$Speaker)))[10]
rep_obs$obs[101:110] <- unname(summary(as.factor(bcamp$Speaker)))[11]
rep_obs$obs[111:120] <- unname(summary(as.factor(bcamp$Speaker)))[12]
rep_obs$obs[121:130] <- unname(summary(as.factor(bcamp$Speaker)))[13]
rep_obs$obs[131:140] <- unname(summary(as.factor(bcamp$Speaker)))[14]
rep_obs$obs[141:150] <- unname(summary(as.factor(bcamp$Speaker)))[15]
rep_obs$obs[151:160] <- unname(summary(as.factor(bcamp$Speaker)))[16]
bcamp_sent_nrc$obs <- rep_obs$obs # bind
bcamp_sent_nrc$n_value <- bcamp_sent_nrc$value/bcamp_sent_nrc$obs # get normalized value
bcamp_sent_nrc$date <- as.numeric(paste0(stringr::str_extract(bcamp_sent_nrc$id, "[0-9]{4}")))
ggplot(bcamp_sent_nrc, aes(x = reorder(id, date), y = n_value, fill = sentiment)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "Sentiment for Candidates in Campaign Remarks in Brazil",
       subtitle = "Normalized by observations for speaker in dataset",
       caption = "NRC sentiment lexicon") +
  theme_fivethirtyeight() +
  coord_flip()

### Intreviews, Debates and Oral Remarks

# Let's start with debates
# Load data
load("~/GitHub/Poldis/data/BR_debates.rda")
# Rename and wrangle
bdeb <- BR_debates %>% rename(doc_id = Title, text = Text) %>%
  arrange(doc_id, text, Speaker, Date)
# Aggregate data by speaker
bdeb$Speaker <- paste0(bdeb$Speaker, "_", stringr::str_extract(bdeb$Date, "^[0-9]{4}")) # get speaker year
bdeb_sent <-  aggregate(bdeb$text, list(bdeb$Speaker), paste, collapse =" ")
# Clean and transfrom into a corpus
bdeb_sent <- rename(bdeb_sent, doc_id = "Group.1", text = "x")
bdeb_sent_corpus <- VCorpus(DataframeSource(bdeb_sent))
bdeb_sent_corpus <- cleanCorpus(bdeb_sent_corpus, stops)
# Get into tidy and get some word frequencies
bdeb_sent_tidy <- tidy(bdeb_sent_corpus)
bdeb_sent_wf <- bdeb_sent_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# get sentiment
bdeb_sent_af <- inner_join(bdeb_sent_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
bdeb_sent_af$obs <- unname(summary(as.factor(bdeb$Speaker))) # get number of obs for each for normalization
bdeb_sent_af$n_value <- bdeb_sent_af$value/bdeb_sent_af$obs # normalized by obs in dataset
bdeb_sent_af$date <- paste0(stringr::str_extract(bdeb_sent_af$id, "[0-9]{4}"))
bdeb_sent_af$id <- stringr::str_replace_all(bdeb_sent_af$id, "_[0-9]{4}", "")
# Plot
deb_af <- ggplot(bdeb_sent_af, aes(x = date, y = n_value , fill = id)) +
  geom_line(aes(group = id)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Debates") +
  theme_fivethirtyeight()

# Let's do the same for interviews
load("~/GitHub/Poldis/data/BR_interviews.rda")
# Rename and wrangle
bint <- BR_Interviews %>% rename(doc_id = Title, text = Text) %>%
  arrange(doc_id, text, Speaker, Date)
# Aggregate data by speaker
bint$Speaker <- paste0(bint$Speaker, "_", stringr::str_extract(bint$Date, "^[0-9]{4}")) # get speaker year
bint_sent <-  aggregate(bint$text, list(bint$Speaker), paste, collapse =" ")
# Clean and transfrom into a corpus
bint_sent <- rename(bint_sent, doc_id = "Group.1", text = "x")
bint_sent_corpus <- VCorpus(DataframeSource(bint_sent))
bint_sent_corpus <- cleanCorpus(bint_sent_corpus, stops)
# Get into tidy and get some word frequencies
bint_sent_tidy <- tidy(bint_sent_corpus)
bint_sent_wf <- bint_sent_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# get sentiment
bint_sent_af <- inner_join(bint_sent_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
bint_sent_af$obs <- unname(summary(as.factor(bint$Speaker))) # get number of obs for each for normalization
bint_sent_af$n_value <- bint_sent_af$value/bint_sent_af$obs # normalized by obs in dataset
bint_sent_af$date <- paste0(stringr::str_extract(bint_sent_af$id, "[0-9]{4}"))
bint_sent_af$id <- stringr::str_replace_all(bint_sent_af$id, "_[0-9]{4}", "")
# Plot
int_af <- ggplot(bint_sent_af, aes(x = date, y = n_value , fill = id)) +
  geom_line(aes(group = id)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Interviews") +
  theme_fivethirtyeight()

# Let's do the same for oral remarks
load("~/GitHub/Poldis/data/BR_oral.rda")
# Rename and wrangle
boral <- BR_oral %>% select(date, presid, text)
# Aggregate data by speaker
boral$Speaker <- paste0(boral$presid, "_", boral$date) # get speaker year
boral_sent <-  aggregate(boral$text, list(boral$Speaker), paste, collapse =" ")
# Clean and transfrom into a corpus
boral_sent <- rename(boral_sent, doc_id = "Group.1", text = "x")
boral_sent_corpus <- VCorpus(DataframeSource(boral_sent))
# dataset is too big to clean with cleancorpus and can cause computer to crash...
# since we are doing sentiment and no cleaning is really needed for now...
# Get into tidy and get some word frequencies
boral_sent_tidy <- tidy(boral_sent_corpus)
boral_sent_wf <- boral_sent_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Get sentiment
boral_sent_af <- inner_join(boral_sent_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
boral_sent_af$obs <- unname(summary(as.factor(boral$Speaker))) # get number of obs for each for normalization
boral_sent_af$n_value <- boral_sent_af$value/boral_sent_af$obs  # normalized by obs in dataset
boral_sent_af$date <- paste0(stringr::str_extract(boral_sent_af$id, "[0-9]{4}"))
boral_sent_af$id <- stringr::str_replace_all(boral_sent_af$id, "_[0-9]{4}", "")
# Plot
oral_af <- ggplot(boral_sent_af, aes(x = date, y = n_value , fill = id)) +
  geom_line(aes(group = id)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Speeches") +
  theme_fivethirtyeight()

# Compare all the graphs side-by-side
gridExtra::grid.arrange(camp_af, int_af, deb_af, oral_af, ncol=2,
                        top = "Comparing Speakers in Different Settings across Time for Brazil",
                        bottom = "Sentiment generated with the Afinn lexicon and values normalized observations for speaker in data.")

# Okay, I do not think there is apoint in going futher. I will stop here and move to analysis.
# All this to say, the types of conclusions we can make using sentiment, word frequencies and
# unsupervised text modelling are limited. Though some might be used to support the types of
# arguments abouthow some politicians discourses is more intense and non neutral languege at
# higher frequencies, this can only be said for some settings and, perhaps, for some cases.
