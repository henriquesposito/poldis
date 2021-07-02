# Text Pre Processing and Exploration US Data
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
stops <- stopwords('english')

### Let's start with one dataset at a time
load("~/GitHub/Poldis/data/US_campaign.rda")
ucamp <- US_campaign

# Get variable names standardized and drop source
ucamp <- ucamp %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
# create a corpus from data frame
camp_corpus <- VCorpus(DataframeSource(ucamp))
# clean corpus = remove stopwords, punctuations and lower case
camp_corpus <- cleanCorpus(camp_corpus, stops)

# Let's explore word frequencies of the datasets as a whole, but first we need to get term document matrixes for all datasets
camp_DTM  <- DocumentTermMatrix(camp_corpus)
camp_DTMm <- as.matrix(camp_DTM)

# Get most frequent terms
camp_WFM <- data.frame(term = names(colSums(camp_DTMm)),
                   freq = colSums(camp_DTMm))
camp_WFM <- camp_WFM[order(camp_WFM$freq, decreasing = T),]
rownames(camp_WFM) <- NULL
head(camp_WFM, 30)
# Very generic 30 most frequent terms...

# Instead, let's look at bigrams and trigrams
# Let's also start to use tidytext as sizes become too large for base
# get text only
tcamp <- VCorpus(VectorSource(ucamp$text))
tcamp <- cleanCorpus(tcamp, stops)
tcamp <- tidy(tcamp)
tcampt <- tcamp %>%
  unnest_tokens(bigram, text , token = "ngrams", n = 3, n_min = 2) %>%
  dplyr::count(bigram, sort = TRUE) %>%
  ungroup()
tcampt
# A bit more informative...
# Let's plot the 50 most common bigrams
tcampt %>% with(wordcloud(bigram, n, random.order = FALSE, max.words = 50, colors=pal))
# Some make sures, tax cuts, new jobs and more. Do you think this differes for the other settings?

# For the sake of it, let's take a look at sentiment for all texts toegther, that is, are texts for debates
# mostly positive or negatives?
camp_DTMt <- tidy(camp_DTM)
camp_sent <- camp_DTMt %>% select(term, count)
camp_sent <- aggregate(count ~ term, data = camp_sent, FUN=sum)
camp_sent <- camp_sent %>% arrange(desc(count))
camp_sentscr <- camp_sent %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  arrange(desc(count))
# Okay, but let's try to quantify this
csent_scr_pos <- camp_sentscr %>% select(count, sentiment) %>% filter(sentiment == "positive")
csent_scr_neg <- camp_sentscr %>% select(count, sentiment) %>% filter(sentiment == "negative")
sum(csent_scr_pos$count)/sum(csent_scr_neg$count) # Mostly positive overall by almost a factor of two

# How about if we try and see which sentiments were overall present
camp_sentnrc <- camp_sent %>%
  inner_join(get_sentiments("nrc"), by = c(term = "word")) %>%
  arrange(desc(count))
camp_sentnrc # note some words get multiple sentiments
# Let's remove positive and negative
camp_sentnrc <- camp_sentnrc %>% filter(sentiment != "positive") %>% filter(sentiment != "negative")
camp_sentnrc <- aggregate(count ~ sentiment, data = camp_sentnrc, FUN=sum)
camp_sentnrc # Trust appears the most followed by anticipatons, joy and fear
# Let's plot this
chartJSRadar(scores = camp_sentnrc,
             labs = camp_sentnrc$sentiment,
             labelSize = 20, showLegend = FALSE, main = "Sentiment in All Campaign Texts")

# Since we are interested in comparisons, what if we broadly compare two speakers as Trump and Obama for example
obama_camp <- ucamp %>% filter(speaker == "Barack Obama")
summary(obama_camp)
trump_camp <- ucamp %>% filter(speaker == "Donald J. Trump")
summary(trump_camp)
# Many more obs for Obama...
# Let's clean them
obama_corpus <- VCorpus(DataframeSource(obama_camp))
obama_corpus <- cleanCorpus(obama_corpus, stops)
obama_DTM  <- DocumentTermMatrix(obama_corpus)
obama_DTMm <- as.matrix(obama_DTM)
trump_corpus <- VCorpus(DataframeSource(trump_camp))
trump_corpus <- cleanCorpus(trump_corpus, stops)
trump_DTM  <- DocumentTermMatrix(trump_corpus)
trump_DTMm <- as.matrix(trump_DTM)
# Let's checkout word frequencies for both
obama_WFM <- data.frame(term = names(colSums(obama_DTMm)),
                       freq = colSums(obama_DTMm))
obama_WFM <- obama_WFM[order(obama_WFM$freq, decreasing = T),]
rownames(obama_WFM) <- NULL
head(obama_WFM, 30)
trump_WFM <- data.frame(term = names(colSums(trump_DTMm)),
                        freq = colSums(trump_DTMm))
trump_WFM <- trump_WFM[order(trump_WFM$freq, decreasing = T),]
rownames(trump_WFM) <- NULL
head(trump_WFM, 30)
# Not very infomative...
# Bigrams for each might help
t_obamac <- tidy(obama_corpus)
t_obamact <- t_obamac %>%
  unnest_tokens(bigram, text , token = "ngrams", n = 3, n_min = 2) %>%
  dplyr::count(bigram, sort = TRUE) %>%
  ungroup()
t_trumpc <- tidy(trump_corpus)
t_trumpct <- t_trumpc %>%
  unnest_tokens(bigram, text , token = "ngrams", n = 3, n_min = 2) %>%
  dplyr::count(bigram, sort = TRUE) %>%
  ungroup()
# Let's compare
t_obamact %>% with(wordcloud(bigram, n, random.order = FALSE, max.words = 50, colors=pal)) # midle class, tax cuts, health care
t_trumpct %>% with(wordcloud(bigram, n, random.order = FALSE, max.words = 50, colors=pal)) # Hillary clinton, make america, america great
# Shall we get sentiment? Let's do it
obama_sent <- tidy(obama_DTM)
obama_sent <- obama_sent %>%
  inner_join(get_sentiments("nrc"), by = c(term = "word")) %>%
  group_by(sentiment) %>%
  summarize(obama_sent = sum(count))
obama_sent$obama_sent <- obama_sent$obama_sent/length(obama_camp$doc_id) # normalize scores for comparison
trump_sent <- tidy(trump_DTM)
trump_sent <- trump_sent %>%
  inner_join(get_sentiments("nrc"), by = c(term = "word")) %>%
  group_by(sentiment) %>%
  summarize(trump_sent = sum(count))
trump_sent$trump_sent <- trump_sent$trump_sent/length(trump_camp$doc_id) # normalize scores for comparisons
sent_ob_tr <- data.frame(cbind(obama_sent, trump_sent)) %>% select(-sentiment.1)
sent_ob_tr <- data.frame(sent_ob_tr[,-1], row.names=sent_ob_tr[,1])
chartJSRadar(scores = sent_ob_tr,
             labs = rownames(sent_ob_tr),
             labelSize = 20, showLegend = TRUE, main = "Obama versus Trump Sentiments in Campaign Compared (Normalized by the number of documents for each speaker)")
# Interesting, they do not seem all that different afterall...

### Debates

# load data
load("~/GitHub/Poldis/data/US_debates.rda")
udeb <- US_debates
# rename
udeb <- udeb %>% rename(doc_id = Title, text = Text, speaker = Speakers, date = Date) %>%
  arrange(doc_id, text, speaker, date)
deb_corpus <- VCorpus(DataframeSource(udeb))
deb_corpus <- cleanCorpus(deb_corpus, stops)
# DTM
deb_DTM  <- DocumentTermMatrix(deb_corpus)
deb_DTMm <- as.matrix(deb_DTM)
# Word frequencies
deb_WFM <- data.frame(term = names(colSums(deb_DTMm)),
                      freq = colSums(deb_DTMm))
deb_WFM <- deb_WFM[order(deb_WFM$freq, decreasing = T),]
rownames(deb_WFM) <- NULL
head(deb_WFM, 30)
# Similar generic words...
# Bigrams?
tdeb <- VCorpus(VectorSource(udeb$text))
tdeb <- cleanCorpus(tdeb, stops)
tdeb <- tidy(tdeb)
tdebt <- tdeb %>%
  unnest_tokens(bigram, text , token = "ngrams", n = 3, n_min = 2) %>%
  dplyr::count(bigram, sort = TRUE) %>%
  ungroup()
# 50 most common bigrams
tdebt %>% with(wordcloud(bigram, n, random.order = FALSE, max.words = 50, colors=pal))
# Yeah, very generic... Is it all that different from most bigrams from campaigns?
# Since looking at sentiment for all texts may not seem that helpful, how about we compare two presidents?
# How about we compare George W. Bush and Al Gore (2000 elections)
# as well as Bill Clinton and George Bush (1992 elections)?
# Let's select only the year of interest
deb_comp <- udeb[grep("^1992|^2000", udeb$Date),]
# Let's remove Ross Perot from sample
deb_comp <- deb_comp[grep("Ross Perot", deb_comp$Speakers, invert = TRUE),]
# Agregate these by speakers
deb_comp <-  aggregate(deb_comp$Text, list(deb_comp$Speakers), paste, collapse =" ")
# Clean and transfrom into a corpus (I use the SMART dictionary for removing stopwords,
# more agreesive, here for testing)
deb_comp <- rename(deb_comp, doc_id = "Group.1", text = "x")
deb_comp_corpus <- VCorpus(DataframeSource(deb_comp))
deb_comp_corpus <- cleanCorpus(deb_comp_corpus, stopwords("SMART"))
deb_comp_DTM <- TermDocumentMatrix(deb_comp_corpus)
deb_comp_DTMm <- as.matrix(deb_comp_DTM)
# Let's first look at word frequencies and associations for each speaker
deb_compt <- tidy(deb_comp_corpus)
deb_comp_wf <- deb_compt %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
w_bush <- deb_comp_wf %>% filter(id == " George W. Bush") %>% slice_head(n = 10)
b_clinton <- deb_comp_wf %>% filter(id == " Bill Clinton") %>% slice_head(n = 10)
al_gore <- deb_comp_wf %>% filter(id == " Al Gore") %>% slice_head(n = 10)
bush <- deb_comp_wf %>% filter(id == " George Bush") %>% slice_head(n = 10)
top_terms <- cbind(w_bush, b_clinton, al_gore, bush)
# Not super helpful...
# How about we do sentiment for each speaker?
w_bush_sent <- deb_comp_wf %>% filter(id == " George W. Bush")
w_bush_sent <- inner_join(w_bush_sent, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(w_bush_sent = sum(n))
b_clinton_sent <- deb_comp_wf %>% filter(id == " Bill Clinton")
b_clinton_sent <- inner_join(b_clinton_sent, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(b_clinton_sent = sum(n))
al_gore_sent <- deb_comp_wf %>% filter(id == " Al Gore")
al_gore_sent <- inner_join(al_gore_sent, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(al_gore_sent = sum(n))
bush_sent <- deb_comp_wf %>% filter(id == " George Bush")
bush_sent <- inner_join(bush_sent, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(bush_sent = sum(n))
sent_deb_sp <- data.frame(cbind(w_bush_sent, b_clinton_sent, al_gore_sent, bush_sent)) %>%
  select(-c(sentiment.1, sentiment.2, sentiment.3))
sent_deb_sp <- data.frame(sent_deb_sp[,-1], row.names=sent_deb_sp[,1])
chartJSRadar(scores = sent_deb_sp,
             labs = rownames(sent_deb_sp),
             labelSize = 20, showLegend = TRUE, main = "Sentiments in Debates Compared")
# Interesting, they do not seem all that different afterall...



### Interviews

uint <- US_interviews

uint <- uint %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
int_corpus <- VCorpus(DataframeSource(uint))
int_corpus <- cleanCorpus(int_corpus, stops)

int_DTM  <- DocumentTermMatrix(int_corpus)
int_DTMm <- as.matrix(int_DTM)

int_WFM <- data.frame(term = names(colSums(int_DTMm)),
                      freq = colSums(int_DTMm))
int_WFM <- int_WFM[order(int_WFM$freq, decreasing = T),]
rownames(int_WFM) <- NULL
head(int_WFM, 30)
# Similar generic words...

### Oral

uora <- US_oral

uora <- uora %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
ora_corpus <- VCorpus(DataframeSource(uora))
ora_corpus <- cleanCorpus(ora_corpus, stops)

ora_DTM  <- DocumentTermMatrix(ora_corpus)
ora_DTMt <- tidy(ora_DTM) # Issue with size here hence why we conveted to tidy

ora_WFM <- ora_DTMt %>% select(term, count)
ora_WFM <- aggregate(count ~ term, data = ora_WFM, FUN=sum)
ora_WFM <- ora_WFM %>% arrange(desc(count))
head(ora_WFM, 30)
# Okay, this has not been all that helpful..

# get sentiment easy with tidy
ora_sent <- ora_DTMt %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  arrange(desc(count))
