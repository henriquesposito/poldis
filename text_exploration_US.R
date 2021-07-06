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

# Let's start with one dataset at a time

### Campaign US

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
# Trump appears just to augment usage of some emotional words, but patterns do not seem to differ too much.

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
# Let's get the words and sentiment frequency for each speakers
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
# merge data
sent_deb_sp <- data.frame(cbind(w_bush_sent, b_clinton_sent, al_gore_sent, bush_sent)) %>%
  select(-c(sentiment.1, sentiment.2, sentiment.3))
sent_deb_sp <- data.frame(sent_deb_sp[,-1], row.names=sent_deb_sp[,1])
# plot
chartJSRadar(scores = sent_deb_sp,
             labs = rownames(sent_deb_sp),
             labelSize = 20, showLegend = TRUE, main = "Sentiments in Debates Compared")
# IT appears that W.Bush displayed more carried words than others...
# But does it seem to be a trend in using more emotional words from 1992 to 2000?

### Interviews

# get datase
load("~/GitHub/Poldis/data/US_interviews.rda")
uint <- US_interviews
# rename
uint <- uint %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
# create a corpus and clean
int_corpus <- VCorpus(DataframeSource(uint))
int_corpus <- cleanCorpus(int_corpus, stops)
# document term matrix
int_DTM  <- DocumentTermMatrix(int_corpus)
int_DTMm <- as.matrix(int_DTM)
# simple word frequencies
int_WFM <- data.frame(term = names(colSums(int_DTMm)),
                      freq = colSums(int_DTMm))
int_WFM <- int_WFM[order(int_WFM$freq, decreasing = T),]
rownames(int_WFM) <- NULL
head(int_WFM, 30)
# Similar generic words...
# Bigrams it is...
tint <- VCorpus(VectorSource(uint$text))
tint <- cleanCorpus(tint, stops)
tint <- tidy(tint)
tintt <- tint %>%
  unnest_tokens(bigram, text , token = "ngrams", n = 3, n_min = 2) %>%
  dplyr::count(bigram, sort = TRUE) %>%
  ungroup()
# 50 most common bigrams
tintt %>% with(wordcloud(bigram, n, random.order = FALSE, max.words = 50, colors=pal))
# Ok, not as informative either...
# Let's compare a few speakers
# How about we compare all of them for once here.
unique(uint$speaker)
# 12 total speakers
# before we join things, we also need to normalize sentiment by the number of obs
sp <- as.factor(uint$speaker)
summary(sp)
# First lets agregate the observations by speaker in a new dataset
int_comp <-  aggregate(uint$text, list(uint$speaker), paste, collapse =" ")
# Clean and transfrom into a corpus
int_comp <- rename(int_comp, doc_id = "Group.1", text = "x")
int_comp_corpus <- VCorpus(DataframeSource(int_comp))
int_comp_DTM <- TermDocumentMatrix(int_comp_corpus)
int_comp_DTMm <- as.matrix(int_comp_DTM)
# Let's first look at word frequencies and associations for each speaker
int_compr_t <- tidy(int_comp_corpus)
int_compr_wf <- int_compr_t %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
w_bush_int <- int_compr_wf %>% filter(id == "George W. Bush")
w_bush_int <- inner_join(w_bush_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(w_bush_int = sum(n)/159) %>% # 159 = number of interviews in dataset
  select(-sentiment)
b_clinton_int <- int_compr_wf %>% filter(id == "William J. Clinton")
b_clinton_int <- inner_join(b_clinton_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(b_clinton_int = sum(n)/239) %>% #interviews in dataset
  select(-sentiment)
bush_int <- int_compr_wf %>% filter(id == "George Bush")
bush_int <- inner_join(bush_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(bush_int = sum(n)/36) %>% #interviews in dataset
  select(-sentiment)
trump_int <- int_compr_wf %>% filter(id == "Donald J. Trump")
trump_int <- inner_join(trump_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(trump_int = sum(n)/25) %>% #interviews in dataset
  select(-sentiment)
reagan_int <- int_compr_wf %>% filter(id == "Ronald Reagan")
reagan_int <- inner_join(reagan_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(reagan_int = sum(n)/118) #interviews in dataset
h_clinton_int <- int_compr_wf %>% filter(id == "Hillary Clinton")
h_clinton_int <- inner_join(h_clinton_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(h_clinton_int = sum(n)/39) %>% #interviews in dataset
  select(-sentiment)
obama_int <- int_compr_wf %>% filter(id == "Barack Obama")
obama_int <- inner_join(obama_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(obama_int = sum(n)/252) %>% #interviews in dataset
  select(-sentiment)
mccain_int <- int_compr_wf %>% filter(id == "John McCain")
mccain_int <- inner_join(mccain_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(mccain_int = sum(n)/27) %>% #interviews in dataset
  select(-sentiment)
sanders_int <- int_compr_wf %>% filter(id == "Bernie Sanders")
sanders_int <- inner_join(sanders_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(sanders_int = sum(n)/31) %>% #interviews in dataset
  select(-sentiment)
dole_int <- int_compr_wf %>% filter(id == "Robert Dole")
dole_int <- inner_join(dole_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(dole_int = sum(n)/1) %>% #interviews in dataset
  select(-sentiment)
romney_int <- int_compr_wf %>% filter(id == "Mitt Romney")
romney_int <- inner_join(romney_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(romney_int = sum(n)/4) %>% #interviews in dataset
  select(-sentiment)
biden_int <- int_compr_wf %>% filter(id == "Joseph R. Biden")
biden_int <- inner_join(biden_int, get_sentiments("nrc"), by = "word") %>%
  group_by(sentiment) %>%
  summarize(biden_int = sum(n)/5) %>% #interviews in dataset
  select(-sentiment)
# bind things together
sent_int_sp <- data.frame(cbind(reagan_int, bush_int, b_clinton_int, dole_int, w_bush_int, obama_int, mccain_int, romney_int,
                                sanders_int, h_clinton_int, trump_int, biden_int))
sent_int_sp <- data.frame(sent_int_sp[,-1], row.names=sent_int_sp[,1])
# plot
chartJSRadar(scores = sent_int_sp,
             labs = rownames(sent_int_sp),
             labelSize = 20, showLegend = TRUE, main = "Sentiments in Interviews Compared (Normalized by number of interviews per speaker")
# Trump does not seem to be an outlier here at all...
# Not too informative though...
# How about we simply collapse things altogether and see the "number of emotions" for speaker
# Let's try to see a different way
sent_int_plot <- data.frame(t(sent_int_sp))
sent_int_plot <- sent_int_plot[-4,] # removes Bob Dole since he has only 1 interview
sent_int_plot$total <- rowSums(sent_int_plot)
sent_int_plot$speaker <- gsub("_int", "", rownames(sent_int_plot))
sent_int_plot <- sent_int_plot %>% tidyr::gather(key = Sentiment, value = Value, anger:trust)
ggplot(sent_int_plot, aes(speaker, Value, fill = Sentiment)) +
  geom_bar(position="stack", stat="identity")
# What do you get from all this? Not much but to say that
# Romney, Trump and Biden use more carried words, according to this disctionary,
# than Bush, H. Clinton or Sanders.

### Oral

# Get Oral remarks, the big one...
load("~/GitHub/Poldis/data/US_oral.rda")
uora <- US_oral
# Rename variables
uora <- uora %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
# create a big corpus and clean
ora_corpus <- VCorpus(DataframeSource(uora))
ora_corpus <- cleanCorpus(ora_corpus, stops)
# DTM
ora_DTM  <- DocumentTermMatrix(ora_corpus)
ora_DTMt <- tidy(ora_DTM) # Issue with size here hence why we conveted to tidy
# Let's see word frequencies, though this is likely not helpful...
ora_WFM <- ora_DTMt %>% select(term, count)
ora_WFM <- aggregate(count ~ term, data = ora_WFM, FUN=sum)
ora_WFM <- ora_WFM %>% arrange(desc(count))
head(ora_WFM, 30)
# Okay, this has not been all that helpful..
# Let's get bigrams just for the sake of it

