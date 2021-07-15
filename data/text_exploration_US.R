# Text Pre Processing and Exploration US Data
# Henrique Sposito

# Before starting, let's load some packages
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
library(topicmodels)

# Some helper functions and options
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# This is a slow one but helpful
cleanCorpus<-function(corpus, customStopwords){
  # corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  # corpus <- tm_map(corpus, content_transformer(qdap::replace_contraction))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

pal <- brewer.pal(8,"Dark2")
stops <- stopwords('SMART') # more agreesive dictionary than english, but slow

# Let's start with one dataset at a time. Please be aware that,
# due to the size of the datasets, there is a risk system will crash running the scripts.
# I recommend that environment and console are cleaned at the beggining of each dataset
# (at ################### which separates the script). To run gc() is also recommended at this point.
# This entails that the first part of the script (above) is re-run at those points to
# re-load packages and functions.

################## Campaign US

load("~/GitHub/Poldis/data/US_campaign.rda")
# The path to  the data might be different to you here, if that is the case you can just click
# on dataset in th data folder and import it manually to environment.
ucamp <- US_campaign

# Get variable names standardized and drop source
ucamp <- ucamp %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
# create a corpus from data frame
camp_corpus <- VCorpus(DataframeSource(ucamp))
# clean corpus = remove stopwords, punctuations and lower case
camp_corpus <- cleanCorpus(camp_corpus, stops)

# Let's explore word frequencies of the datasets as a whole,
# but first we need to get term document matrixes for all datasets
camp_DTM  <- DocumentTermMatrix(camp_corpus)
camp_DTMm <- as.matrix(camp_DTM)

# Get most frequent terms
camp_WFM <- data.frame(term = names(colSums(camp_DTMm)),
                       freq = colSums(camp_DTMm))
camp_WFM <- camp_WFM[order(camp_WFM$freq, decreasing = T),]
rownames(camp_WFM) <- NULL
word_frequencies_camp <- data.frame(head(camp_WFM, 30))
# Just spliting to make the plot prettier
word_frequencies_camp1 <- word_frequencies_camp[1:10,]
word_frequencies_camp2 <- word_frequencies_camp[11:20,]
word_frequencies_camp2 <- rename(word_frequencies_camp2, term_1 = "term", freq_1 = "freq")
word_frequencies_camp3 <- word_frequencies_camp[21:30,]
word_frequencies_camp3 <- rename(word_frequencies_camp3, term_2 = "term", freq_2 = "freq")
word_frequencies_camp <- cbind(word_frequencies_camp1, word_frequencies_camp2, word_frequencies_camp3)
# Get a nice table for codebook
word_frequency_camp <-  kableExtra::kbl(word_frequencies_camp,
                                        caption = "Word Frquencies in US Campaign") %>%
  kableExtra::kable_classic(full_width = F, html_font = "Times New Roman")
word_frequency_camp # Very generic 30 most frequent terms...

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
# Let's plot the 100 most common bigrams
bigram_camp_US <- tcampt %>%
  with(wordcloud(bigram, n, random.order = FALSE, max.words = 100, colors=pal)) %>%
  title(main = "100 Most Frequent Bigrams in Campaign Texts for the US",
        sub = "Sizes are proposrtional to frequency and colors represent similar frequencies")
# Some make sures, tax cuts, new jobs and more. Do you think this differes for the other settings?

# For the sake of it, let's take a look at sentiment for all texts toegther, that is, are texts for debates
# mostly positive or negatives?
camp_DTMt <- tidy(camp_DTM)
camp_sent <- camp_DTMt %>% select(term, count)
camp_sent <- aggregate(count ~ term, data = camp_sent, FUN=sum)
camp_sent <- camp_sent %>% arrange(desc(count))
camp_sentscr <- camp_sent %>%
  inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
  arrange(desc(count)) # Okay, but let's try to quantify this
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
t_obamact %>% with(wordcloud(bigram, n, random.order = FALSE, max.words = 50, colors=pal))
# midle class, tax cuts, health care
t_trumpct %>% with(wordcloud(bigram, n, random.order = FALSE, max.words = 50, colors=pal))
# Hillary clinton, make america, america great
# But none of this is really helpful...

# Let's try and see if we can get word frequencies by speakers
# First let's look at the speakers in the dataset
summary(as.factor(ucamp$speaker))
sp_freq <- ucamp
sp_freq <- aggregate(sp_freq$text, list(sp_freq$speaker), paste, collapse =" ")
sp_freq <- rename(sp_freq, doc_id = "Group.1", text = "x")
sp_freq_corpus <- VCorpus(DataframeSource(sp_freq))
sp_freq_corpus <- tidy(sp_freq_corpus) # Large vector so we are using tidy here
sp_freq_corpus <- sp_freq_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE) # get words
stops_t <- tibble::tibble(stops) %>% rename(word = "stops") # stopwords into tibble
sp_freq_corpus <- sp_freq_corpus %>%
  anti_join(stops_t, by = c("word" = "word")) # remove stopwords
top_words <- sp_freq_corpus %>%
  group_by(id) %>%
  filter(row_number() %in% 1:20) %>%
  arrange(id)
# Too big to plot in one so split the plot in two
top_words_sp1 <-data.frame(Al_Gore = top_words$word[1:20],
                          Obama = top_words$word[21:40],
                          Sanders = top_words$word[41:60],
                          Trump = top_words$word[61:80],
                          Bush = top_words$word[81:100],
                          W_Bush = top_words$word[101:120],
                          H_Clinton = top_words$word[121:140],
                          Jimmy_Carter = top_words$word[141:160],
                          John_Kerry = top_words$word[161:180])
word_frequency_camp_sp1 <-  kableExtra::kbl(top_words_sp1,
                                            caption = "Top 20 Words in US Campaign by Speaker") %>%
  kableExtra::kable_classic(full_width = F, html_font = "Times New Roman")
top_words_sp2 <-data.frame(McCain = top_words$word[181:200],
                           Biden = top_words$word[201:220],
                           Dukakis = top_words$word[221:240],
                           Romney = top_words$word[241:260],
                           Bob_Dole = top_words$word[261:280],
                           Reagan = top_words$word[281:300],
                           W_Mondale = top_words$word[301:320],
                           B_Clinton = top_words$word[321:340])
word_frequency_camp_sp2 <-  kableExtra::kbl(top_words_sp2,
                                            caption = "Top 20 Words in US Campaign by Speaker (Continuation)") %>%
  kableExtra::kable_classic(full_width = F, html_font = "Times New Roman")

################### Debates

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

# None of this seems too helpful tough, how about we try some topic modeling?
# LDA finds words most related to others in clusters which should give topics.

# First thing, let's agregate data by speaker and year.
lda_deb <- udeb
lda_deb$doc_id <- trimws(lda_deb$speaker) # remove extra espace there
lda_deb$doc_id <- paste0(lda_deb$doc_id, "_", stringr::str_extract_all(lda_deb$date, "^[0-9]{4}"))
# get speaker year together
lda_deb <- aggregate(lda_deb$text, list(lda_deb$doc_id), paste, collapse =" ")
# aggregate by speaker + year, we get 24 speakers here.
lda_deb <- rename(lda_deb, doc_id = "Group.1", text = "x") # rename

# Get corpus and clean and DTM
lda_deb_corpus <- VCorpus(DataframeSource(lda_deb))
lda_deb_corpus <- cleanCorpus(lda_deb_corpus, stops)
lda_deb_DTM  <- DocumentTermMatrix(lda_deb_corpus)

# Fit a simple LDA model
# Please note that we set a K = 11, that is we want it to find 11 clusters (topics).
# This is because there are 11 election cycles covered in debates and,
# assuming each election cycles is marked by one main topic, we could have 11 topics.
# Let's see what happens
lda_deb_model <- LDA(lda_deb_DTM, k = 11, control = list(seed = 1234)) # set seed for reproduciability

# Let's produce a few descriptives
# Get the top 10 words per topic
lda_deb_top_words <- tidy(lda_deb_model, matrix = "beta") %>%
  arrange(desc(beta)) %>%
  group_by(topic) %>%
  filter(row_number() %in% 1:10) %>% # get the top 10 words
  arrange(topic)
topten_topic <- aggregate(lda_deb_top_words$term, list(lda_deb_top_words$topic), paste, collapse=", ")
topten_topic <- rename(topten_topic, Topic = "Group.1", Terms = "x")
topten_topic <-  kableExtra::kbl(topten_topic,
                                 caption = "Top 10 Words per Topic for US Debates") %>%
  kableExtra::kable_classic(full_width = F, html_font = "Times New Roman")
topten_topic # not super helpful as topics appear related to cycles and not election issues

# Let's get the gamma (percent of topic in document for speakers)
lda_deb_topic_per_sp <- tidy(lda_deb_model, matrix = "gamma") %>%
  arrange(desc(gamma)) %>%
  group_by(document) %>%
  filter(row_number() %in% 1:2) %>% # get the top 3 topics per speaker
  arrange(document)
lda_deb_topic_per_sp$year <- stringr::str_extract(lda_deb_topic_per_sp$document, "[:digit:]{4}")
lda_deb_topic_per_sp$document <- stringr::str_replace(lda_deb_topic_per_sp$document, "_[:digit:]{4}", "")
lda_deb_topic_per_sp$gamma <- round(lda_deb_topic_per_sp$gamma, digits = 4)
lda_deb_topic_per_sp <- arrange(lda_deb_topic_per_sp, year)
# Just to plot it better, let's split the table and bind.
lda_deb_topic_per_sp1 <- lda_deb_topic_per_sp[1:24,]
lda_deb_topic_per_sp2 <- lda_deb_topic_per_sp[25:48,]
lda_deb_topic_per_sp2 <- rename(lda_deb_topic_per_sp2, document_1 = "document",
                                topic_1 = "topic", gamma_1 = "gamma", year_1 = "year")
lda_deb_topic_per_sp <- cbind(lda_deb_topic_per_sp1, lda_deb_topic_per_sp2)
lda_deb_topic_per_sp <-  kableExtra::kbl(lda_deb_topic_per_sp,
                                         caption = "Top 2 Topics Per Speaker and Year for US Debates") %>%
  kableExtra::kable_classic(full_width = F, html_font = "Times New Roman")
lda_deb_topic_per_sp # Interesting, There is some overlap for election cycles.

################### Interviews

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

# Get word frequencies and sentiment normilized by obs for speaker
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
  geom_bar(position="stack", stat="identity") +
  labs(x = "Speaker",
       y = "",
       title = "Sentiment for Speakers in Interviews",
       subtitle = "Normalized by observations for speaker in dataset")
# What do you get from all this? Not much but to say that
# Romney, Trump and Biden use more carried words, according to this disctionary,
# than Bush, H. Clinton or Sanders.

################## Oral

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
# At this point I will split the data by speaker.
# I will skip bigrams as they are not all that helpful either and
# dataset is too big and causes R to crash.

# Let me look at all the presidents in the dataset
unique(uora$speaker)

# Filter speaker
reagan_speech <- uora %>% filter(speaker == "Ronald Reagan")

# Let's agregate the dataset by speaker and year
reagan_speech$doc_id <- paste0("Reagan_", stringr::str_extract_all(reagan_speech$date, "^[0-9]{4}"))
reagan_speech <- aggregate(reagan_speech$text, list(reagan_speech$doc_id), paste, collapse =" ")
reagan_speech <- rename(reagan_speech, doc_id = "Group.1", text = "x")
reagan_speech <- reagan_speech[-9,] # removes 1989, transition year and, thus, very few speeches

# Get a corcpus and clean
reagan_corpus <- VCorpus(DataframeSource(reagan_speech))
reagan_corpus <- tm_map(reagan_corpus, content_transformer(tryTolower))
reagan_corpus <- tidy(reagan_corpus)
# Repeat the same for all speakers

# Bush
bush_speech <- uora %>% filter(speaker == "George Bush")
bush_speech$doc_id <- paste0("Bush", stringr::str_extract_all(bush_speech$date, "^[0-9]{4}"))
bush_speech <- aggregate(bush_speech$text, list(bush_speech$doc_id), paste, collapse =" ")
bush_speech <- rename(bush_speech, doc_id = "Group.1", text = "x")
bush_speech <- bush_speech[-c(1,2,7),] # removes prior to 1988 and after 1992
bush_corpus<- VCorpus(DataframeSource(bush_speech))
bush_corpus <- tm_map(bush_corpus, content_transformer(tryTolower))
bush_corpus <- tidy(bush_corpus)

# Clinton
clinton_speech <- uora %>% filter(speaker == "William J. Clinton")
clinton_speech$doc_id <- paste0("Clinton_", stringr::str_extract_all(clinton_speech$date, "^[0-9]{4}"))
clinton_speech <- aggregate(clinton_speech$text, list(clinton_speech$doc_id), paste, collapse =" ")
clinton_speech <- rename(clinton_speech, doc_id = "Group.1", text = "x")
clinton_speech <- clinton_speech[-c(9,10,11),] # removes 2001 and onwards, transition year and, thus, very few speeches
clinton_corpus <- VCorpus(DataframeSource(clinton_speech))
clinton_corpus <- tm_map(clinton_corpus, content_transformer(tryTolower))
clinton_corpus <- tidy(clinton_corpus)

# W. Bush
w_bush_speech <- uora %>% filter(speaker == "George W. Bush")
w_bush_speech$doc_id <- paste0("W_Bush_", stringr::str_extract_all(w_bush_speech$date, "^[0-9]{4}"))
w_bush_speech <- aggregate(w_bush_speech$text, list(w_bush_speech$doc_id), paste, collapse =" ")
w_bush_speech <- rename(w_bush_speech, doc_id = "Group.1", text = "x")
w_bush_speech <- w_bush_speech[-9,] # removes 2009, transition year and, thus, very few speeches
w_bush_corpus <- VCorpus(DataframeSource(w_bush_speech))
w_bush_corpus <- tm_map(w_bush_corpus, content_transformer(tryTolower))
w_bush_corpus <- tidy(w_bush_corpus)

# Obama
obama_speech <- uora %>% filter(speaker == "Barack Obama")
obama_speech$doc_id <- paste0("Obama_", stringr::str_extract_all(obama_speech$date, "^[0-9]{4}"))
obama_speech <- aggregate(obama_speech$text, list(obama_speech$doc_id), paste, collapse =" ")
obama_speech <- rename(obama_speech, doc_id = "Group.1", text = "x")
obama_speech <- obama_speech[-9,] # removes 2017, transition year and, thus, very few speeches
obama_corpus <- VCorpus(DataframeSource(obama_speech))
obama_corpus <- tm_map(obama_corpus, content_transformer(tryTolower))
obama_corpus <- tidy(obama_corpus)

# Trump
trump_speech <- uora %>% filter(speaker == "Donald J. Trump")
trump_speech$doc_id <- paste0("Trump_", stringr::str_extract_all(trump_speech$date, "^[0-9]{4}"))
trump_speech <- aggregate(trump_speech$text, list(trump_speech$doc_id), paste, collapse =" ")
trump_speech <- rename(trump_speech, doc_id = "Group.1", text = "x")
trump_speech <- trump_speech[-c(1,6),] # removes 2016 and 2021
trump_corpus <- VCorpus(DataframeSource(trump_speech))
trump_corpus <- tm_map(trump_corpus, content_transformer(tryTolower))
trump_corpus <- tidy(trump_corpus)

# Biden
biden_speech <- uora %>% filter(speaker == "Joseph R. Biden")
biden_speech$doc_id <- paste0("Biden_", stringr::str_extract_all(biden_speech$date, "^[0-9]{4}"))
biden_speech <- aggregate(biden_speech$text, list(biden_speech$doc_id), paste, collapse =" ")
biden_speech <- rename(biden_speech, doc_id = "Group.1", text = "x")
biden_speech <- biden_speech[11,] # keep 2021 only
biden_corpus <- VCorpus(DataframeSource(biden_speech))
biden_corpus <- tm_map(biden_corpus, content_transformer(tryTolower))
biden_corpus <- tidy(biden_corpus)

# Get word frequency
reagan_wf <- reagan_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Bush
bush_wf <- bush_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Clinton
clinton_wf <- clinton_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# W. Bush
w_bush_wf <- w_bush_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Obama
obama_wf <- obama_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Trump
trump_wf <- trump_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Biden
biden_wf <- biden_corpus %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)

# Join with afinn lexicon for sentiment.
# Affin differs from NRC as it has values for words, but it is more restricted (less words).
# Get sentiment across time for speakers
# And normalize by numbers of obs
sp <- as.factor(uora$speaker)
summary(sp)

reagan_speech_af <- inner_join(reagan_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/2505) # normalized by speeches for speaker in dataset
bush_speech_af <- inner_join(bush_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/1669)
clinton_speech_af <- inner_join(clinton_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/3756)
w_bush_speech_af <- inner_join(w_bush_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/2934)
obama_speech_af <- inner_join(obama_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/2201)
trump_speech_af <- inner_join(trump_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/1360)
biden_speech_af <- inner_join(biden_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/336)

# Get dates in correct format
reagan_speech_af$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(reagan_speech_af$id, "[0-9]{4}$")))
bush_speech_af$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(bush_speech_af$id, "[0-9]{4}$")))
clinton_speech_af$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(clinton_speech_af$id, "[0-9]{4}$")))
w_bush_speech_af$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(w_bush_speech_af$id, "[0-9]{4}$")))
obama_speech_af$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(obama_speech_af$id, "[0-9]{4}$")))
trump_speech_af$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(trump_speech_af$id, "[0-9]{4}$")))
biden_speech_af$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(biden_speech_af$id, "[0-9]{4}$")))

# Add a speker column
reagan_speech_af$speaker <- "Reagan"
bush_speech_af$speaker <- "Bush"
clinton_speech_af$speaker <- "Clinton"
w_bush_speech_af$speaker <- "W. Bush"
obama_speech_af$speaker <- "Obama"
trump_speech_af$speaker <- "Trump"
biden_speech_af$speaker <- "Biden"

# Let's bind all and plot
speakers_speech_sent <- rbind(reagan_speech_af, bush_speech_af, clinton_speech_af, w_bush_speech_af,
                              obama_speech_af, trump_speech_af, biden_speech_af)
ggplot(speakers_speech_sent, aes(x = date, y = value , fill = speaker)) +
  geom_line() +
  geom_point(size = 4, shape = 21) +
  labs(x = "Year",
       y = "",
       title = "Sentiment for Presidents in Official Speeches Across time",
       subtitle = "Normalized by observations for speaker in dataset",
       caption = "Sentiments were generated with 'Afinn' lexicon") +
  theme_fivethirtyeight()
# This is getteing much more interesting!!! Trump is an outlier when it comes to
# using certain words, however, it appears that he uses more positive words than others.
# Why a peak in 1984? Campaign for reelection?
# Why a peak in 2000? Campaign for Al Gore?
# reelection 2004 spike? Or, on the other hand, 2007 and 2008 decline correlated to mortage crisis?
# Why 2009 and 2010 peak? Recovery and nobel? and decline in 2012 at reelection period?

# NRC sentiment comparison
# Inner join with NRC sentiment lexicon
reagan_speech_nrc <- inner_join(reagan_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/2505) # normalized by speeches for speaker in dataset
bush_speech_nrc <- inner_join(bush_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/1669)
clinton_speech_nrc <- inner_join(clinton_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/3756)
w_bush_speech_nrc <- inner_join(w_bush_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/2934)
obama_speech_nrc <- inner_join(obama_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/2201)
trump_speech_nrc <- inner_join(trump_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/1360)
biden_speech_nrc <- inner_join(biden_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/336)

# Add a speker column
reagan_speech_nrc$speaker <- "Reagan"
bush_speech_nrc$speaker <- "Bush"
clinton_speech_nrc$speaker <- "Clinton"
w_bush_speech_nrc$speaker <- "W. Bush"
obama_speech_nrc$speaker <- "Obama"
trump_speech_nrc$speaker <- "Trump"
biden_speech_nrc$speaker <- "Biden"

# Add date
reagan_speech_nrc$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(reagan_speech_nrc$id, "[0-9]{4}$")))
bush_speech_nrc$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(bush_speech_nrc$id, "[0-9]{4}$")))
clinton_speech_nrc$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(clinton_speech_nrc$id, "[0-9]{4}$")))
w_bush_speech_nrc$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(w_bush_speech_nrc$id, "[0-9]{4}$")))
obama_speech_nrc$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(obama_speech_nrc$id, "[0-9]{4}$")))
trump_speech_nrc$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(trump_speech_nrc$id, "[0-9]{4}$")))
biden_speech_nrc$date <- lubridate::dmy(paste0("15-06-", stringr::str_extract_all(biden_speech_nrc$id, "[0-9]{4}$")))

# Let's bind all and plot
speakers_nrc <- rbind(reagan_speech_nrc, bush_speech_nrc, clinton_speech_nrc, w_bush_speech_nrc,
                      obama_speech_nrc, trump_speech_nrc, biden_speech_nrc) %>%
  arrange(date)

ggplot(speakers_nrc, aes(reorder(id, date), value, fill = sentiment)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "NRC Sentiment for Presidents in Official Speeches Across time",
       subtitle = "Normalized by observations for speaker in dataset",
       caption = "Sentiments were generated with 'NRC' lexicon") +
  coord_flip()

#################### Compare sentiment for Obama and Trump across settings

# Clean your environment and console here and perform gc() to make sure it all goes well...
# Make sure you re-load packages and functions at the beggining of this script
# Re-load and clean datasets
load("~/GitHub/Poldis/data/US_campaign.rda")
ucamp <- US_campaign
ucamp <- ucamp %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
load("~/GitHub/Poldis/data/US_debates.rda")
udeb <- US_debates
udeb <- udeb %>% rename(doc_id = Title, text = Text, speaker = Speakers, date = Date) %>%
  arrange(doc_id, text, speaker, date)
load("~/GitHub/Poldis/data/US_interviews.rda")
uint <- US_interviews
uint <- uint %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)
load("~/GitHub/Poldis/data/US_oral.rda")
uora <- US_oral
uora <- uora %>% rename(doc_id = title) %>%
  select(-source_links) %>%
  arrange(doc_id, text, speaker, date)

# Obama campaign
camp_sent_obama <- ucamp %>% filter(speaker == "Barack Obama")
camp_sent_obama <- VCorpus(DataframeSource(camp_sent_obama))
camp_sent_obama <- tidy(camp_sent_obama)
camp_sent_obama$id <- "Obama_campaign"
camp_sent_obama <- camp_sent_obama %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
camp_sent_obama_nrc <- inner_join(camp_sent_obama, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/544) # normalized by campaign texts for speaker in dataset
camp_sent_obama_afinn <- inner_join(camp_sent_obama, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/544) # normalized by camapign texts speaker in dataset

# Trump campaign
camp_sent_trump <- ucamp %>% filter(speaker == "Donald J. Trump")
camp_sent_trump <- VCorpus(DataframeSource(camp_sent_trump))
camp_sent_trump <- tidy(camp_sent_trump)
camp_sent_trump$id <- "Trump_campaign"
camp_sent_trump <- camp_sent_trump %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
camp_sent_trump_nrc <- inner_join(camp_sent_trump, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/108) # normalized by campaign texts for speaker in dataset
camp_sent_trump_afinn <- inner_join(camp_sent_trump, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/108)

# Obama debates
debate_sent_obama <- udeb %>% filter(speaker == " Barack Obama")
debate_sent_obama <- VCorpus(DataframeSource(debate_sent_obama))
debate_sent_obama <- tidy(debate_sent_obama)
debate_sent_obama$id <- "Obama_debate"
debate_sent_obama <- debate_sent_obama %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
debate_sent_obama_nrc <- inner_join(debate_sent_obama, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/6) # normalized by debates for speaker in dataset
debate_sent_obama_afinn <- inner_join(debate_sent_obama, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/6) # normalized by debates for speaker in dataset

# Trump debates
debate_sent_trump <- udeb %>% filter(speaker == " Donald Trump")
debate_sent_trump <- VCorpus(DataframeSource(debate_sent_trump))
debate_sent_trump <- tidy(debate_sent_trump)
debate_sent_trump$id <- "Trump_debate"
debate_sent_trump <- debate_sent_trump %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
debate_sent_trump_nrc <- inner_join(debate_sent_trump, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/5) # normalized by debates for speaker in dataset
debate_sent_trump_afinn <- inner_join(debate_sent_trump, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/5) # normalized by debates for speaker in dataset

# Obama Interviews
interview_sent_obama <- uint %>% filter(speaker == "Barack Obama")
interview_sent_obama <- VCorpus(DataframeSource(interview_sent_obama))
interview_sent_obama <- tidy(interview_sent_obama)
interview_sent_obama$id <- "Obama_interview"
interview_sent_obama <- interview_sent_obama %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
interview_sent_obama_nrc <- inner_join(interview_sent_obama, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/252) # normalized by interviews for speaker in dataset
interview_sent_obama_afinn <- inner_join(interview_sent_obama, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/252) # normalized by interviews for speaker in dataset

# Trump interview
interview_sent_trump <- uint %>% filter(speaker == "Donald J. Trump")
interview_sent_trump <- VCorpus(DataframeSource(interview_sent_trump))
interview_sent_trump <- tidy(interview_sent_trump)
interview_sent_trump$id <- "Trump_interview"
interview_sent_trump <- interview_sent_trump %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
interview_sent_trump_nrc <- inner_join(interview_sent_trump, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/25) # normalized by interviews for speaker in dataset
interview_sent_trump_afinn <- inner_join(interview_sent_trump, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/25) # normalized by interviews for speaker in dataset

# Obama speeches
oral_sent_obama <- uora %>% filter(speaker == "Barack Obama")
oral_sent_obama <- VCorpus(DataframeSource(oral_sent_obama))
oral_sent_obama <- tidy(oral_sent_obama)
oral_sent_obama$id <- "Obama_speeches"
oral_sent_obama <- oral_sent_obama %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
oral_sent_obama_nrc <- inner_join(oral_sent_obama, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/2201) # normalized by speeches for speaker in dataset
oral_sent_obama_afinn <- inner_join(oral_sent_obama, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/2201) # normalized by speeches for speaker in dataset

# Trump speeches
oral_sent_trump <- uora %>% filter(speaker == "Donald J. Trump")
oral_sent_trump <- VCorpus(DataframeSource(oral_sent_trump))
oral_sent_trump <- tidy(oral_sent_trump)
oral_sent_trump$id <- "Trump_speeches"
oral_sent_trump <- oral_sent_trump %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
oral_sent_trump_nrc <- inner_join(oral_sent_trump, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n)/1306) # normalized by speeches for speaker in dataset
oral_sent_trump_afinn <- inner_join(oral_sent_trump, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n)/1306) # normalized by speeches for speaker in dataset

# Bind datasets
campaign_sentiment_nrc <- rbind(camp_sent_obama_nrc, camp_sent_trump_nrc, interview_sent_obama_nrc, interview_sent_trump_nrc,
                               debate_sent_obama_nrc, debate_sent_trump_nrc, oral_sent_obama_nrc, oral_sent_trump_nrc)
campaign_sentiment_nrc$setting <- gsub(".*_", "",campaign_sentiment_nrc$id)
campaign_sentiment_nrc$speaker <- gsub("_.*", "",campaign_sentiment_nrc$id)
campaign_sentiment_afinn <- rbind(camp_sent_obama_afinn, camp_sent_trump_afinn, interview_sent_obama_afinn, interview_sent_trump_afinn,
                                debate_sent_obama_afinn, debate_sent_trump_afinn, oral_sent_obama_afinn, oral_sent_trump_afinn)
campaign_sentiment_afinn$setting <- gsub(".*_", "",campaign_sentiment_afinn$id)
campaign_sentiment_afinn$speaker <- gsub("_.*", "",campaign_sentiment_afinn$id)

# Let's visualize
nrc_ot <- ggplot(campaign_sentiment_nrc, aes(reorder(id, setting) , value, fill = sentiment)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "Sentiment for Obama and Trump in Different Political Settings",
       subtitle = "Normalized by observations for speaker in dataset",
       caption = "NRC sentiment lexicon") +
  theme_fivethirtyeight()

afinn_ot <- ggplot(campaign_sentiment_afinn, aes(x = setting, y = value, fill = speaker)) +
  geom_line(aes(group = speaker)) +
  geom_point(size = 4, shape = 21) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       caption = "Afinn sentiment lexicon") +
  theme_fivethirtyeight()

gridExtra::grid.arrange(nrc_ot, afinn_ot)
dev.off()
