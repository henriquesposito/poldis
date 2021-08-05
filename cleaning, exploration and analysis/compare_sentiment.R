# Sentiment Comparison for Brazil and the US
# Henrique Sposito

# Let's load some packages

library(dplyr)
library(ggplot2)
library(ggthemes)
library(tm)
library(tidytext)
library(readr)

# Compare Speeches
# For more details on codes and comments please refer to exploration scripts.
# Load translated lexicons
load("~/GitHub/Poldis/R/sysdata.rda")

# Brazil
load("~/GitHub/Poldis/data/BR_oral.rda")
boral <- BR_oral %>% select(date, presid, text)
boral$Speaker <- paste0(boral$presid, "_", boral$date) # get speaker year
boral_sent <-  aggregate(boral$text, list(boral$Speaker), paste, collapse =" ")
boral_sent <- rename(boral_sent, doc_id = "Group.1", text = "x")
boral_sent_corpus <- VCorpus(DataframeSource(boral_sent))
boral_sent_tidy <- tidy(boral_sent_corpus)
boral_sent_wf <- boral_sent_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
boral_sent_af <- inner_join(boral_sent_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
boral_sent_af$obs <- unname(summary(as.factor(boral$Speaker)))
boral_sent_af$char <- nchar(boral_sent$text)
boral_sent_af <- boral_sent_af %>% filter(obs > 20) # removes years with too littles obs (president leaving office)
boral_sent_af$n_value <- boral_sent_af$value/boral_sent_af$char  # normalized by characters in text obs
boral_sent_af$date <- paste0(stringr::str_extract(boral_sent_af$id, "[0-9]{4}"))
boral_sent_af$id <- stringr::str_replace_all(boral_sent_af$id, "_[0-9]{4}", "")
# US
load("~/GitHub/Poldis/data/US_oral.rda")
uoral <- US_oral %>% select(speaker, date, text)
uoral$speaker <- paste0(uoral$speaker, "_", stringr::str_extract_all(uoral$date, "^[0-9]{4}")) # get speaker year
# Bush 1985 has only 1 observation but it causes a bug with NRC sentiment later down the road.
# I will remove this observation here so that this is avoided.
uoral <- uoral %>% filter(speaker != "George Bush_1985")
uoral_sent <- aggregate(uoral$text, list(uoral$speaker), paste, collapse = " ")
uoral_sent <- rename(uoral_sent, doc_id = "Group.1", text = "x")
uoral_sent_corpus <- VCorpus(DataframeSource(uoral_sent))
uoral_sent_tidy <- tidy(uoral_sent_corpus)
uoral_sent_wf <- uoral_sent_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
uoral_sent_af <- inner_join(uoral_sent_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
uoral_sent_af$obs <- unname(summary(as.factor(uoral$speaker)))
uoral_sent_af$char <- nchar(uoral_sent$text)
uoral_sent_af <- uoral_sent_af %>% filter(obs > 20) # removes years with too littles obs (president leaving office)
uoral_sent_af$n_value <- uoral_sent_af$value/uoral_sent_af$char  # normalized by number of character for obs
uoral_sent_af$date <- paste0(stringr::str_extract(uoral_sent_af$id, "[0-9]{4}"))
uoral_sent_af$id <- stringr::str_replace_all(uoral_sent_af$id, "_[0-9]{4}", "")
# Bind and plot
sent_speech_af <- rbind(boral_sent_af, uoral_sent_af)
sent_speech_af$date_2 <- stringr::str_extract(sent_speech_af$date, "[0-9]{2}$")
sent_speech_af$id <- gsub("W. Bush", "W_Bush", sent_speech_af$id)
sent_speech_af$Speaker <- stringi::stri_extract_last_words(sent_speech_af$id)
ggplot(sent_speech_af, aes(x = reorder(date_2, as.numeric(date)), y = n_value , fill = Speaker)) +
  geom_line(aes(group = id)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Sentiment in Official Speeches Compared for Brazil and the US",
       subtitle = "Normalized by number characters for text per speaker and year",
       caption = "Afinn sentiment lexicon") +
  theme_fivethirtyeight()

# Compare NRC
# Brazil
boral_nrc <- inner_join(boral_sent_wf, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
obs <- tibble::tibble(obs = unname(summary(as.factor(boral$Speaker)))) %>%
  slice(rep(1:n(), each = 10)) # for each speaker 10 sentiments to normilize for the same number of obs
boral_nrc$obs <- obs$obs
boral_nrc <- boral_nrc %>%  filter(obs > 20) # year with little obs
boral_nrc$n_value <- boral_nrc$value/boral_nrc$obs  # normalized by obs in dataset
boral_nrc$date <- paste0(stringr::str_extract(boral_nrc$id, "[0-9]{4}"))
boral_nrc$id <- stringr::str_replace_all(boral_nrc$id, "[0-9]{4}",
                                         stringr::str_extract(boral_nrc$date, "[0-9]{2}$"))

# US
uoral_nrc <- inner_join(uoral_sent_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
obs <- tibble::tibble(obs = unname(summary(as.factor(uoral$speaker)))) %>%
  slice(rep(1:n(), each = 10)) # for each speaker 10 sentiments to normilize for the same number of obs
uoral_nrc$obs <- obs$obs
uoral_nrc <- uoral_nrc %>% filter(obs > 20) # removes years with too littles obs (president leaving office)
uoral_nrc$n_value <- uoral_nrc$value/uoral_nrc$obs  # normalized by obs in dataset
uoral_nrc$date <- paste0(stringr::str_extract(uoral_nrc$id, "[0-9]{4}"))
uoral_nrc$id <- stringr::str_replace_all(uoral_nrc$id, "[0-9]{4}",
                                               stringr::str_extract(uoral_nrc$date, "[0-9]{2}$"))
uoral_nrc$id <- gsub("W. Bush", "W_Bush", uoral_nrc$id)
uoral_nrc$id <- stringi::stri_extract_last_words(uoral_nrc$id)
# Plot side by side
sent_speech_nrc_us <- ggplot(uoral_nrc, aes(x = reorder(id, as.numeric(date)), y = n_value, fill = sentiment)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "Sentiment in Official Speeches Compared for Brazil and the US",
       subtitle = "Normalized by observations for speaker in dataset") +
  theme_fivethirtyeight() +
  coord_flip() +
  theme(legend.position = "none")

sent_speech_nrc_br <- ggplot(boral_nrc, aes(x = reorder(id, as.numeric(date)), y = n_value, fill = sentiment)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "",
       subtitle = "",
       caption = "NRC sentiment lexicon") +
  theme_fivethirtyeight() +
  expand_limits(y=c(0, 1000)) +
  coord_flip()

# Shal we compare just sentiment in settings across the cases?
load("~/GitHub/Poldis/data/US_campaign.rda")
load("~/GitHub/Poldis/data/US_debates.rda")
load("~/GitHub/Poldis/data/US_interviews.rda")
# US_oral is already loaded
load("~/GitHub/Poldis/data/BR_campaign.rda")
load("~/GitHub/Poldis/data/BR_debates.rda")
load("~/GitHub/Poldis/data/BR_interviews.rda")
# BR_oral is already loaded
# Let's just agregate all obs in dataset and then join them
# US
uoral_all <- paste(US_oral$text, collapse = " ")
uoral_all <- data.frame(doc_id = "US Speeches", text = uoral_all, char = nchar(uoral_all))
ucamp_all <- paste(US_campaign$text, collapse = " ")
ucamp_all <- data.frame(doc_id = "US Campaign", text = ucamp_all, char = nchar(ucamp_all))
udeb_all <- paste(US_debates$Text, collapse = " ")
udeb_all <- data.frame(doc_id = "US Debate", text = udeb_all, char = nchar(udeb_all))
uint_all <- paste(US_interviews$text, collapse = " ")
uint_all <- data.frame(doc_id = "US Interviews", text = uint_all, char = nchar(uint_all))
us_all <- rbind(uoral_all, ucamp_all, udeb_all, uint_all)
# Brazil
boral_all <- paste(BR_oral$text, collapse = " ")
boral_all <- data.frame(doc_id = "BR Speeches", text = boral_all, char = nchar(boral_all))
bcamp_all <- paste(BR_Campaign$Text, collapse = " ")
bcamp_all <- data.frame(doc_id = "BR Campaign", text = bcamp_all, char = nchar(bcamp_all))
bdeb_all <- paste(BR_debates$Text, collapse = " ")
bdeb_all <- data.frame(doc_id = "BR Debate", text = bdeb_all, char = nchar(bdeb_all))
bint_all <- paste(BR_Interviews$Text, collapse = " ")
bint_all <- data.frame(doc_id = "BR Interviews", text = bint_all, char = nchar(bint_all))
BR_all <- rbind(boral_all, bcamp_all, bdeb_all, bint_all)
# Get sentiment NRC
# US
us_all_sent <- VCorpus(DataframeSource(us_all))
us_all_sent_t <- tidy(us_all_sent)
us_all_sent_wf <- us_all_sent_t %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
us_all_nrc <- inner_join(us_all_sent_wf, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
# Normailze
us_c_char <- data.frame(char = rep(ucamp_all$char, 10))
us_d_char <- data.frame(char =rep(udeb_all$char, 10))
us_i_char <- data.frame(char =rep(uint_all$char, 10))
us_s_char <- data.frame(char =rep(uoral_all$char, 10))
us_char <- rbind(us_c_char, us_d_char, us_i_char, us_s_char)
us_all_nrc <- cbind(us_all_nrc, us_char)
us_all_nrc$n_value <- us_all_nrc$value/us_all_nrc$char
# BR
BR_all_sent <- VCorpus(DataframeSource(BR_all))
BR_all_sent_t <- tidy(BR_all_sent)
BR_all_sent_wf <- BR_all_sent_t %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
BR_all_nrc <- inner_join(BR_all_sent_wf, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
# Normailze
BR_c_char <- data.frame(char = rep(bcamp_all$char, 10))
BR_d_char <- data.frame(char =rep(bdeb_all$char, 10))
BR_i_char <- data.frame(char =rep(bint_all$char, 10))
BR_s_char <- data.frame(char =rep(boral_all$char, 10))
BR_char <- rbind(BR_c_char, BR_d_char, BR_i_char, BR_s_char)
BR_all_nrc <- cbind(BR_all_nrc, BR_char)
BR_all_nrc$n_value <- BR_all_nrc$value/BR_all_nrc$char
# Bind all
nrc_all <- rbind(us_all_nrc, BR_all_nrc)
# Let's remove positive and negative codes since these are not too informative
nrc_all <- nrc_all[nrc_all$sentiment!="negative",]
nrc_all <- nrc_all[nrc_all$sentiment!="positive",]
# Fixing minor speeling mistake
nrc_all$Sentiment <- nrc_all$sentiment
# Plot
ggplot(nrc_all, aes(x = id, y = n_value, fill = Sentiment)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "Sentiment Across Settings Compared for Brazil and the US",
       subtitle = "Normalized by number of characters for text in dataset",
       caption = "NRC sentiment lexicon") +
  theme_fivethirtyeight()

# How about we compare sentiment across time for different settings?
# US
# Speeches
US_oral$year <- paste0("US_Speeches_", stringr::str_extract(US_oral$date, "^[0-9]{4}"))
uoral <- aggregate(US_oral$text, list(US_oral$year), paste, collapse = " ")
uoral <- rename(uoral, doc_id = "Group.1", text = "x")
# Campaign
US_campaign$year <- paste0("US_Campaign_", stringr::str_extract(US_campaign$date, "^[0-9]{4}"))
ucamp <- aggregate(US_campaign$text, list(US_campaign$year), paste, collapse = " ")
ucamp <- rename(ucamp, doc_id = "Group.1", text = "x")
# Debates
US_debates$year <- paste0("US_Debates_",stringr::str_extract(US_debates$Date, "^[0-9]{4}"))
udeb <- aggregate(US_debates$Text, list(US_debates$year), paste, collapse = " ")
udeb <- rename(udeb, doc_id = "Group.1", text = "x")
# Interviews
US_interviews$year <- paste0("US_Interviews_", stringr::str_extract(US_interviews$date, "^[0-9]{4}"))
uint <- aggregate(US_interviews$text, list(US_interviews$year), paste, collapse = " ")
uint <- rename(uint, doc_id = "Group.1", text = "x")
us_setyear <- rbind(uoral, ucamp, udeb, uint)
us_setyear$char <- nchar(us_setyear$text)
# Get sentimen Afinn lexicon
us_setyear_c <- VCorpus(DataframeSource(us_setyear))
us_setyear_t <- tidy(us_setyear_c)
us_setyear_wf <- us_setyear_t %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
us_setyear_af <- inner_join(us_setyear_wf, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
us_setyear_af$date <- stringr::str_extract(us_setyear_af$id, "[0-9]{4}$")
us_setyear_af$setting <- stringr::str_remove(us_setyear_af$id, "_[0-9]{4}$")
us_setyear_af$char <- us_setyear$char
us_setyear_af$n_value <- us_setyear_af$value/us_setyear_af$char
us_setyear_af$date2 <- stringr::str_extract(us_setyear_af$date, "[0-9]{2}$")
# Plot Us to check US
ggplot(us_setyear_af, aes(x = reorder(date2, as.numeric(date)), y = n_value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Sentiment Across Settings and Time Compared in the US",
       subtitle = "Normalized by number of characters for text in dataset",
       caption = "Afinn sentiment lexicon") +
  theme_fivethirtyeight()
# Why the peak in 2019 in US? Interesting...
# BR
# Speeches
BR_oral$year <- paste0("BR_Speeches_", BR_oral$date)
boral <- aggregate(BR_oral$text, list(BR_oral$year), paste, collapse = " ")
boral <- rename(boral, doc_id = "Group.1", text = "x")
# Campaign
BR_Campaign$year <- paste0("BR_Campaign_", BR_Campaign$Date)
bcamp <- aggregate(BR_Campaign$Text, list(BR_Campaign$year), paste, collapse = " ")
bcamp <- rename(bcamp, doc_id = "Group.1", text = "x")
# Debates
BR_debates$year <- paste0("BR_Debates_", stringr::str_extract(BR_debates$Date, "^[0-9]{4}"))
bdeb <- aggregate(BR_debates$Text, list(BR_debates$year), paste, collapse = " ")
bdeb <- rename(bdeb, doc_id = "Group.1", text = "x")
# Interviews
BR_Interviews$year <- paste0("BR_Interviews_", BR_Interviews$Date)
bint <- aggregate(BR_Interviews$Text, list(BR_Interviews$year), paste, collapse = " ")
bint <- rename(bint, doc_id = "Group.1", text = "x")
br_setyear <- rbind(boral, bcamp, bdeb, bint)
br_setyear$char <- nchar(br_setyear$text)
# Get sentimen Afinn lexicon
br_setyear_c <- VCorpus(DataframeSource(br_setyear))
br_setyear_t <- tidy(br_setyear_c)
br_setyear_wf <- br_setyear_t %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
br_setyear_af <- inner_join(br_setyear_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
br_setyear_af$date <- stringr::str_extract(br_setyear_af$id, "[0-9]{4}$")
br_setyear_af$setting <- stringr::str_remove(br_setyear_af$id, "_[0-9]{4}$")
br_setyear_af$char <- br_setyear$char
br_setyear_af$n_value <- br_setyear_af$value/br_setyear_af$char
br_setyear_af$date2 <- stringr::str_extract(br_setyear_af$date, "[0-9]{2}$")
# Plot to check BR
ggplot(br_setyear_af, aes(x = reorder(date2, as.numeric(date)), y = n_value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Sentiment Across Settings and Time Compared in the US",
       subtitle = "Normalized by number of characters for text in dataset",
       caption = "Afinn sentiment lexicon") +
  theme_fivethirtyeight()
# Bind dataframes and plot
sent_af_all <- rbind(us_setyear_af, br_setyear_af)
ggplot(sent_af_all, aes(x = reorder(date2, as.numeric(date)), y = n_value , fill = setting)) +
  geom_line(aes(group = setting)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Sentiment Across Settings and Time Compared for Brazil and the US",
       subtitle = "Normalized by number of characters for text in dataset",
       caption = "Afinn sentiment lexicon") +
  theme_fivethirtyeight()
# Not a lot of variation but for official speeches in 2019? Trump?

# Let's compare settings for a few speakers
# Shall we compare Trump and Obama with Bolsonaro and Lula across settings?
# For obama and Trump, this was partially done in the text_exploration_US script (lines 800-974).
# Please refer to that and run those lines to get the tibles below.
campaign_sentiment_nrc
campaign_sentiment_afinn
# Let's do the same for Bolsonaro and Lula
# Start with speeches as data is loaded
speeches_lb <- data.frame(boral[grepl("Lula|Bolsonaro", boral$presid, ignore.case = TRUE),])
summary(as.factor(speeches_lb$presid)) # 204 obs for Bolsonaro and 2064 for Lula
speeches_lb <- aggregate(speeches_lb$text, list(speeches_lb$presid), paste, collapse = " ")
speeches_lb <- rename(speeches_lb, doc_id = "Group.1", text = "x")
speeches_lb_corpus <- VCorpus(DataframeSource(speeches_lb))
speeches_lb_tidy <- tidy(speeches_lb_corpus)
speeches_lb_wf <- speeches_lb_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
speeches_lb_af <- inner_join(speeches_lb_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
speeches_lb_af$obs <- c(204, 2064)
speeches_lb_af$value <- speeches_lb_af$value/speeches_lb_af$obs
speeches_lb_af$setting <- "speeches"
speeches_lb_nrc <- inner_join(speeches_lb_wf, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
speeches_lb_nrc$obs <- 204
speeches_lb_nrc$obs[11:20] <- 2064
speeches_lb_nrc$value <- speeches_lb_nrc$value/speeches_lb_nrc$obs
speeches_lb_nrc$setting <- "speeches"
# Campaign
load("~/GitHub/Poldis/data/BR_campaign.rda")
camp_lb <- data.frame(BR_Campaign[grepl("Lula|Bolsonaro", BR_Campaign$Speaker, ignore.case = TRUE),])
summary(as.factor(camp_lb$Speaker)) # 29 obs for Bolsonaro and 24 for Lula
camp_lb <- aggregate(camp_lb$Text, list(camp_lb$Speaker), paste, collapse = " ")
camp_lb <- rename(camp_lb, doc_id = "Group.1", text = "x")
camp_lb_corpus <- VCorpus(DataframeSource(camp_lb))
camp_lb_tidy <- tidy(camp_lb_corpus)
camp_lb_wf <- camp_lb_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
camp_lb_af <- inner_join(camp_lb_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
camp_lb_af$obs <- c(29, 24)
camp_lb_af$value <- camp_lb_af$value/camp_lb_af$obs
camp_lb_af$setting <- "campaign"
camp_lb_nrc <- inner_join(camp_lb_wf, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
camp_lb_nrc$obs <- 29
camp_lb_nrc$obs[11:20] <- 24
camp_lb_nrc$value <- camp_lb_nrc$value/camp_lb_nrc$obs
camp_lb_nrc$setting <- "campaign"
# Debates
load("~/GitHub/Poldis/data/BR_debates.rda")
deb_lb <- data.frame(BR_debates[grepl("Lula|Bolsonaro", BR_debates$Speaker, ignore.case = TRUE),])
summary(as.factor(deb_lb$Speaker)) # 2 obs for Bolsonaro and 6 for Lula
deb_lb <- aggregate(deb_lb$Text, list(deb_lb$Speaker), paste, collapse = " ")
deb_lb <- rename(deb_lb, doc_id = "Group.1", text = "x")
deb_lb_corpus <- VCorpus(DataframeSource(deb_lb))
deb_lb_tidy <- tidy(deb_lb_corpus)
deb_lb_wf <- deb_lb_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
deb_lb_af <- inner_join(deb_lb_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
deb_lb_af$obs <- c(2, 6)
deb_lb_af$value <- deb_lb_af$value/deb_lb_af$obs
deb_lb_af$setting <- "debate"
deb_lb_nrc <- inner_join(deb_lb_wf, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
deb_lb_nrc$obs <- 2
deb_lb_nrc$obs[11:20] <- 6
deb_lb_nrc$value <- deb_lb_nrc$value/deb_lb_nrc$obs
deb_lb_nrc$setting <- "debate"
# Interviews
load("~/GitHub/Poldis/data/BR_interviews.rda")
int_lb <- data.frame(BR_Interviews[grepl("Lula|Bolsonaro", BR_Interviews$Speaker, ignore.case = TRUE),])
summary(as.factor(int_lb$Speaker)) # 81 obs for Bolsonaro and 35 for Lula
int_lb <- aggregate(int_lb$Text, list(int_lb$Speaker), paste, collapse = " ")
int_lb <- rename(int_lb, doc_id = "Group.1", text = "x")
int_lb_corpus <- VCorpus(DataframeSource(int_lb))
int_lb_tidy <- tidy(int_lb_corpus)
int_lb_wf <- int_lb_tidy %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
int_lb_af <- inner_join(int_lb_wf, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(value = sum(n))
int_lb_af$obs <- c(81, 35)
int_lb_af$value <- int_lb_af$value/int_lb_af$obs
int_lb_af$setting <- "interview"
int_lb_nrc <- inner_join(int_lb_wf, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(value = sum(n))
int_lb_nrc$obs <- 81
int_lb_nrc$obs[11:20] <- 35
int_lb_nrc$value <- int_lb_nrc$value/int_lb_nrc$obs
int_lb_nrc$setting <- "interview"

# Bind datasets
br_lb_af <- rbind(speeches_lb_af, camp_lb_af, int_lb_af, deb_lb_af)
br_lb_af <- select(br_lb_af, -obs)
br_lb_af$id <- paste0(br_lb_af$id, "_", br_lb_af$setting)
br_lb_af$speaker <- gsub("_.*", "",br_lb_af$id)
setting_af <- rbind(br_lb_af, campaign_sentiment_afinn)
br_lb_nrc <- rbind(speeches_lb_nrc, camp_lb_nrc, int_lb_nrc, deb_lb_nrc)
br_lb_nrc <- select(br_lb_nrc, -obs)
br_lb_nrc$id <- paste0(br_lb_nrc$id, "_", br_lb_nrc$setting)
us_ot_nrc <- select(campaign_sentiment_nrc, -speaker)
setting_nrc <- rbind(br_lb_nrc, us_ot_nrc)

# Plot
af_compared <- ggplot(setting_af, aes(x = setting, y = value , fill = speaker)) +
  geom_line(aes(group = speaker)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "",
       subtitle = "",
       caption = "Afinn sentiment lexicon") +
  theme_fivethirtyeight()

nrc_compared <- ggplot(setting_nrc, aes(reorder(id, as.numeric(factor(setting))) , value, fill = sentiment)) +
  geom_bar(position="stack", stat="identity") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1)) +
  labs(x = "",
       y = "",
       title = "Sentiment in Across Different  Settings Compared",
       subtitle = "Normalized by observations for speaker in dataset",
       caption = "NRC sentiment lexicon") +
  theme_fivethirtyeight() +
  coord_flip()

gridExtra::grid.arrange(nrc_compared, af_compared)
