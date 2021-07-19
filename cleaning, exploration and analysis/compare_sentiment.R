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
boral_sent_af <- boral_sent_af %>% filter(obs > 20) # removes years with too littles obs (president leaving office)
boral_sent_af$n_value <- boral_sent_af$value/boral_sent_af$obs  # normalized by obs in dataset
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
uoral_sent_af <- uoral_sent_af %>% filter(obs > 20) # removes years with too littles obs (president leaving office)
uoral_sent_af$n_value <- uoral_sent_af$value/uoral_sent_af$obs  # normalized by obs in dataset
uoral_sent_af$date <- paste0(stringr::str_extract(uoral_sent_af$id, "[0-9]{4}"))
uoral_sent_af$id <- stringr::str_replace_all(uoral_sent_af$id, "_[0-9]{4}", "")
# Bind and plot
sent_speech_af <- rbind(boral_sent_af, uoral_sent_af)
sent_speech_af$date_2 <- stringr::str_extract(sent_speech_af$date, "[0-9]{2}$")
sent_speech_af$id <- gsub("W. Bush", "W_Bush", sent_speech_af$id)
sent_speech_af$id_2 <- stringi::stri_extract_last_words(sent_speech_af$id)
ggplot(sent_speech_af, aes(x = reorder(date_2, as.numeric(date)), y = n_value , fill = id_2)) +
  geom_line(aes(group = id)) +
  geom_point(size = 8, shape = 21) +
  labs(x = "",
       y = "",
       title = "Sentiment in Official Speeches Compared for Brazil and the US",
       subtitle = "Normalized by observations for speaker in dataset",
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
