# Modelling

# Packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(stringi)
library(ggplot2)
library(ggthemes)
library(tm)
library(tidytext)
library(plm)
library(stargazer)

options(scipen=999)

# Load BR_ap and US_ap datasets in Data folder contain the authenticity performnces for each case.
# Minor bug in data first time ran, fix it when you have time!!!
BR_model <- BR_ap %>% select(-text) %>% filter(setting != "debates")
bb <- BR_ap  %>% select(-text) %>% filter(setting == "debates") %>%
  select(-c(date, setting, settingc))
bb$doc_id <- stringr::str_remove(bb$doc_id, "-[0-9]{2}-[0-9]{2}")
bb <- bb %>%
  group_by(doc_id) %>%
  summarise(across(everything(), sum))
bb$setting <- "debates"
bb$settingc <- "debates_BR"
bb$date <- str_extract(bb$doc_id, "[0-9]{4}")
BR_model <- rbind(BR_model, bb)

# Remove text
# BR_model <- select(BR, -text)
US_model <- select(US_ap, -text)

# Add speaker
BR_model$speaker <- stringr::str_remove(BR_model$doc_id, "_[0-9]{4}")
BR_model$speaker <- gsub("Dilma", "Rousseff", BR_model$speaker)
BR_model$speaker <- gsub("Aecio", "Neves", BR_model$speaker)
BR_model$speaker <- gsub("FHC", "Cardoso", BR_model$speaker)
US_model$speaker <- stringr::str_remove(US_model$doc_id, "_[0-9]{4}")
US_model$speaker <- gsub("Albert Gore, Jr.", "Gore", US_model$speaker)
US_model$speaker <- gsub("Hillary Clinton", "H_Clinton", US_model$speaker)
US_model$speaker <- gsub("George W. Bush", "W_Bush", US_model$speaker)
US_model$speaker <- stringr::word(US_model$speaker, -1) # standardises some speaker names

# First, let's get sentiment scores to serve as controls.
# Sentiment Brazil
sent_BR <- BR %>% select(doc_id, setting, text)
sent_BR$doc_id <- paste0(sent_BR$doc_id, "_", sent_BR$setting)
sent_BR$doc_id <- stringr::str_remove(sent_BR$doc_id, "-[0-9]{2}-[0-9]{2}") # minor bug, fix it later
sent_BR <- VCorpus(DataframeSource(sent_BR))
sent_BR <- tidy(sent_BR)
sent_BR <- sent_BR %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Get Afinn
sent_BR_af <- inner_join(sent_BR, Afinn_pt, by = "word") %>%
  group_by(id) %>%
  summarize(Afinn = sum(n)) %>%
  arrange(id)
# Get NRC
sent_BR_NRC <- inner_join(sent_BR, nrc_portuguese, by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(NRC = sum(n)) %>%
  pivot_wider(names_from = sentiment, values_from = NRC) %>%
  arrange(id)
# Bind
sent_BR_NRC <- sent_BR_NRC[,-1]
sent_BR_all <- cbind(sent_BR_af, sent_BR_NRC)
sent_BR_all <- sent_BR_all[,-1]
BR_model$id <- paste0(BR_model$doc_id, "_", BR_model$setting)
BR_modelling <- BR_model %>%
  arrange(id) %>%
  select(-id)
BR_modelling <- cbind(BR_modelling, sent_BR_all)

# Sentiment US
sent_US <- US_ap %>% select(doc_id, setting, text)
sent_US$doc_id <- paste0(sent_US$doc_id, "_", sent_US$setting)
sent_US <- VCorpus(DataframeSource(sent_US))
sent_US <- tidy(sent_US)
sent_US <- sent_US %>%
  unnest_tokens(word, text) %>%
  count(id, word, sort = TRUE)
# Get Afinn
sent_US_af <- inner_join(sent_US, get_sentiments("afinn"), by = "word") %>%
  group_by(id) %>%
  summarize(Afinn = sum(n)) %>%
  arrange(id)
# Get NRC
sent_US_NRC <- inner_join(sent_US, get_sentiments("nrc"), by = "word") %>%
  group_by(id, sentiment) %>%
  summarize(NRC = sum(n)) %>%
  pivot_wider(names_from = sentiment, values_from = NRC) %>%
  arrange(id)
# bind
sent_US_NRC <- sent_US_NRC[,-1]
sent_US_all <- cbind(sent_US_af, sent_US_NRC)
sent_US_all <- sent_US_all[,-1]
US_model$id <- paste0(US_model$doc_id, "_", US_model$setting)
US_modelling <- US_model %>%
  arrange(id) %>%
  select(-id)
US_modelling <- cbind(US_modelling, sent_US_all)

# Let's normalize all scores
# Brazil
BR_model_final <- BR_modelling %>%
  mutate(truth_telling = (truth/length)*1000,
         lie_accusations = (lies/length)*1000,
         consistency = (consistency/length)*1000,
         finger_pointing = (fpoint/length)*1000,
         origins = (origins/length)*1000,
         common_sense = (common_sense/length)*1000,
         anti_pc = (anti_PC/length)*1000,
         territory = (territory/length)*1000,
         Afinn = (Afinn/length)*1000,
         anger = (anger/length)*1000,
         anticipation = (anticipation/length)*1000,
         disgust = (disgust/length)*1000,
         fear = (fear/length)*1000,
         joy = (joy/length)*1000,
         negative = (negative/length)*1000,
         positive = (positive/length)*1000,
         sadness = (sadness/length)*1000,
         surprise= (surprise/length)*1000,
         trust = (trust/length)*1000,
         ap_total = consistency + origins + common_sense + # create a total frequency variable
           territory + truth_telling + lie_accusations +
           finger_pointing + anti_pc) %>%
  select(-c(truth, lies, fpoint, anti_PC, length, doc_id, settingc)) %>% # Remove lenght, id and doc_id
  relocate(speaker, date, setting)
BR_model_final$speaker <- gsub("Lula", "aa_Lula", BR_model_final$speaker)
BR_model_final$setting <- gsub("speeches", "a_speeches", BR_model_final$setting)
# Just so Lula and Speeches are the refernec category in regression models

# same for the US
US_model_final <- US_modelling %>%
  mutate(truth_telling = (truth/length)*1000,
         lie_accusations = (lies/length)*1000,
         consistency = (consistency/length)*1000,
         finger_pointing = (fpoint/length)*1000,
         origins = (origins/length)*1000,
         common_sense = (common_sense/length)*1000,
         anti_pc = (anti_PC/length)*1000,
         territory = (territory/length)*1000,
         Afinn = (Afinn/length)*1000,
         anger = (anger/length)*1000,
         anticipation = (anticipation/length)*1000,
         disgust = (disgust/length)*1000,
         fear = (fear/length)*1000,
         joy = (joy/length)*1000,
         negative = (negative/length)*1000,
         positive = (positive/length)*1000,
         sadness = (sadness/length)*1000,
         surprise= (surprise/length)*1000,
         trust = (trust/length)*1000,
         ap_total = consistency + origins + common_sense + # create a total frequency variable
           territory + truth_telling + lie_accusations +
           finger_pointing + anti_pc) %>%
  select(-c(truth, lies, fpoint, anti_PC, length, doc_id, settingc)) %>% # Remove lenght, id and doc_id
  relocate(speaker, date, setting)
US_model_final$speaker <- gsub("Obama", "aa_Obama", US_model_final$speaker)
US_model_final$setting <- gsub("speeches", "a_speeches", US_model_final$setting)
# Just so Lula is the refernec category in regression models

# Modelling
# Truth-telling
truth_BR <- plm(truth_telling ~ speaker + setting, data = BR_model_final, model = "pooling", index = "date")
summary(truth_BR)
truth_US <- plm(truth_telling ~ speaker + setting, data = US_model_final, model = "pooling", index = "date")
summary(truth_US)
# a little comparison
stargazer::stargazer(truth_BR, truth_US, type = "text", column.labels = c("BR", "US"))
# Lie accusations
lie_BR <- plm(lie_accusations ~ speaker + setting, data = BR_model_final, model = "pooling", index = c("date"))
lie_US <- plm(lie_accusations ~ speaker + setting, data = US_model_final, model = "pooling", index = c("date"))
# Consistency
con_BR <- plm(consistency ~ speaker + setting, data = BR_model_final, model = "pooling", index = c("date"))
con_US <- plm(consistency ~ speaker + setting, data = US_model_final, model = "pooling", index = c("date"))
# Finger-pointing
fp_BR <- plm(finger_pointing ~ speaker + setting, data = BR_model_final, model = "pooling", index = c("date"))
fp_US <- plm(finger_pointing ~ speaker + setting, data = US_model_final, model = "pooling", index = c("date"))
# Origins
og_BR <- plm(origins ~ speaker + setting, data = BR_model_final, model = "pooling", index = c("date"))
og_US <- plm(origins ~ speaker + setting, data = US_model_final, model = "pooling", index = c("date"))
# Common Sense
cs_BR <- plm(common_sense ~ speaker + setting, data = BR_model_final, model = "pooling", index = c("date"))
cs_US <- plm(common_sense ~ speaker + setting, data = US_model_final, model = "pooling", index = c("date"))
# Anti_PC
anti_pc_BR <- plm(anti_pc ~ speaker + setting, data = BR_model_final, model = "pooling", index = c("date"))
anti_pc_US <- plm(anti_pc ~ speaker + setting, data = US_model_final, model = "pooling", index = c("date"))
# territory
tt_BR <- plm(territory ~ speaker + setting, data = BR_model_final, model = "pooling", index = c("date"))
tt_US <- plm(territory ~ speaker + setting, data = US_model_final, model = "pooling", index = c("date"))
# Total
pol_ap_BR <- plm(ap_total ~ speaker + setting, data = BR_model_final, model = "pooling", index = c("date"))
pol_ap_US <- plm(ap_total ~ speaker + setting , data = US_model_final, model = "pooling", index = c("date"))
# table all
stargazer::stargazer(truth_BR, lie_BR, con_BR, fp_BR, og_BR, cs_BR, anti_pc_BR, tt_BR, pol_ap_BR, type = "text",
                     title = "Pooled OLS for Different Authenticity Performances in Brazil",
                     out = "ap_BR.htm", align=TRUE)
stargazer::stargazer(truth_US, lie_US, con_US, fp_US, og_US, cs_US, anti_pc_US, tt_US, pol_ap_US, type = "text",
                     title = "Pooled OLS for Different Authenticity Performances in the US",
                     out = "ap_US.htm", align=TRUE)

# Let's see for the aggregation
fe_ap_BR <- plm(ap_total ~ speaker + setting , data = BR_model_final, model = "within", index = c("date"))
fe_ap_US <- plm(ap_total ~ speaker + setting, data = US_model_final, model = "within", index = c("date"))
re_ap_BR <- plm(ap_total ~ speaker + setting, data = BR_model_final, model = "random", index = c("date"))
re_ap_US <- plm(ap_total ~ speaker + setting, data = US_model_final, model = "random", index = c("date"))
pol_ap_BRc <- plm(ap_total ~ speaker + setting + Afinn + anger + anticipation + disgust + fear +
                     joy + sadness + surprise + trust, data = BR_model_final, model = "pooling", index = c("date"))
pol_ap_USc <- plm(ap_total ~ speaker + setting + Afinn + anger + anticipation + disgust + fear +
                     joy + sadness + surprise + trust, data = US_model_final, model = "pooling", index = c("date"))
fe_ap_BRc <- plm(ap_total ~ speaker + setting + Afinn + anger + anticipation + disgust + fear +
                     joy + sadness + surprise + trust, data = BR_model_final, model = "within", index = c("date"))
fe_ap_USc <- plm(ap_total ~ speaker + setting + Afinn + anger + anticipation + disgust + fear +
                     joy + sadness + surprise + trust, data = US_model_final, model = "within", index = c("date"))
re_ap_BRc <- plm(ap_total ~ speaker + setting + Afinn + anger + anticipation + disgust + fear +
                     joy + sadness + surprise + trust, data = BR_model_final, model = "random", index = c("date"))
re_ap_USc <- plm(ap_total ~ speaker + setting + Afinn + anger + anticipation + disgust + fear +
                     joy + sadness + surprise + trust, data = US_model_final, model = "random", index = c("date"))
stargazer::stargazer(pol_ap_BR, fe_ap_BR, re_ap_BR, pol_ap_BRc, fe_ap_BRc, re_ap_BRc, type = "text",
                     column.labels = c("Pooled OLS", "Fixed-Effects", "Random-Effects", "Pooled OLS", "Fixed-Effects", "Random-Effects"),
                     title = "Total Authenticity Performances in Brazil in Time",
                     out = "ap_BR_total_c.htm", align=TRUE)
stargazer::stargazer(pol_ap_US, fe_ap_US, re_ap_US, pol_ap_USc, fe_ap_USc, re_ap_USc, type = "text",
                     column.labels = c("Pooled OLS", "Fixed-Effects", "Random-Effects", "Pooled OLS", "Fixed-Effects", "Random-Effects"),
                     title = "Total Authenticity Performances in the US in Time",
                     out = "ap_US_total_c.htm", align=TRUE)
