# Modelling

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

# Merge datasets
model <- rbind(BR_model, US_model)
# Should I add a dummy in regerssion for elected or not? And or a dummy for election years?

# Add speaker
model$speaker <- stringr::str_remove(model$doc_id, "_[0-9]{4}")

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
  arrange(id) %>%
  select(-id)
# Bind
sent_BR_NRC <- sent_BR_NRC[,-1]
sent_BR_all <- cbind(sent_BR_af, sent_BR_NRC)
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
# bind BR and US
sent_model <- rbind(sent_US_all, sent_BR_all)

# Bind model data and sentiment
sent_model <- arrange(sent_model, id)
sent_model <- sent_model[,-1]
model$id <- paste0(model$doc_id, "_", model$setting)
model <- arrange(model, id)
model <- cbind(model, sent_model)

# Let's normalize all scores
model_data <- model %>%
  mutate(truth_telling = truth/(length*1000),
         lie_accusations = lies/(length*1000),
         consistency = consistency/(length*1000),
         finger_pointing = fpoint/(length*1000),
         origins = origins/(length*1000),
         common_sense = common_sense/(length*1000),
         anti_pc = anti_PC/(length*1000),
         territory = territory/(length*1000),
         Afinn = Afinn/(length*1000),
         anger = anger/(length*1000),
         anticipation = anticipation/(length*1000),
         disgust = disgust/(length*1000),
         fear = fear/(length*1000),
         joy = joy/(length*1000),
         negative = negative/(length*1000),
         positive = positive/(length*1000),
         sadness = sadness/(length*1000),
         surprise= surprise/(length*1000),
         trust = trust/(length*1000)) %>%
  select(-c(truth, lies, fpoint, anti_PC, length, id, doc_id)) %>% # Remove lenght, id and doc_id
  relocate(speaker, date, setting, settingc)
# create a total frequency variable
model_data$ap_total <- model_data$consistency + model_data$origins + model_data$common_sense +
  model_data$territory + model_data$truth_telling + model_data$lie_accusations +
  model_data$finger_pointing + model_data$anti_pc

# Modelling
mmr <- lm(cbind(truth_telling, lie_accusations, consistency, finger_pointing,
                origins, common_sense, territory, anti_pc, ap_total) ~ speaker + date + setting,
          data = model_data)
summary(mmr)
coef(mmr)
# Not a lot of interesting stuff
manova.mmr <- car::Anova(mmr)
summary(manova.mmr)

# fit a linear model
m1 <- lm(ap_total ~ speaker + date + setting,
         data = model_data)
m2 <- lm(ap_total ~ speaker + date + setting + Afinn + anger + anticipation + disgust +
           fear + joy + negative + positive + sadness + surprise + trust,
         data = model_data)
summary(m1)
summary(m2)

# Okay, le
pols1 <- plm(ap_total ~ speaker + setting, data = model_data, model = "pooling", index = c("date"))
summary(pols1)
fe1 <- plm(ap_total ~ speaker + setting, data = model_data, model = "within", index = c("date"))
summary(fe1)
re1 <- plm(ap_total ~ speaker + setting, data = model_data, model = "random", index = c("date"))
summary(re1)
