# Inter-coder reliability
library(dplyr)
library(readxl)
# Data from dropbox was converted/altered and loaded in
HS <- read_excel("Case study/WHO_reform_speeches_coded_sposito (1).xlsx") |>
  rename(promise_hs = Promise)
JH <- read_excel("Case study/WHO_reform_speeches_JH.xlsx", skip = 1) |>
  rename(promise_jh = `“Promise”`)
JT <- read_excel("Case study/WHO_reform_speeches_Jael.xlsx") |>
  rename(promise_jt = promise)
# Join data
intercoder <- inner_join(HS, JT, by = "text")
intercoder <- inner_join(intercoder, JH, by = "text") |>
  select(text, promise_hs, promise_jh, promise_jt, Topic)
# Some changes here and there, so we only match 258 obs (instead of 322)
poldis <- annotate_text(intercoder$text, "sentences") |>
  mutate(promise_poldis = ifelse(stringr::str_detect(tags, " MD ") |
                            stringr::str_detect(sentence,
                            "going to|need to|ready to|is time to|
                            |commit to|promise to|intend to|let's|
                            |plan to|tackle the|fix the|address the"), 1, 0),
         sentence = stringr::str_squish(sentence))
# not great, too much stuff is changed for comparison to be any good but ...
intercoder <- left_join(intercoder, poldis, by = c("text" = "sentence"))
# between coders (for 258 sentences only)
table(intercoder$promise_hs == intercoder$promise_jh) # 81%
table(intercoder$promise_hs == intercoder$promise_jt) # 77%
table(intercoder$promise_jt == intercoder$promise_jh) # 86%
# coders and package (for 79 obs only unfortunately)
table(intercoder$promise_poldis == intercoder$promise_jh) # 81%
table(intercoder$promise_poldis == intercoder$promise_jt) # 86%
table(intercoder$promise_poldis == intercoder$promise_hs) # 82%
# let's also topic data for comparison (automatic)
topics <- get_urgency(intercoder$text)
topics$topic
get_urgency_rank(topics)
# add some topics
topics <- inner_join(intercoder, topics, by = c("text" = "sentence"))
dplyr::tibble(topics$Topic, unlist(topics$topic)) |> print(n = 30)
# Okay, not too bad ...
