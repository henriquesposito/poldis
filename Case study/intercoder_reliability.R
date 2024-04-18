# Intercoder reliability
# Data from dropbox was coverted/altereted and loaded in
# Join data
intercoder <- inner_join(HS, JT, by = "text")
intercoder <- inner_join(intercoder, JH, by = "text") |>
  select(text, promises_hs, promise_jh, promise_jt, Topic)
# Some changes here and there, so we only match 258 obs (instead of 322)
poldis <- annotate_text(intercoder$text, "sentences") |>
  mutate(promise_poldis = ifelse(stringr::str_detect(tags, " MD ") |
                            stringr::str_detect(sentence,
                            "going to|need to|ready to|is time to|
                            |commit to|promise to|intend to|let's|
                            |plan to|tackle the|fix the|address the"), 1, 0),
         sentence = stringr::str_squish(sentence))
# not great, too much stuff is changed for comparison to be good
intercoder <- left_join(intercoder, poldis, by = c("text" = "sentence"))
# between coders
hs_jh <- intercoder$promises_hs == intercoder$promise_jh
hs_jt <- intercoder$promises_hs == intercoder$promise_jt
jt_jh <- intercoder$promise_jt == intercoder$promise_jh
# coders and package
poldis_jh <- intercoder$promise_poldis == intercoder$promise_jh
poldis_jt <- intercoder$promise_poldis == intercoder$promise_jt
poldis_hs <- intercoder$promise_poldis == intercoder$promises_hs
# get also topic data for comparison
poldis <- get_urgency(intercoder$text) # issue with topics needs fixing
