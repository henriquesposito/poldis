# Packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(janitor)
library(readr)
library(BradleyTerry2)
library(textstem)
library(purrr)
library(usethis)

### Data -----------------------------------------------------------------------

# Load survey
survey <- readRDS("~/GitHub/poldis/data_raw/survey_analysis/Urgency_survey_2_anonymous.rds")
survey <- survey |>
  # Remove screened out participants, those that failed attention check,
  # or that completed the survey extremely fast.
  filter(!is.na(Check) & Check != "later", as.numeric(`Duration (in seconds)`) > 180)
names(survey) <- gsub("\\.\\.\\..*|\\..*","", names(survey)) # fix naming issues
# Get how many time 3rd option was given and selected
about_urgency <- survey |>
  select(where(~ any(grepl("Click to write Choice 3", .x))))
length(grep("Click to write Choice 3", as.matrix(about_urgency))) # 1521
length(as.matrix(about_urgency)) # 64602
1521/(64602 - sum(unlist(lapply(about_urgency, function(x) sum(is.na(x))))))
# 35 % of the times when provided option was selected.
rm(list = c("about_urgency")) # Remove data items to avoid confusion
# Remove not about urgency scores; they still influence analysis since
# number of times term is chosen over others matters.
survey[survey=="Click to write Choice 3"] <- NA
# Load world pairs dfata to code words if needed.
wordpairs <- read_csv("data_raw/survey_analysis/wordpairs.csv") |>
  unite("question_ID", section:qn, sep = "-") |>
  select(question_ID, word) |>
  group_by(question_ID) |>
  summarise(terms = paste0(word, collapse = " ")) |>
  distinct()

# Paired comparisons -----------------------------------------------------------

pair_subset <- function(data, dim) { # function to subset pairs
  data |>
    pivot_longer(everything(), names_to = "question_ID",
                 values_to = "choice") |>
    na.omit() |>
    group_by(question_ID) |>
    mutate(options = paste(unique(choice), collapse = " - ")) |>
    group_by(question_ID, choice) |>
    mutate(n_choice = n(), dim = dim) |>
    ungroup() |>
    distinct() |>
    separate(options, into = c("termA", "termB"), sep = " - ")
}
# Transform pairs data
pairs1 <- rbind(pair_subset(cbind(survey[,21:33], survey[,96:108],
                                  survey[,138:149], survey[,214:225]), dim = "timing"),
               pair_subset(cbind(survey[,67:79], survey[,109:121],
                                 survey[,400:411], survey[,412:423]), dim = "frequency"),
               pair_subset(cbind(survey[,242:256], survey[,273:287],
                                 survey[,80:95], survey[,122:137]), dim = "intensity"),
               pair_subset(cbind(survey[,150:165], survey[,226:241],
                                 survey[,257:272], survey[,288:303]), dim = "commitment"))
# Add missing terms where needed
pairs1 <- left_join(pairs1, wordpairs) |>
  mutate(termB = ifelse(is.na(termB), terms, termB),
         termB = str_squish(str_remove_all(termB, termA))) |>
  select(-c(terms, question_ID)) |>
  relocate(termA, termB, choice, n_choice, dim)
# Mark columns "used" for later.
colnames(survey)[c(21:33, 67:79, 96:108, 109:121, 150:165, 226:241, 242:256,
                   273:287, 138:149, 214:225, 400:411, 412:423, 80:95, 122:137,
                   257:272, 288:303)] <- "pairs1"

# Transform word rankings in to paired comparisons -----------------------------

mat_to_pair <- function(data, n, dim, dtype = "matrix") { # function
  seq <- seq(from = 1, to = ncol(data), by = n)
  out <- list()
  for (i in 1:(ncol(data)/n)) {
    out[[i]] <- data |>
      select(seq[i]:(seq[i] + 2)) |>
      drop_na()
    if (dtype == "matrix") {
      out[[i]] <- out[[i]] |>
        mutate(across(everything(), ~ ifelse(.x == "Very urgent", 1, ifelse(.x == "Urgent", 2, 3))))
    } else if (dtype == "trio") {
      out[[i]] <- out[[i]] |>
        mutate(across(everything(), ~ as.numeric(.x)))
    }
    pair1 <- out[[i]] |>
      select(1:2) |>
      mutate(termA = names(out[[i]])[1],
             termB = names(out[[i]])[2],
             choice = cur_data()[[1]] - cur_data()[[2]],
             choice = case_when(choice > 0 ~ termB,
                                choice < 0 ~ termA,
                                .default = NA)) |>
      na.omit() |>
      group_by(termA, termB, choice) |>
      summarise(n_choice = n()) |>
      ungroup()
    pair2 <- out[[i]] |>
      select(2:3) |>
      mutate(termA = names(out[[i]])[2],
             termB = names(out[[i]])[3],
             choice = cur_data()[[1]] - cur_data()[[2]],
             choice = case_when(choice > 0 ~ termB,
                                choice < 0 ~ termA,
                                .default = NA)) |>
      na.omit() |>
      group_by(termA, termB, choice) |>
      summarise(n_choice = n()) |>
      ungroup()
    out[[i]] <- rbind(pair1, pair2)
  }
  do.call(rbind, out) |> mutate(dim = dim)
}
# Transform matrix ranks into data
pairs2 <- rbind(mat_to_pair(survey[,34:57], n = 3, dim = "timing"),
                mat_to_pair(survey[,166:195], n = 3, dim = "frequency"),
                mat_to_pair(survey[,304:354], n = 3, dim = "commitment"),
                mat_to_pair(survey[,355:381], n = 3, dim = "intensity"))
# Transform rank words trio into pairs
T2_rk <- survey[,58:66]
F2_rk <- survey[,196:213]
I2_rk <- survey[,382:399] # no commitment dimension here?
names(T2_rk) <- left_join(data.frame(question_ID = names(T2_rk)),
                          wordpairs)[[2]] # Add terms as column names
names(F2_rk) <- left_join(data.frame(question_ID = names(F2_rk)), wordpairs)[[2]]
names(I2_rk) <- left_join(data.frame(question_ID = names(I2_rk)), wordpairs)[[2]]
# Use the same function as above to get paired comparisons from data
pairs2 <- rbind(pairs2,
                mat_to_pair(T2_rk, n = 3, dim = "timing", dtype = "trio"),
                mat_to_pair(F2_rk, n = 3, dim = "frequency", dtype = "trio"),
                mat_to_pair(I2_rk, n = 3, dim = "intensity", dtype = "trio"))
rm(list = c("T2_rk", "F2_rk", "I2_rk")) # Remove data items to avoid confusion
# Mark columns "used" for later checks
colnames(survey)[c(34:57, 166:195, 304:354, 355:381, 58:66, 196:213,
                   382:399)] <- "pairs2"

# Inter dimension comparison ----------------------------------------------------

# Dimension ranking questions
ID_comp <- survey[,424:435] |> mutate(across(everything(), ~ as.numeric(.x)))
names(ID_comp) <- c("commitment1", "frequency1", "intensity1", "timing1",
                    "timing2", "intensity2" , "frequency2", "commitment2",
                    "commitment3", "frequency3" , "intensity3", "timing3")
# Means, coeficient, and expected order comparisons
comp_tab <- data.frame(term = c("important", "everyday", "extensively", "tonight",
                                "after", "slightly", "usually", "possible",
                                "ready", "steadly", "remarkably", "speedy"),
                        dim = c("commitment", "frequency", "intensity", "timing",
                                "timing", "intensity" , "frequency", "commitment",
                                "commitment", "frequency" , "intensity", "timing"),
                        compt = c("high", "high", "high", "high",
                                 "low", "low", "low", "low",
                                 "medium", "medium", "medium", "medium"),
                        coef_s1 = c(-.39, -.38, -.35, -0.38,
                                    -4.36, -3.76, -4.27, -4.59,
                                    -1.94, -2.81, -0.51, -2.89),
                        mean_score_s2 = c(mean(ID_comp$commitment1),
                                          mean(ID_comp$frequency1),
                                          mean(ID_comp$intensity1),
                                          mean(ID_comp$timing1),
                                          mean(ID_comp$commitment2),
                                          mean(ID_comp$frequency2),
                                          mean(ID_comp$intensity2),
                                          mean(ID_comp$timing2),
                                          mean(ID_comp$commitment3),
                                          mean(ID_comp$frequency3),
                                          mean(ID_comp$intensity3),
                                          mean(ID_comp$timing3))) |>
  group_by(compt) |>
  mutate(expected_order = with_order(order_by = coef_s1*-1, fun = row_number,
                                     x = coef_s1*-1),
         survey_order = with_order(order_by = mean_score_s2, fun = row_number,
                                     x = mean_score_s2),
         dif_order = expected_order - survey_order) |>
  ungroup()
# Timing and commitment appear to rank higher than expected,
# frequency and intensity appear to rank lower than expected;
# but how can we use these scores to inform urgency?
comp_tab |> # mean comparison
  group_by(dim) |>
  summarise(mean_dim_score = mean(mean_score_s2),
            mean_dim_dif = mean(expected_order)-mean_dim_score)
# In terms of importance, perhaps intensity dilutes commitment when together?
# In terms of proximity, perhaps frequency dilutes timing when together?
# We could add importance and proximity in that case, no?
summary(lm(mean_score_s2 ~ dim + expected_order, data = comp_tab))
# Though correlations might not be statistically significant and could use
# further validation...
ID_sent1 <- survey[,436:440] # Dimension addition in sentences
ID_sent2 <- survey[,441:445]
ID_sent3 <- survey[,446:450]
names(ID_sent1) <- 1:5 # set names and join data
names(ID_sent2) <- 1:5
names(ID_sent3) <- 1:5
ID_sent <- rbind(ID_sent1, ID_sent2, ID_sent3) |> drop_na()
ID_sent <- data.frame(survey_mean = t(ID_sent |>
  mutate(across(everything(), ~ mean(as.numeric(.x)))) |>
  distinct())) |> # make into table and add example senetnces
  mutate(sentences = c("We must implement these measures to limit inflation now",
                      "We should implement these measures to limit inflation now",
                      "We must implement these measures to limit inflation",
                      "We must vigorously implement these measures to limit inflation",
                      "We must constantly implement these measures to limit inflation"))
# library(poldis) # version 0.1.3
# get_urgency(c("We must implement these measures to limit inflation now.", #0.5718750
#               "We should implement these measures to limit inflation now.", #0.5137451
#               "We must implement these measures to limit inflation.", #0.3812500
#               "We must vigorously implement these measures to limit inflation.", #0.4752841
#               "We must constantly implement these measures to limit inflation.")) #0.5474162
# # expected order S1 = 1, 3, 5, 4, 2
comp_sent <- data.frame(sentences = c("We must implement these measures to limit inflation now", #0.5718750
                                      "We should implement these measures to limit inflation now", #0.5137451
                                      "We must implement these measures to limit inflation", #0.3812500
                                      "We must vigorously implement these measures to limit inflation", #0.4752841
                                      "We must constantly implement these measures to limit inflation"),
                        urgency_score_S1 = c(0.5718750, 0.5137451, 0.3812500,
                                             0.4752841, 0.5474162),
                        timing = c(1,1,0,0,0),
                        frequency = c(0,0,0,0,1),
                        intensity = c(0,0,0,1,0),
                        commitment = c(1,0.5,1,1,1)) |>
  left_join(ID_sent) |> # merge urgency scores, dimensions and more
  mutate(n_dim = timing + frequency + intensity + commitment,
         survey_order = with_order(order_by = survey_mean, fun = row_number,
                                   x = survey_mean),
         expected_order = with_order(order_by = urgency_score_S1*-1, fun = row_number,
                                     x = urgency_score_S1*-1),
         dif_order = expected_order - survey_order)
# Investigate correlation between dimensions and urgency
cor.test(comp_sent$survey_order, comp_sent$n_dim, method= "kendall")
# The p-value of 0.052 is almost at the threshold (alpha = 0.05),
# indicating some evidence to reject the null hypothesis that there could be a
# correlation between number of dimensions and order.
# That is, there is an additive component to dimensions.
# Furthermore, sentence containing commitment and timing ranks higher than
# others as expected; or higher than expected for the second sentence.

# Demographics -----------------------------------------------------------------

#demographics  <- survey[,c(451, 453, 454, 456:457)]
# Not used for now but maybe later as heterogeneous treatment effects.
rm(list = c("ID_comp", "ID_sent", "ID_sent1", "ID_sent2", "ID_sent3"))
# Remove data items to avoid confusion

### Models ---------------------------------------------------------------------

pairs <- rbind(pairs1, pairs2) |> # Merge paired data into expected format
  mutate(win1 = ifelse(termA == choice, n_choice, NA),
         win2 = ifelse(termB == choice, n_choice, NA)) |>
  select(-c(choice, n_choice)) |>
  mutate(word1 = str_squish(stem_strings(str_to_lower(termA))),
         word2 = str_squish(stem_strings(str_to_lower(termB)))) |>
  group_by(word1, word2) |>
  fill(win1, .direction = "downup") |>
  fill(win2, .direction = "downup") |>
  ungroup() |>
  distinct() |>
  group_by(word1) |>
  mutate(termA = paste(unique(str_to_lower(termA)), collapse = "|")) |>
  group_by(word2) |>
  mutate(termB = paste(unique(str_to_lower(termB)), collapse = "|")) |>
  ungroup() |>
  distinct() |>
  mutate(across(everything(), ~ replace_na(.x, 0))) |>
  group_by(word1, word2) |>
  mutate(win1 = sum(win1), win2 = sum(win2)) |>
  ungroup() |>
  distinct() |> # add key to reduce duplication
  mutate(combo_key = map2_chr(word1, word2, ~paste(sort(c(.x, .y)), collapse = "-"))) |>
  group_by(combo_key) |>
  mutate(count = n()) |>
  ungroup() |>
  distinct()
# Filter duplicates
dup1 <- pairs |>
  mutate(dup = paste0(word1, "-", word2)) |>
  filter(count > 1, dup == combo_key)
dup2 <- pairs |>
  mutate(dup = paste0(word1, "-", word2)) |>
  filter(count > 1, dup != combo_key) |>
  rename(word1 = word2, win1 = win2, word2 = word1, win2 = win1,
         termA = termB, termB = termA)
dup <- full_join(dup1, dup2) |>
  group_by(word1, word2) |>
  mutate(win1 = sum(win1), win2 = sum(win2)) |>
  ungroup() |>
  select(-c(dup)) |>
  distinct()
pairs <- pairs |>
  filter(count < 2) |>
  full_join(dup) |>
  select(-count)
rm(list = c("pairs1", "pairs2", "dup", "dup1", "dup2"))
# Remove data items to avoid confusion

# Bradley Terry model ----------------------------------------------------------

filt_lvl <- function(data, dimension) { # Function to adjust levels for models
  out <- filter(data, dim == dimension)
  lvls <- out[c("word1", "word2")] |> unlist() |> as.character() |> unique()
  out[c("word1", "word2")] <- lapply(out[c("word1", "word2")], factor, levels = lvls)
  out
}
# Run models by dimension
timing_BT <- BTm(cbind(win1, win2), word1, word2, formula = ~ word,
                 id = "word", br = TRUE, refcat = "soon", na.action = na.omit,
                 data = filt_lvl(pairs, "timing"))
commitment_BT <- BTm(cbind(win1, win2), word1, word2, formula = ~ word,
                     id = "word", br = TRUE, refcat = "mai", na.action = na.omit,
                     data = filt_lvl(pairs, "commitment"))
intensity_BT <- BTm(cbind(win1, win2), word1, word2, formula = ~ word,
                    id = "word", br = TRUE, refcat = "some", na.action = na.omit,
                    data = filt_lvl(pairs, "intensity"))
frequency_BT <- BTm(cbind(win1, win2), word1, word2, formula = ~ word,
                    id = "word", br = TRUE, refcat = "usual", na.action = na.omit,
                    data = filt_lvl(pairs, "frequency"))
# Update reference categories to smallest values
timing_BT$coefficients == min(timing_BT$coefficients, na.rm = TRUE) # eventu
commitment_BT$coefficients == min(commitment_BT$coefficients, na.rm = TRUE) # might
intensity_BT$coefficients == min(intensity_BT$coefficients, na.rm = TRUE) # everywher
frequency_BT$coefficients == min(frequency_BT$coefficients, na.rm = TRUE) # everi year
timing_BT <- update(timing_BT, refcat = "eventu")
commitment_BT <- update(commitment_BT, refcat = "might")
intensity_BT <- update(intensity_BT, refcat = "everywher")
frequency_BT <- update(frequency_BT, refcat = "everi year")
# Check NAs with intensity and timing
summary(is.na(intensity_BT$coefficients)) # 4 NAs?
intensity_BT$coefficients[is.na(intensity_BT$coefficients)]
summary(is.na(frequency_BT$coefficients)) # 2 NAs?
frequency_BT$coefficients[is.na(frequency_BT$coefficients)]
# Check, clean and replace NA scores when/where possible
BT_models <- rbind(data.frame(word = str_remove_all(names(na.omit(commitment_BT$coefficients)), "word"),
                              coefficients = na.omit(commitment_BT$coefficients),
                              se = unname(summary(commitment_BT)[["coefficients"]][,2]),
                              z = unname(summary(commitment_BT)[["coefficients"]][,3]),
                              prob_z = unname(summary(commitment_BT)[["coefficients"]][,4]),
                              dimension = "commitment"),
                   data.frame(word = str_remove_all(names(na.omit(intensity_BT$coefficients)), "word"),
                              coefficients = na.omit(intensity_BT$coefficients),
                              se = unname(summary(intensity_BT)[["coefficients"]][,2]),
                              z = unname(summary(intensity_BT)[["coefficients"]][,3]),
                              prob_z = unname(summary(intensity_BT)[["coefficients"]][,4]),
                              dimension = "intensity"),
                   data.frame(word = str_remove_all(names(timing_BT$coefficients), "word"),
                              coefficients = timing_BT$coefficients,
                              se = unname(summary(timing_BT)[["coefficients"]][,2]),
                              z = unname(summary(timing_BT)[["coefficients"]][,3]),
                              prob_z = unname(summary(timing_BT)[["coefficients"]][,4]),
                              dimension = "timing"),
                   data.frame(word = str_remove_all(names(na.omit(frequency_BT$coefficients)), "word"),
                              coefficients = na.omit(frequency_BT$coefficients),
                              se = unname(summary(frequency_BT)[["coefficients"]][,2]),
                              z = unname(summary(frequency_BT)[["coefficients"]][,3]),
                              prob_z = unname(summary(frequency_BT)[["coefficients"]][,4]),
                              dimension = "frequency")) |>
  arrange(desc(coefficients)) |>
  rbind(data.frame(word = c("eventu", "might", "everywher", "everi year"),
                   coefficients = 0, se = NA, z = NA, prob_z = NA,
                   dimension = c("timing", "commitment", "intensity", "frequency"))) # add reference words
# Test: calculating urgency by adding log odds (and dividing by must score)
new_urgency <- function(v, data) {
  commitment_dic <- filter(data, dimension == "commitment")
  intensity_dic <- filter(data, dimension == "intensity")
  timing_dic <- filter(data, dimension == "timing")
  frequency_dic <- filter(data, dimension == "frequency")
  v <- stem_strings(unlist(str_split(str_to_lower(str_squish(v)), " ")))
  for (i in v) {
    if (any(grepl(paste0('^', i,'$'), data$word))) {
      if (any(grepl(paste0('^', i,'$'), commitment_dic$word))) {
        Comm <- filter(commitment_dic, word == i)[["coefficients"]]
      }
      if (any(grepl(paste0('^', i,'$'), intensity_dic$word))) {
        Int <- filter(intensity_dic, word == i)[["coefficients"]]
      }
      if (any(grepl(paste0('^', i,'$'), timing_dic$word))) {
        Tim <- filter(timing_dic, word == i)[["coefficients"]]
      }
      if (any(grepl(paste0('^', i,'$'), frequency_dic$word))) {
        Freq <- filter(frequency_dic, word == i)[["coefficients"]]
      }
    }
  }
  if (!exists("Comm", inherits = FALSE)) Comm <- 0
  if (!exists("Int", inherits = FALSE)) Int <- 0
  if (!exists("Tim", inherits = FALSE)) Tim <- 0
  if (!exists("Freq", inherits = FALSE)) Freq <- 0
  (Comm+Int+Tim+Freq)/filter(commitment_dic, word == "must")[["coefficients"]]
  # I think it is okay to divide the log odds coefficients by a single value,
  # since the probabilities will remain identical after division?
}
# Check results in comparison to survey and to sentence rankings
comp_sent$new_urgency <- unlist(lapply(comp_sent$sentences, function(x)
  new_urgency(x, data = BT_models)))
comp_sent |>
  select(sentences, urgency_score_S1, expected_order, survey_order, new_urgency) |>
  mutate(new_urgency_order = with_order(order_by = new_urgency*-1, fun = row_number,
                          x = new_urgency*-1)) # Ok, kind of works reasonably!
BT_models <- rbind(select(pairs, termA, word1) |> rename(terms = termA, word = word1),
      select(pairs, termB, word2) |> rename(terms = termB, word = word2)) |>
  distinct() |> # Merge words and terms
  full_join(BT_models) |>
  group_by(word) |>
  mutate(terms = paste(unique(str_to_lower(terms)), collapse = "|"),
         terms = paste(unique(unlist(str_split(terms, "\\|"))), collapse = "|")) |>
  ungroup() |>
  distinct() |>
  drop_na(coefficients)
# Add close synonyms to data
syn_scores <- function(synonym_list, v) {
  out <- data.frame(word = "",
                    synonyms = unlist(c(synonym_list, names(synonym_list))))
  for (i in seq_len(length(out[["synonyms"]]))) {
    out[["word"]][[i]] <- ifelse(any(stem_strings(str_squish(out[["synonyms"]][[i]])) == v),
                                 stem_strings(str_squish(out[["synonyms"]][[i]])), NA)
  }
  alt <- data.frame(alt1 = names(synonym_list), alt2 = unlist(synonym_list))
  a <- full_join(out, alt, by = c("synonyms" = "alt1")) |> drop_na(alt2) |>
    group_by(alt2) |> fill(word, .direction = "downup") |> ungroup() |>
    drop_na() |> distinct() |> rename(alt = alt2)
  b <- full_join(out, alt, by = c("synonyms" = "alt2")) |> drop_na(alt1) |>
    group_by(alt1) |> fill(word, .direction = "downup") |> ungroup() |>
    drop_na() |> distinct() |> rename(alt = alt1)
  ab <- rbind(a, b) |> group_by(word) |>
    summarise(synonyms = paste(unique(synonyms),  collapse = "|"),
              alternative = paste(unique(alt), collapse = "|")) |>
    ungroup() |> distinct() |>
    unite("alternative", synonyms:alternative, sep = "|")
  out <- full_join(out, ab) |> distinct() |> drop_na(word) |>
    unite("synonyms", synonyms:alternative, sep = "|") |>
    group_by(word) |>
    summarise(alt_synonyms = paste(unique(unlist(str_split(synonyms, "\\|"))),
                               collapse = "|")) |>
    ungroup() |> distinct()
  out
}
# synonyms list
l_synonym <- list("hasty" = "with haste", "hastily" = "with haste",
                  "immediate" = "immediately", "pervasive" = "extensive",
                  "prevalent" = "extensive",
                  "plan to" = "promise to", "small" = "low",
                  "marginally" = "moderate",
                "imminent" = "imminently", "prompt" = "promptly",
                "quick" = "quickly", "speedy" = "speedily",
                "short" = "shortly", "earlier" = "early",
                "rapid" = "rapidly", "late" = "later",
                "slow" = "slowly", "constant" = "constantly",
                "by the minute" = "hourly", "by the hour" = "hourly",
                "everyday" = "daily", "nightly" = "daily",
                "every night" = "daily", "persistent" = "persistently",
                "relentless" = "relentlessly", "incessant" = "incessantly",
                "unrelentingly" = "relentlessly", "interminable" = "incessantly",
                "interminably" = "incessantly", "fortnightly" = "weekly",
                "yearly" = "annually", "every minute" = "hourly",
                "every month" = "monthly",
                "normally" = "usually", "regular" = "regularly",
                "steady" = "steadily", "frequent" = "frequently",
                "progressive" = "progressively", "gradual" = "gradually",
                "sporadic" = "sporadically", "infrequent" = "infrequently",
                "irregular" = "irregularly", "intermittent" = "intermittently",
                "rare" = "rarely", "necessarily" = "necessary",
                "importantly" = "important", "obligatory" = "oblige to",
                "require" = "need to", "commit to" = "promise to",
                "intend to" = "promise to", "urge" = "going to",
                "relevantly" = "relevant", "eminent" = "important",
                "prominent" = "important", "prominently" = "important",
                "preeminent" = "important", "pre-eminent" = "important",
                "momentous" = "important", "worthy" = "invaluable",
                "quintessential" = "essential", "valuable" = "invaluable",
                "unavoidably" = "unavoidable", "inevitable" = "unavoidable",
                "inevitably" = "unavoidable",  "inexorable" = "unavoidable",
                "inexorably" = "unavoidable", "let us" = "let's",
                "shall" = "should",  "plan to" = "intend to",
                "marginal" = "optional", "tenable" = "able",
                "doable" = "able", "plausible" = "possible",
                "plausibly" = "possible", "possibly" = "possible",
                "useable" = "useful", "ideally" = "ideal",
                "carefully" = "cautious", "obvious" = "able",
                "vague" = "ambiguous", "option" = "optional",
                "preferably" = "preferable",  "deeply" = "extremely",
                "high" = "highly", "definite" = "definitely",
                "surely" = "certainly", "certain" = "certainly",
                "utterly" = "completely", "absolutely" = "completely",
                "urgent" = "urgently", "huge" = "enormous",
                "large" = "enormous", "largest" = "enormous",
                "big" = "enormous", "biggest" = "enormous",
                "global" = "globally",
                "prevalent" = "pervasive", "intensely" = "intense",
                "extensively" = "extensive",
                "extraordinary" = "exceptional", "remarkably" = "remarkable",
                "intensive" = "intensively", "immense" = "enormous",
                "vast" = "enormous", "ultimate" = "enormous",
                "ambitiously" = "determined", "ambitious" = "determined",
                "realistically" = "realistic",
                "moderate" = "moderately", "substantial" = "substantially",
                "lots" = "substantially", "plenty" = "substantially",
                "totally" = "fully",
                "entirely" = "fully", "far" = "substantially",
                "clearly" = "clear", "least" = "almost",
                "marginally" = "limited", "minimum" = "minimal",
                "weakly" = "slightly", "adequate" = "adequately",
                "sufficient" = "enough", "near" = "nearly",
                "slight" = "slightly", "much" = "more",
                "reasonable" = "reasonably", "wide" = "extensive",
                "widely" = "extensive", "widespread" = "extensive",
                "plenti of" = "average", "lot of" = "nearly",
                "persist" = "frequently")
# Merge data and synonyms
BT_models <- full_join(BT_models, syn_scores(synonym_list = l_synonym,
                                             v = BT_models$word))
# Make data longer and save final
BT_models <- BT_models |>
  group_by(word) |>
  mutate(alt_synonyms = ifelse(is.na(alt_synonyms), "", alt_synonyms),
         terms = paste0(str_squish(terms), "|", str_squish(alt_synonyms)),
         terms = paste(unique(unlist(str_split(terms, "\\|"))), collapse = "|"),
         terms = str_remove_all(terms, "\\|$|\\|\\|")) |>
  ungroup() |>
  distinct() |>
  separate_rows(terms, sep = "\\|") |>
  mutate(synonym = ifelse(stem_strings(terms) != word, 1, 0)) |>
  select(-c(alt_synonyms)) |>
  rename(word_stem = word) |>
  relocate(terms, coefficients, dimension) |>
  mutate(across(se:prob_z, ~ ifelse(synonym == 1, NA, .x)))

#saveRDS(BT_models, "BT_models.rds") # save data for later
