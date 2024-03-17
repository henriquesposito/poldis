#' Urgency Analysis
#'
#' @param v Text vector.
#' @param n Number of subjects.
#' @return A scored data frame.
#' @import dplyr
#' @export
get_urgency <- function(v, n = 20) {
  frequency <- timing <- topic <- degree <- urgency <- NULL
  words <- annotate_text(v)
  sentences <- annotate_text(v, "sentences")
  promises <- extract_promises(sentences)
  subjects <- extract_subjects(words, n = n)
  similar_words <- extract_related_terms(v, subjects)
  promises |>
    dplyr::mutate(topic = .assign_subjects(promises, similar_words),
                  frequency = .assign_frequencies(promises),
                  timing = .assign_time(promises),
                  degree = .assign_degree(promises),
                  urgency = frequency + timing + degree) |>
    dplyr::arrange(-urgency)
  # todo: adjust frequency, timing, and degree
}

.assign_subjects <- function(promises, subjects) {
  subjects <- lapply(subjects, function(x) paste0(x, collapse = "|"))
  out = list()
  for (i in names(subjects)) {
    out[[i]] <- stringr::str_count(promises$sentence, subjects[[i]])
  }
  out <- apply(data.frame(out), 1, function(i) which(i > 0))
  out <- lapply(out, function(x) paste0(names(x), collapse = ", "))
  out
}

.assign_frequencies <- function(promises) {
  frequently <- sometimes <- infrequently <- NULL
  # freq_adverbs <- list(definite = c("once", "twice", "annually", "daily",
  #                                   "fortnightly", "hourly", "monthly", "nightly",
  #                                   "quarterly", "weekly", "yearly"),
  #                      indefinite = c("always", "usually", "often", "sometimes",
  #                                     "rarely", "never", "ever", "hardly ever",
  #                                     "occasionally", "seldom", "generally",
  #                                     "frequently", "normally", "constantly",
  #                                     "infrequently", "regularly"))
  freq_adverbs <- list(frequently = c("daily", "fortnightly", "hourly", "nightly",
                                    "weekly", "always", "usually", "often",
                                    "generally", "frequently", "normally",
                                    "constantly", "regularly"),
                       sometimes = c("twice", "annually","monthly",
                                     "quarterly", "sometimes",
                                     "occasionally"),
                       infrequently = c("once", "yearly", "rarely", "never",
                                      "ever", "hardly ever", "seldom",
                                      "infrequently"))
  frequency <- lapply(freq_adverbs, function(x) paste0(x, collapse = "|"))
  out <- list()
  for (i in names(freq_adverbs)) {
    out[[i]] <- stringr::str_count(promises$sentence, frequency[[i]])
  }
  out <- data.frame(out) |>
    dplyr::mutate(frequently = frequently*2,
                  infrequently = infrequently*(-1),
                  frequency = infrequently + sometimes + frequently)
  out$frequency
}

.assign_time <- function(promises) {
  short_term <- long_term <- NULL
  # time_adverbs <- list(relative_timepoints = c("yesterday", "then", "now",
  #                                              "today", "tonight", "tomorrow"),
  #                      relational_timepoints = c("already", "daily", "last", "next",
  #                                                "previously", "after", "afterwards",
  #                                                "early", "late", "later", "since",
  #                                                "still", "just", "seldom", "before",
  #                                                "early", "earlier", "eventually",
  #                                                "finally", "first", "formerly",
  #                                                "just", "lately", "recently",
  #                                                "soon", "yet"))
  time_adverbs <- list(short_term = c("now", "today", "tonight", "tomorrow",
                                      "next", "early", "daily", "early", "earlier",
                                      "just", "first", "recently", "soon"),
                       long_term = c("after", "afterwards", "late", "later",
                                     "since", "last", "still", "just", "eventually",
                                     "finally", "formerly", "lately", "yet"))
  timing <- lapply(time_adverbs, function(x) paste0(x, collapse = "|"))
  out <- list()
  for (i in names(time_adverbs)) {
    out[[i]] <- stringr::str_count(promises$sentence, timing[[i]])
  }
  out <- data.frame(out) |>
    dplyr::mutate(short_term = short_term*2,
                  timing = long_term + short_term)
  out$timing
}

.assign_degree <- function(promises) {
  important <- unimportant <- NULL
  # degr_adverbs <- c("lots", "somewhat", "barely", "very", "much", "most", "nearly",
  #                   "too", "extremely", "enough", "so", "slightly", "especially",
  #                   "just", "almost", "scarcely", "virtually", "fully", "far",
  #                   "exceptionally", "absolutely", "awfully", "badly", "completely",
  #                   "decidedly", "deeply", "enormously", "entirely", "fairly",
  #                   "greatly", "hardly", "highly", "how", "incredibly", "indeed",
  #                   "intensely", "just", "least", "less", "little", "nearly",
  #                   "perfectly", "positively", "practically", "pretty", "purely",
  #                   "quite", "rather", "really", "scarcely", "simply", "strongly",
  #                   "terribly", "thoroughly", "totally", "utterly", "very",
  #                   "virtually", "well")
  degr_adverbs <-  list(important = c("lots", "very", "much", "most", "extremely",
                                      "especially", "fully", "far", "exceptionally",
                                      "absolutely", "awfully", "badly", "completely",
                                      "decidedly", "deeply", "greatly", "highly",
                                      "incredibly", "enormously", "entirely",
                                      "intensely", "perfectly", "positively",
                                      "practically", "purely", "really", "scarcely",
                                      "simply", "strongly", "terribly", "thoroughly",
                                      "totally", "utterly", "very", "enough"),
                        unimportant = c("somewhat", "barely", "slightly", "almost",
                                        "scarcely", "virtually", "hardly", "least",
                                        "less", "little", "nearly", "fairly", "indeed",
                                        "pretty", "quite", "rather", "too"))
  degree <- lapply(degr_adverbs, function(x) paste0(x, collapse = "|"))
  out <- list()
  for (i in names(degr_adverbs)) {
    out[[i]] <- stringr::str_count(promises$sentence, degree[[i]])
  }
  out <- data.frame(out) |>
    dplyr::mutate(important = important*2,
                  degree = important + unimportant)
  out$degree
}
