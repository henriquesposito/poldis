#' Urgency Analysis
#'
#' @param .data A data frame, promises, or text vector.
#' For data frames, function will search for "text" variable.
#' For promises data, function will search for "promises" variable.
#' @param normalization Would you like urgency scores to be normalized?
#' By default, urgency scores are normalized by number of words in urgency
#' dimension and the number of words in text observation.
#' Other options include "tokens" for the number of words in text observation.
#' Users can also declare "none", for no normalization.
#' @return A scored data frame.
#' @import dplyr
#' @examples
#' \donttest{
#' get_urgency(US_News_Conferences_1960_1980[1:10,3])
#' }
#' @export
get_urgency <- function(.data, dictionary, normalize) {
  # get text variable
  if (inherits(.data, "data.frame")) {
    text <- .data["text"]
  } else if (inherits(.data, "promises")) {
    text <- data["promises"]
  } else text <- .data
  # assign urgency dimensions
  out <- text
  out$frequency <- .assign_frequencies(promises)
  out$timing <- .assign_time(promises)
  out$degree <- .assign_degree(promises)
  out$commit <- .assign_commitment(promises)
  if (missing(normalize)) {
    out <- out |>
      dplyr::mutate(urgency = (frequency + timing + degree + commit)/nchar(text)) |>
      dplyr::arrange(-urgency)
  } else if (normalize == "tokens") {
    out <- out |>
      dplyr::mutate(urgency = (frequency + timing + degree + commit)/nchar(text)) |>
      dplyr::arrange(-urgency)
  } else {
    out <- out |>
      dplyr::mutate(urgency = (frequency + timing + degree + commit)) |>
      dplyr::arrange(-urgency)
  }
  class(out) <- c("urgency", class(out))
  out
  # todo: fix how the function works for small numbers of text
  # todo: what about nouns, should we code them using SO-CAL dictionaries?
  # todo: fix normalization scores, how to best do it?
}

.assign_frequencies <- function(promises) {
  frequency_words <-
  out <- data.frame(sentence = 1:(length(promises[["promises"]])))
  for (i in names(unlist(unname(freq_adverbs)))) {
    out[[i]] <- stringr::str_count(promises[["lemmas"]], textstem::lemmatize_strings(i))*
      unlist(unname(freq_adverbs))[[i]]
  }
  rowSums(out[-1])
}

.assign_time <- function(promises) {
  time_adverbs <- list(relative_timepoints = list("last week" = -7/7, "yesterday" = -1/7,
                                                  "then" = 1/7, "today|now" = 7/7,
                                                  "tonight" = 3.5/7,
                                                  "tomorrow" = 4/7, "next week" = 1/7),
                       relational_timepoints = list("formerly" = -8/8, "previously" = -5/8,
                                                    "already|since" = -4/8,
                                                    "lately" = -3/8, "recently" = -2/8,
                                                    "just|still" = -1/8,
                                                    "first" = 8/8, "next" = 7/8,
                                                    "before" = 5.5/8,
                                                    "early|earlier" = 5.5/8,
                                                    "soon" = 4/8,
                                                    "next" = 4/8,
                                                    "after|afterwards" = 3.5/8,
                                                    "late" = 3/8, "later" = 2.5/8,
                                                    "finally" = 2/8, # degree?
                                                    "eventually" = 1/8,
                                                    "at some stage" = 0.5/8))
  out <- data.frame(sentence = 1:(length(promises[["promises"]])))
  for (i in names(unlist(unname(time_adverbs)))) {
    out[[i]] <- stringr::str_count(promises[["lemmas"]], textstem::lemmatize_strings(i))*
      unlist(unname(time_adverbs))[[i]]
  }
  rowSums(out[-1])
}

.assign_degree <- function(promises) {
  # only for adverbs not in SO-CAL list
  degr_adverbs <-  list(very_important = c("especially|completely|decidedly|
                                           |deeply|highly|entirely|practically|
                                           |really|simply|strongly|totally|utterly|
                                           |virtually|urgently|obvious|serious|
                                           |significant|major" = 1),
                        important = c("lots|very|much|most|fully|far|clearly" = 0.5),
                        unimportant = c("somewhat|almost|least|less|indeed|quite|rather" = 0.2))
  out <- data.frame(sentence = 1:(length(promises[["promises"]])))
  for (i in names(unlist(unname(degr_adverbs)))) {
    i <- stringr::str_remove_all(i, "|\n")
    out[[i]] <- stringr::str_count(promises[["lemmas"]], textstem::lemmatize_strings(i))*
      unlist(unname(degr_adverbs))[[i]]
  }
  rowSums(out[-1])
}

.assign_commitment <- function(promises) {
  commit_level <- list(commited = c("will|must|going to|need to|ready to|is time to|
                                    |commit to|promise to|intend to|urge|stand for|
                                    |address|take care of|tackle|fix" = 0.2),
                       not_as_commited = c("should|let's|want to|can|could|may|
                                           |might|shall|would" = 0.1))
  out <- data.frame(sentence = 1:(length(promises[["promises"]])))
  for (i in names(unlist(unname(commit_level)))) {
    out[[i]] <- stringr::str_count(promises[["lemmas"]], textstem::lemmatize_strings(i))*
      unlist(unname(commit_level))[[i]]
  }
  rowSums(out[-1])
}

#' Rank urgent topics
#'
#' @param .data A data frame, promises, or text vector.
#' For data frames, function will search for "text" variable.
#' For promises data, function will search for "promises" variable.
#' @param subjects List of subjects.
#' @import dplyr
#' @importFrom tidyr separate_rows
#' @examples
#' \donttest{
#' get_urgency_rank(US_News_Conferences_1960_1980[1:10,3])
#' }
#' @export
get_urgency_rank <- function(v, subjects) {
  topic <- urgency <- urgency_sum <- NULL
  if (!inherits(v, "urgency")) {
    v <- get_urgency(v, subjects)
  }
  v |> dplyr::mutate(topic = ifelse(topic == "" | is.na(topic), "No topic", topic)) |>
    tidyr::separate_rows(topic, sep = ", ") |>
    dplyr::group_by(topic) |>
    dplyr::summarise(urgency_sum = sum(urgency),
                     urgency_mean = mean(urgency),
                     urgency_median = stats::median(urgency)) |>
    dplyr::arrange(-urgency_sum)
}
