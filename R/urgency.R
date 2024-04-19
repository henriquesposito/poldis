#' Urgency Analysis
#'
#' @param v Text vector or annotated data frame.
#' @param subjects A vector or a list of subjects and related terms.
#' If not declared, package will try to automatically detect subjects
#' and related terms to these subjects.
#' If users declare a vector, each element is treated as a independent topic
#' and related terms to each of the elements will be automatically
#' identified if possible.
#' If users declare a list of subjects and related terms, function understands
#' these as the topics and the related terms.
#' @return A scored data frame.
#' @import dplyr
#' @importFrom usethis ui_done ui_info
#' @examples
#' \donttest{
#' get_urgency(US_News_Conferences_1960_1980[1:10,3])
#' get_urgency(US_News_Conferences_1960_1980[1:10,3],
#'             subjects = c("war", "inflation"))
#' get_urgency(US_News_Conferences_1960_1980[1:10,3],
#'             subjects = list("war" = c("war", "military", "guns"),
#'                             "inflation" = c("inflation", "interest rates", "prices")))
#' }
#' @export
get_urgency <- function(v, subjects) {
  frequency <- timing <- topic <- degree <- urgency <- commit <- NULL
  if (any(class(v) == "data.frame") & !"doc_id" %in% names(v)) {
    stop("Please declare a text vector or an annotated object.")
  }
  if (any(class(v) == "promises")) promises <- v
  else {
    promises <- extract_promises(v)
    usethis::ui_done("Extracted promises.")
  }
  if (missing(subjects)) {
    subjects <- extract_subjects(promises)
    usethis::ui_done("Extracted subjects.")
  }
  if (is.list(subjects) | "related_subjects" %in% class(subjects)) {
    similar_words <- subjects
  } else {
    similar_words <- tryCatch({
      extract_related_terms(promises, subjects)
      usethis::ui_done("Extracted similar topics for subjects.")
    }, error = function(e) {
      usethis::ui_info("Failed to identify related terms, subjects will be used to code topics.")
      subjects
    })
  }
  usethis::ui_info("Coding urgency components...")
  out <- promises
  out$topic <- .assign_subjects(promises, similar_words)
  out$frequency <- .assign_frequencies(promises)
  out$timing <- .assign_time(promises)
  out$degree <- .assign_degree(promises)
  out$commit <- .assign_commitment(promises)
  out$adjectives <- .assign_adj(promises)
  out$adverbs <- .assign_adv(promises)
  out <- out |>
    dplyr::mutate(urgency = (frequency + timing + degree + commit +
                               adjectives + adverbs)/ntoken) |>
    dplyr::arrange(-urgency)
  class(out) <- c("urgency", class(out))
  out
  # todo: fix how the function works for small numbers of text
  # todo: what about nouns, should we code them using SO-CAL dictionaries?
  # todo: fix normalization scores, how to best do it?
}

.assign_subjects <- function(promises, subjects) {
  if (is.list(subjects)) {
    subjects <- lapply(subjects, function(x) paste0(x, collapse = "|"))
  } else names(subjects) <- subjects
  out = list()
  for (i in names(subjects)) {
    out[[i]] <- stringr::str_count(promises[["promises"]], subjects[[i]])
  }
  out <- apply(data.frame(out), 1, function(i) which(i > 0))
  out <- lapply(out, function(x) paste0(names(x), collapse = ", "))
  # todo: get nouns if empty?
  out
}

.assign_frequencies <- function(promises) {
  freq_adverbs <- list(definite = list("by the minute|by the hour|hourly" = 1,
                                       "daily|nightly" = 365/8765,
                                       "weekly" = 52/8765,
                                       "fortnightly" = 26/8765,
                                       "monthly" = 12/8765,
                                       "quarterly" = 4/8765,
                                       "annually|yearly" = 1/8765),
                       indefinite = list("always|constantly" = 1,
                                         "usually|regularly" = 0.9,
                                         "generally|normally" = 0.8,
                                         "often|frequently" = 0.7,
                                         "sometimes" = 0.5,
                                         "occasionally" = 0.3,
                                         "seldom" = 0.2,
                                         "infrequently" = 0.1,
                                         "rarely" = 0.05,
                                         "hardly ever" = 0.02,
                                         "never" = 0))
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

.assign_adv <- function(promises) {
  out <- data.frame(sentence = 1:(length(promises[["promises"]])))
  for (i in 1:length(adverbs[,1])) {
    out[[adverbs[,1][i]]] <- stringr::str_count(promises[["adverbs"]], adverbs[,3][i])*
      abs(adverbs[,2][i]/5) # absolute value since we do not care about direction
  }
  rowSums(out[-1])
}

.assign_adj <- function(promises) {
  out <- data.frame(sentence = 1:(length(promises[["promises"]])))
  for (i in 1:length(adjectives[,1])) {
    out[[adjectives[,1][i]]] <- stringr::str_count(promises[["adjectives"]], adjectives[,3][i])*
      abs(adjectives[,2][i]/5) # absolute value since we do not care about direction
  }
  rowSums(out[-1])
}

#' Rank urgent topics
#'
#' @param v Text vector or annotated data frame.
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
  if (any(class(v) != "urgency")) {
    v <- get_urgency(v, subjects)
  }
  v |> dplyr::mutate(topic = ifelse(topic == "", "No topic", topic)) |>
    tidyr::separate_rows(topic, sep = ", ") |>
    dplyr::group_by(topic) |>
    dplyr::summarise(urgency_sum = sum(urgency),
                     urgency_mean = mean(urgency),
                     urgency_median = stats::median(urgency)) |>
    dplyr::arrange(-urgency_sum)
}
