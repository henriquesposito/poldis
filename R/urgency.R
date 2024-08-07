#' Urgency Analysis
#'
#' @param .data A data frame, priorities data frame coded using
#' `select_priorities()`, or text vector.
#' For data frames, function will search for "text" variable.
#' For priorities data frame function will search for "priorities" variable.
#' @param normalize Would you like urgency scores to be normalized?
#' By default, urgency scores are normalized by "tokens",
#' the number of words in text observation.
#' Users can also declare "none", for no normalization.
#' Since dictionaries for each dimension of urgency have a slightly different
#' number of words, scores for in each dimension are normalized
#' by the number of words in dictionary by default.
#' @details
#' Urgency words and scores were generated and validated with an
#' online survey with 206 participants.
#' The survey collected responses anonymously but included basic demographic
#' information about participants, as English proficiency and education levels.
#' The survey results were recorded as counts of the number of participants
#' who said a certain randomly selected urgency related word was more urgent
#' than another randomly selected urgency related word.
#' To analyze the survey results, we employed Bradley-Terry models for
#' paired comparisons using the BradleyTerry2 R package (Turner and Firth, 2012).
#' This allowed to obtain a rank of the words for each dimension of urgency.
#' The rankings were then used to adjust and validate urgency word scores
#' in the dictionaries.
#' @return A scored data frame for each dimension of urgency.
#' @import dplyr
#' @examples
#' \donttest{
#' get_urgency(US_News_Conferences_1960_1980[1:10, 3])
#' get_urgency(US_News_Conferences_1960_1980[1:10,])
#' }
#' @export
get_urgency <- function(.data, normalize = "tokens") {
  Frequency <- Timing <- Commitment <- Intensity <- Urgency <- text_clean <- NULL
  # get text variable
  if (inherits(.data, "priorities")) {
    text_clean <- getElement(.data, "priorities")
  } else if (inherits(.data, "data.frame")) {
    text_clean <- getElement(.data, "text")
  } else text_clean <- .data
  # assign urgency dimensions
  out <- data.frame("text_clean" = .clean_token(text_clean)) %>%
    dplyr::mutate(Frequency = .assign_frequencies(text_clean)/61, #61 terms
                  Timing = .assign_timing(text_clean)/41, #41 terms
                  Intensity = .assign_intensity(text_clean)/98, #98 terms
                  Commitment = .assign_commitment(text_clean)/85) #85 terms
  if (normalize == "tokens") {
    out <- out %>%
      dplyr::mutate(Urgency = (Frequency + Timing + Intensity + Commitment)/
                      nchar(text_clean))
  } else if (normalize == "none") {
    out <- out %>%
      dplyr::mutate(Urgency = (Frequency + Timing + Intensity + Commitment))
  }
  out <- cbind(.data, out) %>% dplyr::select(-c(text_clean))
  class(out) <- c("urgency", class(out))
  out
}

.assign_frequencies <- function(v) {
  frequency <- score_frequency_scaled <- NULL
  freq_words <- urgency_word_scores[,c(5,8)] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(frequency = textstem::lemmatize_words(frequency)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_frequency_scaled) %>%
    summarise(terms = paste0(frequency, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(freq_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", freq_words$terms[i], "\\b"))*
      freq_words$score_frequency_scaled[i])))
}

.assign_timing <- function(v) {
  timing <- score_timing_scaled <- NULL
  timing_words <- urgency_word_scores[,c(1,4)] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(timing = textstem::lemmatize_words(timing)) %>%
    dplyr::distinct() %>% # 41 terms
    dplyr::group_by(score_timing_scaled) %>%
    summarise(terms = paste0(timing, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(timing_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", timing_words$terms[i], "\\b"))*
      timing_words$score_timing_scaled[i])))
}

.assign_intensity <- function(v) {
  intensity <- score_intensity_scaled <- NULL
  intensity_words <- urgency_word_scores[,c(13, 16)] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(intensity = textstem::lemmatize_words(intensity)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_intensity_scaled) %>%
    summarise(terms = paste0(intensity, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(intensity_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", intensity_words$terms[i], "\\b"))*
      intensity_words$score_intensity_scaled[i])))
}

.assign_commitment <- function(v) {
  commitment <- score_commitment_scaled <- NULL
  commitment_words <- urgency_word_scores[,c(9,12)] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(commitment = textstem::lemmatize_words(commitment)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_commitment_scaled) %>%
    summarise(terms = paste0(commitment, collapse = "|"))
  out <- list()
  rowSums(do.call("cbind", lapply(seq_len(nrow(commitment_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", commitment_words$terms[i], "\\b"))*
      commitment_words$score_commitment_scaled[i])))
}
