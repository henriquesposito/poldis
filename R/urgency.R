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
    dplyr::mutate(Frequency = .assign_frequencies(text_clean),
                  Timing = .assign_timing(text_clean),
                  Intensity = .assign_intensity(text_clean),
                  Commitment = .assign_commitment(text_clean))
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
  frequency <- score_frequency <- NULL
  freq_words <- urgency_word_scores[,1:2] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(frequency = textstem::lemmatize_words(frequency)) %>%
    dplyr::distinct() %>% # 61 terms
    dplyr::group_by(score_frequency) %>%
    summarise(terms = paste0(frequency, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(freq_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", freq_words$terms[i], "\\b"))*
      freq_words$score_frequency[i])))
}

.assign_timing <- function(v) {
  timing <- score_timing <- NULL
  timing_words <- urgency_word_scores[,3:4] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(timing = textstem::lemmatize_words(timing)) %>%
    dplyr::distinct() %>% # 41 terms
    dplyr::group_by(score_timing) %>%
    summarise(terms = paste0(timing, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(timing_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", timing_words$terms[i], "\\b"))*
      timing_words$score_timing[i])))
}

.assign_intensity <- function(v) {
  intensity <- score_intensity <- NULL
  intensity_words <- urgency_word_scores[,5:6] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(intensity = textstem::lemmatize_words(intensity)) %>%
    dplyr::distinct() %>% # 103 terms
    dplyr::group_by(score_intensity) %>%
    summarise(terms = paste0(intensity, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(intensity_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", intensity_words$terms[i], "\\b"))*
      intensity_words$score_intensity[i])))
}

.assign_commitment <- function(v) {
  commitment <- score_commitment <- NULL
  commitment_words <- urgency_word_scores[,7:8] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(commitment = textstem::lemmatize_words(commitment)) %>%
    dplyr::distinct() %>% # 78 terms
    dplyr::group_by(score_commitment) %>%
    summarise(terms = paste0(commitment, collapse = "|"))
  out <- list()
  rowSums(do.call("cbind", lapply(seq_len(nrow(commitment_words)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", commitment_words$terms[i], "\\b"))*
      commitment_words$score_commitment[i])))
}
