#' Urgency Analysis
#'
#' @param .data A data frame, promises data frame coded using
#' `select_promises()`, or text vector.
#' For data frames, function will search for "text" variable.
#' For promises data frame function will search for "promises" variable.
#' @param normalize Would you like urgency scores to be normalized?
#' By default, urgency scores are normalized by "tokens",
#' the number of words in text observation.
#' Users can also declare "none", for no normalization.
#' @return A scored data frame for each dimension of urgency.
#' @import dplyr
#' @examples
#' \donttest{
#' get_urgency(US_News_Conferences_1960_1980[1:10,3])
#' }
#' @export
get_urgency <- function(.data, normalize = "tokens") {
  frequency <- timing <- commitment <- intensity <- urgency <- text_clean <- NULL
  # get text variable
  if (inherits(.data, "promises")) {
    text <- stats::na.omit(getElement(.data, "promises"))
  } else if (inherits(.data, "data.frame")) {
    text <- getElement(.data, "text")
  } else text <- .data
  # assign urgency dimensions
  out <- data.frame("text" = text, "text_clean" = .clean_token(text)) %>%
    dplyr::mutate(frequency = .assign_frequencies(text_clean)/60,
                  timing = .assign_timing(text_clean)/41,
                  intensity = .assign_intensity(text_clean)/104,
                  commitment = .assign_commitment(text_clean)/83)
  if (normalize == "tokens") {
    out <- out %>%
      dplyr::mutate(urgency = (frequency + timing + intensity + commitment)/nchar(text_clean)) %>%
      dplyr::arrange(-urgency)
  } else if (normalize == "none") {
    out <- out %>%
      dplyr::mutate(urgency = (frequency + timing + intensity + commitment)) %>%
      dplyr::arrange(-urgency)
  }
  class(out) <- c("urgency", class(out))
  dplyr::select(out, -text_clean)
}

.assign_frequencies <- function(v) {
  frequency <- score_frequency <- NULL
  freq_words <- urgency_word_scores[,1:2] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(frequency = textstem::lemmatize_words(frequency)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_frequency) %>%
    summarise(terms = paste0(frequency, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(freq_words)), function(i)
    stringr::str_count(as.character(v), freq_words$terms[i])* freq_words$score_frequency[i])))
}

.assign_timing <- function(v) {
  timing <- score_timing <- NULL
  timing_words <- urgency_word_scores[,3:4] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(timing = textstem::lemmatize_words(timing)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_timing) %>%
    summarise(terms = paste0(timing, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(timing_words)), function(i)
    stringr::str_count(as.character(v), timing_words$terms[i])*timing_words$score_timing[i])))
}

.assign_intensity <- function(v) {
  intensity <- score_intensity <- NULL
  intensity_words <- urgency_word_scores[,5:6] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(intensity = textstem::lemmatize_words(intensity)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_intensity) %>%
    summarise(terms = paste0(intensity, collapse = "|"))
  rowSums(do.call("cbind", lapply(seq_len(nrow(intensity_words)), function(i)
    stringr::str_count(as.character(v), intensity_words$terms[i])*
      intensity_words$score_intensity[i])))
}

.assign_commitment <- function(v) {
  commitment <- score_commitment <- NULL
  commitment_words <- urgency_word_scores[,7:8] %>%
    tidyr::drop_na() %>%
    dplyr::mutate(commitment = textstem::lemmatize_words(commitment)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(score_commitment) %>%
    summarise(terms = paste0(commitment, collapse = "|"))
  out <- list()
  rowSums(do.call("cbind", lapply(seq_len(nrow(commitment_words)), function(i)
    stringr::str_count(as.character(v), commitment_words$terms[i])*
      commitment_words$score_commitment[i])))
}
