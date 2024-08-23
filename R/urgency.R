#' Urgency Analysis
#'
#' @param .data A data frame, priorities data frame coded using
#' `select_priorities()`, or text vector.
#' For data frames, function will search for "text" variable.
#' For priorities data frame function will search for "priorities" variable.
#' If missing, opens the webpage containing the urgency codebook.
#' @param summarise How to handle multiple matches for the same dimension
#' in the same text observation?
#' By default, multiple matches are added together and
#' their "sum" per text observation is returned.
#' Users can, instead, choose the "mean" which returns the average
#' score per dimension per text observation when there are multiple matches.
#' The "mean" can also be used as a form of normalization per
#' dimension and text observation in certain cases.
#' @details
#' Urgency in political discourses is an expression of how necessary and/or
#' how soon an action should be undertaken or completed.
#' This is measured along four dimensions,
#' two related to necessity and two related to timing.
#' The first two dimensions, degree of intensity and degree of commitment,
#' relate to the necessity of taking the action, while the next two dimensions,
#' frequency of action and timing of action,
#' relate to the timing in which action is taken.
#' Our dictionary includes terms in each of these dimensions.
#' The terms included in each of these dimensions were validated and adjusted
#' through an online survey that took place between July and August of 2024.
#' The survey results were recorded as counts of the number of participants
#' who selected an urgency-related word as more urgent than its pair.
#' To analyze the survey results, we employed Bradley-Terry models for
#' paired comparisons.
#' A rank of the words for each dimension of urgency was obtained from the analysis,
#' which were then used to create the urgency word scores in the dictionaries.
#' For more information on the dimensions, scores, or the survey on urgency,
#' please run `get_urgency()` to access the urgency codebook.
#' For priorities (i.e. coded using the `select_priorities()`),
#' urgency scores are calculated by multiplying the commitment scores by all
#' other dimensions.
#' This is done because commitment words are indicative of political priorities,
#' For more information please refer to the `select_priorities()` function.
#' For vectors or data frames urgency scores are calculated by
#' adding commitment and intensity dimension scores (i.e. how necessary)
#' and multiplying these by the sum of timing and frequency dimension
#' scores (i.e. how soon).
#' In both cases, zero urgency scores are indicative of no urgency but maximum
#' scores can vary.
#' @return A scored data frame for each dimension of urgency.
#' @import dplyr
#' @importFrom textstem lemmatize_strings
#' @examples
#' \donttest{
#' get_urgency(US_News_Conferences_1960_1980[1:10, 3])
#' get_urgency(US_News_Conferences_1960_1980[1:10,])
#' #get_urgency(select_priorities(US_News_Conferences_1960_1980[1:2, 3]))
#' #summary(get_urgency(US_News_Conferences_1960_1980[1:10, 3]))
#' #plot(get_urgency(US_News_Conferences_1960_1980[1:10, 3]))
#' #get_urgency()
#' }
#' @export
get_urgency <- function(.data, summarise = "sum") {
  Frequency <- Timing <- Commitment <- Intensity <- Urgency <- text_clean <- NULL
  # tries to open urgency codebook if no argument is declared
  if (missing(.data)) open_codebook(codebook = "urgency")
  # get text variable
  if (inherits(.data, "priorities")) {
    text_clean <- getElement(.data, "lemmas")
    priorities <- TRUE
  } else if (inherits(.data, "data.frame")) {
    if ("lemmas" %in% names(.data)) text_clean <- getElement(.data, "lemmas") else
      text_clean <- textstem::lemmatize_strings(getElement(.data, "text"))
    priorities <- FALSE
  } else {
    text_clean <- textstem::lemmatize_strings(.data)
    priorities <- FALSE
  }
  # assign urgency dimensions
  out <- data.frame("text_clean" = text_clean) %>%
    dplyr::mutate(Frequency = .assign_urgency_dimensions(text_clean, dimension = "frequency",
                                                         summarise = summarise, priority = priorities),
                  Timing = .assign_urgency_dimensions(text_clean, dimension = "timing",
                                                      summarise = summarise, priority = priorities),
                  Intensity = .assign_urgency_dimensions(text_clean, dimension = "intensity",
                                                         summarise = summarise, priority = priorities),
                  Commitment = .assign_urgency_dimensions(text_clean, dimension = "commitment",
                                                          summarise = summarise, priority = priorities))
  # calculate urgency scores
  if (isTRUE(priorities)) {
    out <- out %>%
      dplyr::mutate(Urgency = Commitment * ifelse(Intensity == 0, 1, Intensity) *
                      ifelse(Timing == 0, 1, Timing) * ifelse(Frequency == 0, 1, Frequency))
  } else {
    out <- out %>%
      dplyr::mutate(Urgency = (((1 + Commitment) + (1 + Intensity))/4) *
                      (((1 + Timing) + (1 + Frequency))/4))
  }
  # bind data results to original data
  out <- cbind(.data, out) %>% dplyr::select(-c(text_clean))
  class(out) <- c("urgency", class(out))
  out
}

.assign_urgency_dimensions <- function(v, dimension, summarise, priority) {
  scaled <- rescaled <- word <- grammar_function <- NULL
  # get dictionaries
  if (dimension == "frequency") {
    out <- frequency
  } else if (dimension == "timing") {
    out <- timing
  } else if (dimension == "commitment") {
    out <- commitment
  } else if (dimension == "intensity") {
    out <- intensity
  }
  # check if priorities and get correct scale and data
  if (isTRUE(priority)) { # for urgency in priorities
    out <- dplyr::rename(out, scale = rescaled) %>%
      dplyr::mutate(word = ifelse(grammar_function == "verb",
                                  stringr::str_remove_all(word, " to$"),
                                  word)) %>% # to is not necessary because of how we select priorities
      dplyr::select(word, scale)
  } else out <- dplyr::rename(out, scale = scaled) %>% dplyr::select(word, scale)
  # remove missing and group words
  out <- out %>%
    tidyr::drop_na() %>%
    dplyr::mutate(word = textstem::lemmatize_strings(word)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(scale) %>%
    summarise(terms = paste0(word, collapse = "|"))
  # summarise if multiple matches per dimension per text observation
  if (summarise == "sum") {
    out <- rowSums(do.call("cbind", lapply(seq_len(nrow(out)), function(i)
      stringr::str_count(as.character(v), paste0("\\b", out$terms[i], "\\b"))*
        out$scale[i]))) # get added scores
  } else if (summarise == "mean") {
    rowcount <- rowSums(do.call("cbind", lapply(seq_len(nrow(out)), function(i)
      stringr::str_count(as.character(v), paste0("\\b", out$terms[i], "\\b"))))) # get count
    out <- rowSums(do.call("cbind", lapply(seq_len(nrow(out)), function(i)
      stringr::str_count(as.character(v), paste0("\\b", out$terms[i], "\\b"))*
        out$scale[i]))) # get added scores
    out <- out/(replace(rowcount, rowcount == 0, 1)) # get average value
  }
  out
}
