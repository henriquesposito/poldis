#' Alternative urgency function
#'
#' @param .data A data frame, priorities data frame coded using
#' `select_priorities()`, or text vector.
#' For data frames, function will search for "text" variable.
#' For priorities data frame function will search for "priorities" variable.
#' If missing, opens the webpage containing the urgency codebook.
#' @examples
#' get_urgency2(US_News_Conferences_1960_1980[1:10, 3])
#' @export
get_urgency2 <- function(.data) {
  Frequency <- Timing <- Commitment <- Intensity <- Urgency <- text_clean <- NULL
  # tries to open urgency codebook if no argument is declared
  if (missing(.data)) open_codebook(codebook = "urgency")
  # get text variable
  if (inherits(.data, "priorities")) {
    text_clean <- getElement(.data, "priorities")
  } else if (inherits(.data, "data.frame")) {
    text_clean <- getElement(.data, "text")
  } else text_clean <- .data
  # assign urgency dimensions
  out <- data.frame("text_clean" = .clean_token(text_clean)) %>%
    dplyr::mutate(Frequency = .assign_urgency_dimension(text_clean, dimension = "frequency"),
                  Timing = .assign_urgency_dimension(text_clean, dimension = "timing"),
                  Intensity = .assign_urgency_dimension(text_clean, dimension = "intensity"),
                  Commitment = .assign_urgency_dimension(text_clean, dimension = "commitment"),
                  Urgency = (((1 + Commitment) + (1 + Intensity))/4) * (((1 + Timing) + (1 + Frequency))/4))
  out <- cbind(.data, out) %>% dplyr::select(-c(text_clean))
  class(out) <- c("urgency", class(out))
  out
}

.assign_urgency_dimension <- function(v, dimension) {
  Rescaled <- word <- sums <- NULL
  if (dimension == "frequency") {
    out <- frequency_0_1
  } else if (dimension == "timing") {
    out <- timing_0_1
  } else if (dimension == "commitment") {
    out <- commitment_0_1
  } else if (dimension == "intensity") {
    out <- intensity_0_1
  }
  out <- select(out, word, Rescaled) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(word = textstem::lemmatize_words(word)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Rescaled) %>%
    summarise(terms = paste0(word, collapse = "|"))
  rowcount <- rowSums(do.call("cbind", lapply(seq_len(nrow(out)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", out$terms[i], "\\b"))))) # get count
  out <- rowSums(do.call("cbind", lapply(seq_len(nrow(out)), function(i)
    stringr::str_count(as.character(v), paste0("\\b", out$terms[i], "\\b"))*
      out$Rescaled[i]))) # get scores
  out/(replace(rowcount, rowcount == 0, 1)) # get mean value
}
