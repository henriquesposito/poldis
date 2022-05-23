#' Extract dates from text
#'
#' Sometimes dates can be contained in text,
#' this function extracts those dates from text.
#' Please also see `messydates::as_messydate()` for more on dates extraction
#' from texts.
#' @param v Text vector
#' @return A list of dates
#' @importFrom  stringr str_squish str_remove_all str_extract str_replace_all
#' @importFrom messydates as_messydate
#' @examples
#' extract_date("This function was created on the 29 September 2021")
#' @export
extract_date <- function(v) {
  # make all lower case
  out <- tolower(v)
  # remove commas
  out <- gsub("\\,|\\-|\\.|\\/", " ", out)
  # remove ordinal signs and date related articles
  out <- stringr::str_squish(stringr::str_remove_all(out, "de |of |st |nd |rd |th "))
  # sub months to dates
  months <- as.data.frame(months)
  for (k in seq_len(nrow(months))) {
    out <- gsub(paste0(months$months[k]),
                paste0(months$number[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # get the first date per row
  out <- stringr::str_extract(out, "[:digit:]{2}\\s[:digit:]{2}\\s[:digit:]{4}|
  |[:digit:]{1}\\s[:digit:]{2}\\s[:digit:]{4}|
  |[:digit:]{2}\\s[:digit:]{2}\\s[:digit:]{2}|
  |[:digit:]{1}\\s[:digit:]{2}\\s[:digit:]{2}|
  |[:digit:]{2}\\s[:digit:]{1}\\s[:digit:]{4}|
  |[:digit:]{1}\\s[:digit:]{1}\\s[:digit:]{4}|
  |[:digit:]{2}\\s[:digit:]{1}\\s[:digit:]{2}|
  |[:digit:]{1}\\s[:digit:]{1}\\s[:digit:]{2}|
  |[:digit:]{4}\\s[:digit:]{2}\\s[:digit:]{2}|
  |[:digit:]{4}\\s[:digit:]{2}\\s[:digit:]{1}|
  |[:digit:]{4}\\s[:digit:]{1}\\s[:digit:]{2}|
                              |[:digit:]{4}\\s[:digit:]{1}\\s[:digit:]{1}")
  # standardize separators
  out <- stringr::str_replace_all(out, " ", "-")
  out <- as.character(ifelse(out == "", NA_character_, messydates::as_messydate(out)))
  out
}

#' Extract first sentence from text
#'
#' A lot of information is contained in the first sentence of a text.
#' In political texts, for example, dates and locations are often contained
#' in the first sentence of the text.
#' @param v Text vector
#' @return A list of the first sentences
#' @examples
#' extract_title("This is the first sentence. This is the second sentence.")
#' @export
extract_title <- function(v) {
  out <- gsub("([a-z0-9][?!.])\\s.*", "\\1", v)
  out
}

#' Extract location from strings
#'
#' Extracts location from strings.
#' Works for Brasilian states and other countries.
#' Texts must be in english or portuguese.
#' @param v Text variable/object
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr
#' @return A list of the first locations
#' @details If more than one location is found,
#' returns only the first match.
#' @examples
#' text <- c("This is the United States", "This is Sao Paulo",
#' "I was in Rio de Janeiro and Sao Paulo, then back to the United States")
#' extract_location(text)
#' @export
extract_location <- function(v) {
  v <- stringi::stri_trans_general(v, id = "Latin-ASCII")
  for (k in seq_len(nrow(location))) {
    v <- gsub(paste0(location$regex[k]),
              paste0(location$location[k]),
              v, ignore.case = TRUE,
              perl = T)
  }
  v <- stringr::str_extract(v, "\\.\\.\\.[^()]+\\.\\.\\.")
  v <- paste0(v, "...", NA_character_)
  v <- strsplit(v, "\\.\\.\\.")
  v <- purrr::map_chr(v, c(2))
  v
}

#' Split Texts
#'
#' Split texts into structured lists according to a split sign.
#' @param text text variable
#' @param splitsign Where do you want to split?
#' By default sentences (".").
#' This can also be words, signals or other markers you want.
#' For special characters, please use escape sign before (i.e. "\\").
#' @return A splitted list for each row
#' @examples
#' text <- "This is the first sentence. This is the second sentence."
#' split_text(text)
#' @export
split_text <- function(text, splitsign = "\\.") {
  t <-  strsplit(as.character(text), splitsign)
  # Add attribute for the number of divisions
  for(i in seq_len(length(t))) {
    attr(t[[i]], "Parts") <- paste0("Number of parts = ", lengths(t[i]))
  }
  t
}

#' Get text matches
#'
#' Get texts in which only certain "matches" occur.
#' @param text A text variable
#' @param match A regex match for a word(s) or expression.
#' For multiple words, please use "|" to divide them.
#' @param invert Do you want texts without certain matches to be returned?
#' By default FALSE.
#' @param ignore.case Should case be ignored?
#' By default, TRUE.
#' @importFrom purrr map_chr
#' @return A list of matches of the same length as text variable
#' @examples
#' text <- c("This function was created on the 29 September 2021",
#' "Today is October 12, 2021")
#' text_match(text, "October")
#' @export
text_match <- function(text, match, invert = FALSE, ignore.case = TRUE) {
  if (invert == TRUE & ignore.case == FALSE) {
    t <- lapply(text, function(x) grep(match, x, value = TRUE, ignore.case = FALSE, invert = TRUE))
  } else if (invert == TRUE & ignore.case == TRUE) {
    t <- lapply(text, function(x) grep(match, x, value = TRUE, ignore.case = TRUE, invert = TRUE))
  } else if (invert == FALSE & ignore.case == FALSE) {
    t <- lapply(text, function(x) grep(match, x, value = TRUE, ignore.case = FALSE))
  } else {
    t <- lapply(text, function(x) grep(match, x, value = TRUE, ignore.case = TRUE))
  }
  t
}
