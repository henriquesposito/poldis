#' Extract dates from text
#'
#' Sometimes dates can be contained in text,
#' this function extracts those dates from text.
#' @param v Text variable/object
#' @return A list of dates
#' @import stringr
#' @examples
#' text <- c("This function was created on the 29 September 2021",
#' "Today is October 12, 2021")
#' extract_date(text)
#' @export
extract_date <- function(v) {
  # make all lower case
  out <- tolower(v)
  # remove commas
  out <- gsub(",", "", out)
  # remove ordinal signs and date related articles
  out <- stringr::str_remove_all(out, "de |of |st |nd |rd |th ")
  # correct double white space left
  out <- stringr::str_squish(out)
  # get the first date per row
  out <- stringr::str_extract(out, "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|[:alpha:]{3}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{4}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{5}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{6}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{7}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{8}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{9}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{3}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{4}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{5}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{6}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{7}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{8}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{9}\\s[:digit:]{1}\\s[:digit:]{4}|[:digit:]{2}-[:digit:]{2}-[:digit:]{4}|[:digit:]{1}-[:digit:]{2}-[:digit:]{4}|[:digit:]{2}-[:digit:]{2}-[:digit:]{2}|[:digit:]{1}-[:digit:]{2}-[:digit:]{2}|[:digit:]{2}-[:digit:]{1}-[:digit:]{4}|[:digit:]{1}-[:digit:]{1}-[:digit:]{4}|[:digit:]{2}-[:digit:]{1}-[:digit:]{2}|[:digit:]{1}-[:digit:]{1}-[:digit:]{2}|[:digit:]{4}-[:digit:]{2}-[:digit:]{2}|[:digit:]{4}-[:digit:]{2}-[:digit:]{1}|[:digit:]{4}-[:digit:]{1}-[:digit:]{2}|[:digit:]{4}-[:digit:]{1}-[:digit:]{1}|[:digit:]{2}/[:digit:]{2}/[:digit:]{4}|[:digit:]{1}/[:digit:]{2}/[:digit:]{4}|[:digit:]{2}/[:digit:]{2}/[:digit:]{2}|[:digit:]{1}/[:digit:]{2}/[:digit:]{2}|[:digit:]{2}/[:digit:]{1}/[:digit:]{4}|[:digit:]{1}/[:digit:]{1}/[:digit:]{4}|[:digit:]{2}/[:digit:]{1}/[:digit:]{2}|[:digit:]{1}/[:digit:]{1}/[:digit:]{2}|[:digit:]{4}/[:digit:]{2}/[:digit:]{2}|[:digit:]{4}/[:digit:]{2}/[:digit:]{1}|[:digit:]{4}/[:digit:]{1}/[:digit:]{2}|[:digit:]{4}/[:digit:]{1}/[:digit:]{1}|[:digit:]{3}\\s[:alpha:]{4}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{2}|[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{2}|[:digit:]{3}\\s[:alpha:]{4}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{4}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{5}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{6}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{7}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{8}\\s[:digit:]{1}|[:digit:]{4}\\s[:alpha:]{9}\\s[:digit:]{1}")
  # re-order dates if necessary
  out <- lapply(out, re_order)
  # get the months into numeric form
  months <- as.data.frame(months)
  for (k in seq_len(nrow(months))) {
    out <- gsub(paste0(months$months[k]),
                paste0(months$number[k]),
                out, ignore.case = TRUE,
                perl = T)
  }
  # standardize separators
  out <- stringr::str_replace_all(out, " |/", "-")
  out <- as.character(ifelse(out == "", NA, out))
  out
}

#' Helper function for re-ordering dates
#'
#' Helper function for re-ordering character dates for consistency.
#' Default is DMY.
#' @param l List of dates
#' @import stringr
#' @return A list of dates of the same lenghte but re-ordered, if necessary.
re_order <- function(l) {
  l <- stringr::str_squish(l)
  st <- stringr::word(l, 1)
  mi <- stringr::word(l, 2)
  ed <- stringr::word(l, 3)
  out <- ifelse(stringr::str_starts(l, "[:digit:]{4}"), paste0(ed, "-", mi, "-", st), l)
  out <- ifelse(stringr::str_starts(out, "[:alpha:]"), paste0(mi, "-", st, "-", ed), out)
  out <- stringr::str_remove_all(out, "-NA|NA-|NA")
  out
}

#' Extract title from text
#'
#' A lot of information is contained in the first sentence of a text.
#' For political texts, dates and locations are often contained in the
#' first sentence.
#' This function extracts the first sentence of text.
#' @param v Text variable/object
#' @return A list of the first sentences
#' @examples
#' text <- "This is the first sentence. This is the second sentence."
#' extract_title(text)
#' @export
extract_title <- function(v) {
  out <- gsub("([a-z0-9][?!.])\\s.*", "\\1", v)
  out
}
