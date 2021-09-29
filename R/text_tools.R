#' Extract dates from text
#'
#' Sometimes dates can be contained in text,
#' this function extracts those dates from text.
#' @param v Text variable/object
#' @param language English is default.
#' For portuguese, specify "pt".
#' @return A list of dates
#' @import stringr
#' @example
#' text <- c("This function was created on the 29 September 2021",
#' "Today is October 12, 2021")
#' extract_date(text)
#' @export
extract_date <- function(v, language = "en") {
  out <- tolower(v)
  out <- gsub(",", "", out)
  out <- stringr::str_remove_all(out, "de |of |st |nd |rd |th ")
  out <- gsub("  ", " ", out)
  if (language == "pt") {
    # portuguese
    out <- stringr::str_extract_all(out, "[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}|[:digit:]{2}-[:digit:]{2}-[:digit:]{4}|[:digit:]{1}-[:digit:]{2}-[:digit:]{4}|[:digit:]{2}-[:digit:]{2}-[:digit:]{2}|[:digit:]{1}-[:digit:]{2}-[:digit:]{2}|[:digit:]{2}-[:digit:]{1}-[:digit:]{4}|[:digit:]{1}-[:digit:]{1}-[:digit:]{4}|[:digit:]{2}-[:digit:]{1}-[:digit:]{2}|[:digit:]{1}-[:digit:]{1}-[:digit:]{2}|[:digit:]{4}-[:digit:]{2}-[:digit:]{2}|[:digit:]{4}-[:digit:]{2}-[:digit:]{1}|[:digit:]{4}-[:digit:]{1}-[:digit:]{2}|[:digit:]{4}-[:digit:]{1}-[:digit:]{1}|[:digit:]{2}/[:digit:]{2}/[:digit:]{4}|[:digit:]{1}/[:digit:]{2}/[:digit:]{4}|[:digit:]{2}/[:digit:]{2}/[:digit:]{2}|[:digit:]{1}/[:digit:]{2}/[:digit:]{2}|[:digit:]{2}/[:digit:]{1}/[:digit:]{4}|[:digit:]{1}/[:digit:]{1}/[:digit:]{4}|[:digit:]{2}/[:digit:]{1}/[:digit:]{2}|[:digit:]{1}/[:digit:]{1}/[:digit:]{2}|[:digit:]{4}/[:digit:]{2}/[:digit:]{2}|[:digit:]{4}/[:digit:]{2}/[:digit:]{1}|[:digit:]{4}/[:digit:]{1}/[:digit:]{2}|[:digit:]{4}/[:digit:]{1}/[:digit:]{1}")
  }
  if (language == "en") {
    out <- stringr::str_extract_all(out, "[:digit:]{2}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{4}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{6}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{8}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{3}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{4}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{5}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{6}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{7}\\s[:digit:]{4}|[:digit:]{1}\\s[:alpha:]{8}\\s[:digit:]{4}|[:digit:]{2}\\s[:alpha:]{9}\\s[:digit:]{4}|[:alpha:]{3}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{4}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{5}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{6}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{7}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{8}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{9}\\s[:digit:]{2}\\s[:digit:]{4}|[:alpha:]{3}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{4}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{5}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{6}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{7}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{8}\\s[:digit:]{1}\\s[:digit:]{4}|[:alpha:]{9}\\s[:digit:]{1}\\s[:digit:]{4}|[:digit:]{2}-[:digit:]{2}-[:digit:]{4}|[:digit:]{1}-[:digit:]{2}-[:digit:]{4}|[:digit:]{2}-[:digit:]{2}-[:digit:]{2}|[:digit:]{1}-[:digit:]{2}-[:digit:]{2}|[:digit:]{2}-[:digit:]{1}-[:digit:]{4}|[:digit:]{1}-[:digit:]{1}-[:digit:]{4}|[:digit:]{2}-[:digit:]{1}-[:digit:]{2}|[:digit:]{1}-[:digit:]{1}-[:digit:]{2}|[:digit:]{4}-[:digit:]{2}-[:digit:]{2}|[:digit:]{4}-[:digit:]{2}-[:digit:]{1}|[:digit:]{4}-[:digit:]{1}-[:digit:]{2}|[:digit:]{4}-[:digit:]{1}-[:digit:]{1}|[:digit:]{2}/[:digit:]{2}/[:digit:]{4}|[:digit:]{1}/[:digit:]{2}/[:digit:]{4}|[:digit:]{2}/[:digit:]{2}/[:digit:]{2}|[:digit:]{1}/[:digit:]{2}/[:digit:]{2}|[:digit:]{2}/[:digit:]{1}/[:digit:]{4}|[:digit:]{1}/[:digit:]{1}/[:digit:]{4}|[:digit:]{2}/[:digit:]{1}/[:digit:]{2}|[:digit:]{1}/[:digit:]{1}/[:digit:]{2}|[:digit:]{4}/[:digit:]{2}/[:digit:]{2}|[:digit:]{4}/[:digit:]{2}/[:digit:]{1}|[:digit:]{4}/[:digit:]{1}/[:digit:]{2}|[:digit:]{4}/[:digit:]{1}/[:digit:]{1}")
  }

  months <- as.data.frame(months)
  for (k in seq_len(nrow(months))) {
    out <- gsub(paste0(months$months[[k]]),
                paste0(months$number[[k]]),
                out, ignore.case = TRUE,
                perl = T)
  }
  out <- stringr::str_trim(out, "both")
  out <- stringr::str_replace_all(out, " ", "-")
  out <-as.character(out)
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
#' @example
#' text <- "This is the first sentence. This is the second sentence."
#' extract_title(text)
#' @export
extract_title <- function(v) {
  out <- gsub("([a-z0-9][?!.])\\s.*", "\\1", v)
  out
}
