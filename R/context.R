#' Context to string matches
#'
#' A function for getting string matches and the context in which they occur.
#' @param match Character string to be matched.
#' For multiple strings, please use "|" as a separator.
#' @param textvar Text variable.
#' @param level At which text level do you want matches to be returned?
#' Options are sentences, words, and paragraph.
#' @param n Number of sentences or words matched before and after string match.
#' For paragraphs, n is always set to one.
#' 1 by default.
#' That is, one word or one sentence before, and after, string match.
#' @importFrom stringr str_detect str_extract_all
#' @examples
#' context(match = "war|weapons of mass destruction|conflict|NATO|peace",
#' textvar = US_News_Conferences_1960_1980$text[100], level = "sentences")
#' @return A list of string matches an their context
#' @export
context <- function(match, textvar,
                    level = c("sentences", "words", "paragraph"), n = 1) {
  if (is.null(level)) {
    stop("Please declare the level of the text to be returned, option are sentences, words or paragraph")
  }
  if (level == "sentences") {
    word <- paste0("([^.]+\\.){0,", "n", "}[^.]+(", match, ").*?\\.([^.]+\\.){0,1}")
    s <- stringr::str_extract_all(textvar, word)
  }
  if (level == "words") {
    sentence <- paste0("(\\w+){", n, "}", match, "(\\w+){", "n", "}")
    s <- stringr::str_extract_all(textvar, sentence)
  }
  if (level == "paragraph") {
    if (stringr::str_detect(textvar, "\\.\n", negate = TRUE))
    {
      stop("No paragraph markings were found in text variable, please set level to sentences or words")
    }
    paragraph <- strsplit(textvar, "\\.\n")
    s <- ifelse(stringr::str_detect(match, paragraph), paragraph, "")
  }
  s
}
