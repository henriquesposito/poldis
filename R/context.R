#' Context to string matches
#'
#' A function for getting string matches and the context in which they occur.
#' @param string Character string to be matched.
#' For multiple strings, please use "|" as a separator.
#' @param var Text variable
#' @param level At which text level do you want matches to be returned?
#' Options are sentences, words, and paragraph.
#' @importFrom stringr str_detect str_extract_all
#' @details For sentences, the sentence before and the sentence after match are returned.
#' For words, 3 words before and 3 words after match are returned.
#' For paragraph, the whole paragraph of the match is returned.
#' For paragraph to work properly, please make sure paragraph marks are present in text.
#' @examples
#' context(string = "war|weapons of mass destruction|conflict|NATO|peace",
#' var = US_News_Conferences_1960_1980$text[100], level = "sentences")
#' @return A list of string matches an their context
#' @export
context <- function(string, var, level = c("sentences", "words", "paragraph")) {

  if (is.null(level)) {
    stop("Please declare the level of the text to be returned, option are sentences, words or paragraph")
  }
  if (level == "sentences") {
    # sentence before and after string match
    match <- paste0("([^.]+\\.){0,1}[^.]+(", string, ").*?\\.([^.]+\\.){0,1}")
    s <- stringr::str_extract_all(var, match)
  }
  if (level == "words") {
    # three words before and after string match
    match <- paste0("\\w+ \\w+ \\w+ ", string, " \\w+ \\w+ \\w+")
    s <- stringr::str_extract_all(var, match)
  }
  if (level == "paragraph") {
    # whole paragraph match
    if (stringr::str_detect(var, "\\.\n", negate = TRUE))
    {
      stop("No paragraph markings were found in text variable, please set level to sentences or words")
    }
    paragraph <- strsplit(var, "\\.\n")
    s <- ifelse(stringr::str_detect(string, paragraph), paragraph, "")
  }
  s
}
