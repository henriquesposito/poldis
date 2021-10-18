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
#' var = US_News_Conferences$text[500], level = "sentences")
#' @return A list of string matches an their context
#' @export
context <- function(string, var, level = c("sentences", "words", "paragraph")) {

  if (is.null(level)) {
    stop("Please declare the level of the text to be returned, option are sentences, words or paragraph")
  }
  if (level == "sentences") {
    # sentence before and after string match
    match <- paste0("([^.]+\\.){1}[^.]+(", string, ").*?\\.([^.]+\\.){1}")
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

# # Banco Central em Discursos Presidenciais
#
# library(dplyr)
# library(stringr)
#
# # count
# banco_central <- stringr::str_count(BR_oral$text, "banco central|Banco Central|^BC$")
# sum(banco_central)
# # sentence level dataframe
# bc <- data.frame(BR_oral[grep("banco central|Banco Central|^BC$", BR_oral$text, ignore.case = TRUE),])
# bc <- bc %>% select(date, presid, party, length, text)
# bc[50,]
# bce <- bc
# # 3 sentences
# text <- gsub(".*?(([^.]+\\.){1}[^.]+(banco central|Banco Central|banco Central|Banco central).*?\\.(.*?\\.){1}).*",
#              "\\1", bce$text, ignore.case = TRUE)
# summary(as.factor(bce$presid))
# bce$text
#
# bca <-head(bc)
# t <- stringr::str_extract_all(bca$text, ".*?(([^.]+\\.){1}[^.]+(banco central|Banco Central|banco Central|Banco central).*?\\.(.*?\\.){1}).*")
#
# # Amazonia
#
# library(dplyr)
# library(stringr)
#
# # count
# am <- stringr::str_count(BR_oral$text, "Amazon|desmatamento|climaticas")
# sum(am)
# # sentence level dataframe
# am <- data.frame(BR_oral[grep("Amazon|desmatamento|climaticas", BR_oral$text, ignore.case = TRUE),])
# am <- am %>% select(date, presid, party, length, text)
# am[50,]
# ame <- am
# # 3 sentences
# text <- gsub(".*?(([^.]+\\.){1}[^.]+(Amazon|desmatamento|climaticas).*?\\.(.*?\\.){1}).*",
#              "\\1", ame$text, ignore.case = TRUE)
# summary(as.factor(ame$presid))
# ame[50,]
#
# ama <-head(am)
# t <- stringr::str_extract_all(ama$text, ".*?(([^.]+\\.){1}[^.]+(Amazon|desmatamento|climaticas).*?\\.(.*?\\.){1}).*")
