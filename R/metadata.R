# Metadata

#' Extract a list of the speakers in texts
#'
#' @param v A text vector.
#' @importFrom dplyr distinct filter %>% summarize group_by
#' @importFrom stringdist stringsimmatrix
#' @import spacyr
#' @return A list of speakers.
#' @details The function relies on NLP models and, therefore, results
#' might not be accurate or consistent.
#' @examples
#' \dontrun{
#' extract_speaker(US_News_Conferences_1960_1980[20, 3])
#' }
#' @export
extract_speaker <- function(v) {
  ent_type <- text <- NULL
  spacyr::spacy_extract_entity(v, type = "named") %>%
    dplyr::filter(ent_type == "PERSON") %>%
    dplyr::group_by(text) %>%
    dplyr::summarise(length = sum(length))
  # if (is.null(allSpeakers)) {
  #   message("No speakers were found in text...")
  # } else {
  #   # check if similar names are the same person
  #   s <- stringdist::stringsimmatrix(parse$text, parse$text)
  #   s <- ifelse(s == 1, 0, s)
  #   rownames(s) <- parse$text
  #   colnames(s) <- parse$text
  #   s <- ifelse(s > 0.8, rownames(s), 0)
  #   s <- data.frame(match1 = colnames(s)[row(s)],
  #                   match2 = as.character(c(t(s))),
  #                   stringsAsFactors = FALSE)
  #   s <- dplyr::distinct(s)
  #   s <- ifelse(s$match2 == 0, s$match1, paste(s$match1, " - ", s$match2))
  #   s
  # }
  # to do: setup plotting method (as a network)
  spacyr::spacy_finalize()
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

#' Extract dates from text
#'
#' Wrapper function for `messydates::as_messydates`.
#' @param v Text vector
#' @return A vector of the dates in text
#' @examples
#' extract_date("Today is the twenty six of February of two thousand and twenty four")
#' @export
extract_date <- function(v) {
  messydates::as_messydate(v)
}

#' Extract location from strings
#'
#' Extracts location from strings.
#' Works for Brazilian states and other countries.
#' Texts must be in English or Portuguese.
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
  # todo: find a better list of countries/cities/locations in the world
  # todo: use NLP to identify location entity
}
