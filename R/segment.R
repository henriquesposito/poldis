# Functions to help with text segmentation

#' Extract future promises from political discourses
#'
#' @param v Text vector.
#' @importFrom dplyr filter
#' @export
extract_promises <- function(v) {
  tags <- tokens <- NULL
  if (any(class(v) == "spacyr_parsed")) {
    if ("token_id" %in% names(object))
      stop("Please declare a text vector or a an annotated object at the sentence level.")
  } else v <- annotate_text(v, level = "sentences")
  v |> dplyr::filter(grepl(" MD ", tags) | grepl("going to", sentence))
  # todo: extract related sentences around promises and re-paste together
}

#' Extract most frequent subjects from political discourses
#'
#' @param v Text vector.
#' @param n Number of subjects
#' @import dplyr
#' @export
extract_subjects <- function(v, n = 20) {
  if (any(class(v) == "spacyr_parsed")) {
    if ("ntoken" %in% names(object))
      stop("Please declare a text vector or a an annotated object at the word level.")
  } else v <- annotate_text(v)
  out <- spacyr::entity_extract(v) |>
    group_by(doc_id, sentence_id) |>
    mutate(duplicated = n() > 1,
           entity = stringr::str_squish(
             tm::removePunctuation(tm::removeWords(
             stringr::str_replace_all(tolower(entity), "_", " "),
             tm::stopwords())))) |>
    filter(duplicated == FALSE) |>
    ungroup()
  purrr::map_dfr(out$entity, ~ {
    i <- which(stringdist::stringdist(., out$entity, "lv") < 2)
    tibble(index = i, subjects = out$entity[i])
  }, .id = "group") |>
    distinct(index, .keep_all = T) |>
    group_by(group) |>
    summarize(subjects = paste(unique(subjects), collapse = "|"),
              count = n()) |>
    arrange(-count) |>
    ungroup() |>
    filter(subjects != "") |>
    select(subjects) |>
    slice_head(n = n)
  # todo: exclude small words
}

# assign_subjects <- function(v, subjects) {
#   if (any(class(v) == "spacyr_parsed")) {
#     if ("token_id" %in% names(object))
#       stop("Please declare a text vector or a an annotated object at the sentence level.")
#   } else v <- annotate_text(v, level = "sentences")
#   # get subjects
#   # get similar words
#   # match in sentences
# }

#' Extract context for string matches
#'
#' A function for getting string matches and the context in which they occur.
#' @param match Character string to be matched.
#' For multiple strings, please use "|" as a separator.
#' @param v Text vector.
#' @param level At which text level do you want matches to be returned?
#' Options are sentences, words, and paragraph.
#' @param n Number of sentences or words matched before and after string match.
#' 1 by default.
#' That is, one word or one sentence before, and after, string match.
#' For paragraphs, n is always set to one.
#' @importFrom stringr str_detect str_extract_all
#' @examples
#' extract_context(match = "war|weapons of mass destruction|conflict|NATO|peace",
#' v = US_News_Conferences_1960_1980$text[100],
#' level = "sentences",
#' n = 2)
#' @return A list of string matches an their context
#' @export
extract_context <- function(match, v, level = c("sentences", "words", "paragraph"), n = 1) {
  if (is.null(level)) {
    stop("Please declare the level of the text to be returned, option are sentences, words or paragraph")
  }
  if (level == "sentences") {
    s <- stringr::str_extract_all(v, paste0("([^.]+\\.){0,", n, "}[^.]+(", match, ").*?\\.([^.]+\\.){0,", n, "}"))
  } else if (level == "words") {
    s <- stringr::str_extract_all(v, paste0("([^\\s]+\\s+){", n,"}", match, "(\\s+[^\\s]+){", n, "}"))
  } else if (level == "paragraph") {
    if (stringr::str_detect(v, "\\.\n", negate = TRUE))
    {
      stop("No paragraph markings were found in text variable, please set level to sentences or words")
    }
    paragraph <- strsplit(v, "\\.\n")
    s <- ifelse(stringr::str_detect(match, paragraph), paragraph, "")
  }
  s
}

#' Split texts
#'
#' Split texts into structured lists of lists according to a split sign.
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

#' Extract text matches
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
#' extract_match(text, "October")
#' @export
extract_match <- function(text, match, invert = FALSE,
                          ignore.case = TRUE) {
  if (invert == TRUE & ignore.case == FALSE) {
    t <- lapply(text, function(x) grep(match, x, value = TRUE,
                                       ignore.case = FALSE, invert = TRUE))
  } else if (invert == TRUE & ignore.case == TRUE) {
    t <- lapply(text, function(x) grep(match, x, value = TRUE,
                                       ignore.case = TRUE, invert = TRUE))
  } else if (invert == FALSE & ignore.case == FALSE) {
    t <- lapply(text, function(x) grep(match, x, value = TRUE,
                                       ignore.case = FALSE))
  } else {
    t <- lapply(text, function(x) grep(match, x, value = TRUE,
                                       ignore.case = TRUE))
  }
  t
}

#' Extract similarities and differences in texts/segments
#'
#' @param v Text vector
#' @param comparison Would you like to extract similarities or differences
#' between treaties?
#' If not specified, defaults to "similarities".
#' Alternatively, users can also get the "differences".
#' @param method A method for checking similarities or differences.
#' If chosen comparison are similarities, extracts "correlation" between
#' treaty texts if not specified.
#' Other similarity methods from `quanteda.textstats::textstat_simil()`
#' include "cosine", "jaccard", "ejaccard", "dice", "edice",
#' "simple matching", and "hamann".
#' If chosen comparison are differences, extracts "euclidean" difference
#' between treaty texts if not specified.
#' Other difference methods from `quanteda.textstats::textstat_dist()` include
#' "manhattan", "maximum", "canberra", and "minkowski".
#' @export
extract_similarities <- function(v, comparison, method) {
  if (missing(comparison) | comparison == "similarities") {
    if(missing(method)) method = "correlation"
    quanteda.textstats::textstat_simil(quanteda::dfm(v), method = method)
  } else {
    if(missing(method)) method = "euclidean"
    quanteda.textstats::textstat_dist(quanteda::dfm(v), method = method)
  } #todo: add plotting method that plots texts similarities as a dendogram
}
