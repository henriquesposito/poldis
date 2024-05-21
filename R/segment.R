#' Extract future promises from political discourses
#'
#' @param v Text vector or annotated data frame.
#' @import dplyr
#' @importFrom stringr str_detect str_remove_all
#' @examples
#' \donttest{
#' extract_promises(US_News_Conferences_1960_1980[1:2,3])
#' }
#' @export
extract_promises <- function(v) {
  tags <- sentence <- lemmas <- sentence_id <- doc_id <- promises <- NULL
  if (any(class(v) == "data.frame")) {
    if ("token_id" %in% names(v))
      stop("Please declare a text vector or an annotated data frame at the sentence level.")
  } else v <- suppressMessages(annotate_text(v, level = "sentences"))
  v <- v |>
    dplyr::mutate(lemmas = tolower(lemmas),
                  promises = ifelse(stringr::str_detect(tags, "PRP MD ")|
                                      stringr::str_detect(lemmas,
                                                          "going to|need to|ready to|
                                     |is time to|commit to|promise to|have to|
                                     |plan to|intend to|let 's|require|want to"),
                                    paste(sentence), NA), # detect promises
                  promises = ifelse(stringr::str_detect(promises, " not |
                                                        |yesterday|last week|
                                                        |last month|last year|
                                                        |minister|president|
                                                        |thank|honor|honour|
                                                        |applause|greet|
                                                        |privilege to|great to|
                                                        |good to") |
                                      stringr::str_detect(tags, "MD VB( RB)? VBN|
                                                         |VBD( RB)? VBN|VBZ( RB)? VBN|
                                                         |VBD( RB)? JJ|PRP( RB)? VBD TO"),
                                    NA, promises)) |>
    dplyr::distinct()
  class(v) <- c("promises", class(v))
  v
}

#' Extract most frequent subjects from political discourses
#'
#' @param v Text vector or annotated data frame.
#' @param n Number of subjects
#' @param method Method to match similar strings.
#' Defaults to "cosine".
#' For other methods available please refer to `stringdist::stringdist()`.
#' @param level Maximum threshold to match strings.
#' Defaults to 0.1.
#' Possible values range from 0 to 1.
#' @import dplyr
#' @examples
#' \donttest{
#' extract_subjects(US_News_Conferences_1960_1980[1:2, 3])
#' }
#' @export
extract_subjects <- function(v, n = 20, method = "cosine", level = 0.1) {
  sentence_id <- doc_id <- entity <- pos <- token <- subject <- strings <- NULL
  if (any(class(v) == "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
  } else v <- suppressMessages(annotate_text(v))
  if ("sentence" %in% names(v) | "segment" %in% names(v)) {
    nouns <- data.frame(strings = .clean_token(unlist(strsplit(v[["nouns"]], " ")))) |>
      dplyr::filter(nchar(strings) > 3) |>
      dplyr::group_by(strings) |>
      dplyr::count() |>
      dplyr::ungroup()
    entity <- data.frame(strings = .clean_token(unlist(strsplit(v[["entities"]], " ")))) |>
      dplyr::filter(nchar(strings) > 3) |>
      dplyr::group_by(strings) |>
      dplyr::count() |>
      dplyr::ungroup()
  } else {
    nouns <- dplyr::filter(v, pos == "NOUN", nchar(token) > 3, entity == "") |>
      dplyr::mutate(strings = .clean_token(token)) |> # should we use the "lemma"?
      dplyr::group_by(strings) |>
      dplyr::count() |>
      dplyr::ungroup()
    entity <- spacyr::entity_extract(v) |>
      dplyr::group_by(doc_id, sentence_id) |>
      dplyr::mutate(duplicated = n() > 1, strings = .clean_token(entity)) |>
      dplyr::filter(duplicated == FALSE, nchar(strings) > 3) |>
      dplyr::group_by(strings) |>
      dplyr::count() |>
      dplyr::ungroup()
  }
  out <- .find_similar_words(count = rbind(nouns, entity), method = method, level = level) |>
    dplyr::arrange(-n) |>
    dplyr::ungroup() |>
    dplyr::select(subject) |>
    dplyr::slice_head(n = n) |>
    unlist()
  class(out) <- c("subjects", class(out))
  out
  # todo: what to do with small words currently excluded?
  # todo: what to do with plural words
}

.clean_token <- function(v) {
  stringr::str_squish(tm::removePunctuation(tm::removeWords(
    stringr::str_replace_all(tolower(v), "_", " "), quanteda::stopwords())))
}

.find_similar_words <- function(count, method, level) {
  group <- subjects <- subject <- NULL
  out <- purrr::map_dfr(count[,1][[1]], ~ {
    i <- which(stringdist::stringdist(., count[,1][[1]], method) < level)
    dplyr::tibble(index = i, subjects = count[,1][[1]][i])
  }, .id = "group") |>
    dplyr::distinct(index, .keep_all = TRUE) |>
    dplyr::left_join(count, by = c("subjects" = names(count[,1]))) |>
    dplyr::group_by(group) |>
    dplyr::summarize(subject = paste(unique(subjects), collapse = "|"),
                     n = sum(n)) |>
    dplyr::mutate(n = n/(stringr::str_count(subject, "\\|")+1)) |>
    dplyr::select(subject, n) |>
    dplyr::distinct()
}

#' Extract terms related to subjects
#'
#' @param v Text vector or annotated data frame.
#' @param subjects Vector containing subjects.
#' @import quanteda
#' @import dplyr
#' @importFrom keyATM keyATM keyATM_read
#' @importFrom stringr str_detect str_remove_all
#' @examples
#' \donttest{
#' extract_related_terms(US_News_Conferences_1960_1980[1:2, 3],
#'                       subjects = extract_subjects(US_News_Conferences_1960_1980[1:2, 3]))
#' }
#' @export
extract_related_terms <- function(v, subjects) {
  doc_id <- sentence_id <- token <- text <- entity <- entities <-
    nouns <- pos <- tok <- NULL
  if (any(class(v) == "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v) | "segment" %in% names(v)) {
      v <- dplyr::mutate(v, text = ifelse(is.na(entities), nouns,
                                          paste0(nouns, " ", entities))) |>
        dplyr::select(text)
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::filter(v, entity != "" | pos == "NOUN") |>
        dplyr::group_by(doc_id, sentence_id) |>
        dplyr::summarise(text = paste(token, collapse = " ")) |>
        dplyr::ungroup() |>
        dplyr::select(text)
    }
  }
  # Get tokens and document feature matrix
  tok <- quanteda::corpus_reshape(quanteda::corpus(v), "sentences") %>%
    quanteda::tokens(remove_numbers = TRUE, remove_punct = TRUE,
                     remove_symbols = TRUE, remove_separators = TRUE,
                     remove_url = TRUE) %>%
    quanteda::tokens_tolower() %>%
    quanteda::tokens_select(min_nchar = 3) %>%
    quanteda::dfm() %>%
    quanteda::dfm_remove(quanteda::stopwords("en"))
  tok <- quanteda::dfm_subset(tok, quanteda::ntoken(tok) > 0)
  out <- suppressMessages(keyATM::keyATM(docs = keyATM::keyATM_read(texts = tok),
                                         no_keyword_topics = 0,
                                         keywords = .as_dictionary(subjects, tok),
                                         model = "base"))
  out <- as.list(keyATM::top_words(out))
  out <- lapply(out, function(x)
    stringr::str_squish(stringr::str_remove_all(
      ifelse(stringr::str_detect(x, " \\[([:digit:])"),  "", x), "\\[\\✓\\]")))
  out <- lapply(out, function(x) x[x!=""])
  names(out) <- stringr::str_remove_all(names(out), "[0-9]|\\_")
  class(out) <- c("related_subjects", class(out))
  out
  # todo: get only words above certain threshold for topics?
  # todo: what to do with overlapping words/topics?
  # todo: fix issue with multiple word subjects
}

# helper function
.as_dictionary <- function(v, dfm) {
  out <- list()
  names <- ifelse(stringr::str_detect(v, "\\|"), # split multiple elements
                  stringr::str_split_i(v, "\\|", i = 1), v)
  for (i in seq_len(length(v))) {
    out[[i]] <- ifelse(stringr::str_detect(v[[i]], "\\|"),
                       stringr::str_split(v[[i]], "\\|"),
                       v[[i]])
  }
  out <- lapply(out, function(x) stringr::str_replace_all(unname(unlist(x)),
                                                          " ", "_"))
  names(out) <- names # add names
  out[unlist(lapply(out, function(x) any(x %in% colnames(dfm))))]
  # todo: warn users that topics that do not match names in DFM are removed
}

#' Extract context for string matches
#'
#' A function for getting string matches and the context in which they occur.
#' @param v Text vector or annotated data frame.
#' @param match Character string to be matched.
#' For multiple strings, please use "|" as a separator.
#' @param level At which text level do you want matches to be returned?
#' Defaults to "sentences".
#' Options are sentences, words, and paragraph.
#' @param n Number of sentences or words matched before and after string match.
#' Defaults to 1.
#' That is, one word or one sentence before, and after, string match.
#' For paragraphs, n is always set to one.
#' @importFrom stringr str_detect str_extract_all
#' @importFrom dplyr group_by summarise select
#' @examples
#' \donttest{
#' extract_context(match = "war|weapons of mass destruction|conflict|NATO|peace",
#'                 v = US_News_Conferences_1960_1980$text[100],
#'                 level = "sentences", n = 2)
#' }
#' @return A list of string matches an their context
#' @export
extract_context <- function(match, v, level = "sentences", n = 1) {
  doc_id <- sentence <- text <- token <- NULL
  if (any(class(v) == "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- dplyr::group_by(doc_id) |>
        dplyr::summarise(text = paste(sentence, collapse = " ")) |>
        dplyr::select(text)
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::group_by(doc_id) |>
        dplyr::summarise(text = paste(token, collapse = " ")) |>
        dplyr::select(text)
    }
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
#' @param v Text vector or annotated data frame.
#' @param splitsign Where do you want to split?
#' By default sentences (".").
#' This can also be words, signals or other markers you want.
#' For special characters, please use escape sign before (i.e. "\\").
#' @return A splitted list for each row
#' @importFrom dplyr group_by summarise select
#' @examples
#' \donttest{
#' text <- "This is the first sentence. This is the second sentence."
#' split_text(text)
#' }
#' @export
split_text <- function(v, splitsign = "\\.") {
  doc_id <- sentence <- token <- text <- NULL
  if (any(class(v) == "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- dplyr::group_by(doc_id) |>
        dplyr::summarise(text = paste(sentence, collapse = " ")) |>
        dplyr::select(text)
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::group_by(doc_id) |>
        dplyr::summarise(text = paste(token, collapse = " ")) |>
        dplyr::select(text)
    }
  }
  t <-  strsplit(as.character(v), splitsign)
  # Add attribute for the number of divisions
  for(i in seq_len(length(t))) {
    attr(t[[i]], "Parts") <- paste0("Number of parts = ", lengths(t[i]))
  }
  t
}

#' Extract text matches
#'
#' Get texts in which only certain "matches" occur.
#' @param v Text vector or annotated data frame.
#' @param match A regex match for a word(s) or expression.
#' For multiple words, please use "|" to divide them.
#' @param invert Do you want texts without certain matches to be returned?
#' By default FALSE.
#' @param ignore.case Should case be ignored?
#' By default, TRUE.
#' @importFrom purrr map_chr
#' @return A list of matches of the same length as text variable
#' @examples
#' \donttest{
#' text <- c("This function was created on the 29 September 2021",
#' "Today is October 12, 2021")
#' extract_match(text, "October")
#' }
#' @export
extract_match <- function(v, match, invert = FALSE,
                          ignore.case = TRUE) {
  doc_id <- token <- text <- NULL
  if (any(class(v) == "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- v[["sentence"]]
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::group_by(doc_id) |>
        dplyr::summarise(text = paste(token, collapse = " ")) |>
        dplyr::select(text)
    }
  }
  if (invert == TRUE & ignore.case == FALSE) {
    t <- lapply(v, function(x) grep(match, x, value = TRUE,
                                       ignore.case = FALSE, invert = TRUE))
  } else if (invert == TRUE & ignore.case == TRUE) {
    t <- lapply(v, function(x) grep(match, x, value = TRUE,
                                       ignore.case = TRUE, invert = TRUE))
  } else if (invert == FALSE & ignore.case == FALSE) {
    t <- lapply(v, function(x) grep(match, x, value = TRUE,
                                       ignore.case = FALSE))
  } else {
    t <- lapply(v, function(x) grep(match, x, value = TRUE,
                                       ignore.case = TRUE))
  }
  t
}

#' Extract similarities and differences in texts/segments
#'
#' @param v Text vector or annotated data frame.
#' @param comparison Would you like to extract similarities or differences
#' between treaties? Options are "similarities" or "differences".
#' Defaults to "similarities".
#' @param method A method for checking similarities or differences.
#' For similarities, defaults to "correlation" method.
#' Other methods from `quanteda.textstats::textstat_simil()`
#' include "cosine", "jaccard", "ejaccard", "dice", "edice",
#' "simple matching", and "hamann".
#' For differences, fedaukts to "euclidean".
#' Other methods from `quanteda.textstats::textstat_dist()` include
#' "manhattan", "maximum", "canberra", and "minkowski".
#' @importFrom quanteda.textstats textstat_simil textstat_dist
#' @examples
#' \donttest{
#' extract_similarities(US_News_Conferences_1960_1980[1:2,3])
#' }
#' @export
extract_similarities <- function(v, comparison = "similarities", method) {
  doc_id <- token <- text <- NULL
  if (any(class(v) == "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- v[["sentence"]]
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::group_by(doc_id) |>
        dplyr::summarise(text = paste(token, collapse = " ")) |>
        dplyr::select(text)
    }
  }
  v <- quanteda::corpus(v)
  if (comparison == "similarities") {
    if(missing(method)) method = "correlation"
    quanteda.textstats::textstat_simil(quanteda::dfm(v), method = method)
  } else {
    if(missing(method)) method = "euclidean"
    quanteda.textstats::textstat_dist(quanteda::dfm(v), method = method)
  }
  # todo: add plotting method that plots texts similarities as a dendogram
}
