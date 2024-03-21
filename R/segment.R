# Functions to help with text segmentation

#' Extract future promises from political discourses
#'
#' @param v Text vector.
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @export
extract_promises <- function(v) {
  tags <- tokens <- sentence <- NULL
  if (any(class(v) == "data.frame")) {
    if ("token_id" %in% names(v))
      stop("Please declare a text vector or an annotated data frame at the sentence level.")
  } else v <- suppressMessages(annotate_text(v, level = "sentences"))
  v <- v |> dplyr::filter(stringr::str_detect(tags, " MD ") |
                       stringr::str_detect(sentence, "going to|need to|ready to|is time to|commit to|promise to|intend to|let's"))
  class(v) <- c("promises", class(v))
  v
  # todo: extract related sentences around promises and re-paste together
}

#' Extract most frequent subjects from political discourses
#'
#' @param v Text vector.
#' @param n Number of subjects
#' @import dplyr
#' @importFrom purrr map_dfr
#' @export
extract_subjects <- function(v, n = 20) {
  sentence_id <- doc_id <- entity <- group <- subjects <- subject <- NULL
  if (any(class(v) == "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- suppressMessages(annotate_text(v[["sentence"]]))
    }
  } else v <- suppressMessages(annotate_text(v))
  out <- spacyr::entity_extract(v) |>
    group_by(doc_id, sentence_id) |>
    mutate(duplicated = n() > 1,
           entity = stringr::str_squish(
             tm::removePunctuation(tm::removeWords(
             stringr::str_replace_all(tolower(entity), "_", " "),
             tm::stopwords())))) |>
    filter(duplicated == FALSE) |>
    ungroup()
  out <- purrr::map_dfr(out$entity, ~ {
    i <- which(stringdist::stringdist(., out$entity, "lv") < 2 &
                 nchar(out$entity) > 3)
    tibble(index = i, subjects = out$entity[i])
  }, .id = "group") |>
    distinct(index, .keep_all = T) |>
    group_by(group) |>
    summarize(subject = paste(unique(subjects), collapse = "|"),
              count = n()) |>
    arrange(-count) |>
    ungroup() |>
    filter(subject != "") |>
    select(subject) |>
    slice_head(n = n) |>
    unlist()
  class(out) <- c("subjects", class(out))
  out
  # todo: exclude small words but sill count them
  # todo: change similarity method to get expressions together
  #(e.g. united states and unites states america)
}

#' Extract terms related to subjects
#'
#' @param v Text vector.
#' @param subjects List of subjects.
#' @param n Number of terms.
#' @import LSX
#' @import quanteda
#' @export
extract_related_terms <- function(v, subjects, n = 5) {
  doc_id <- token <- NULL
  if (any(class(v) == "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- v[["sentence"]]
    } else if ("token_id" %in% names(v)) {
      v <- group_by(doc_id) |>
        dplyr::summarise(text = paste(token, collapse = " "))
    }
  }
  corp <- quanteda::corpus(v) |>
    quanteda::corpus_reshape(to = "sentences") # sentences
  toks <- quanteda::tokens(corp, remove_punct = TRUE, remove_symbols = TRUE,
                           remove_numbers = TRUE, remove_url = TRUE) # tokens
  dfmt <- quanteda::dfm(toks) |>
    quanteda::dfm_remove(quanteda::stopwords("en")) # document feature matrix
  dict <- .as_dictionary(subjects) # set subjects
  out <- list()
  for (i in 1:length(dict)) out[[i]] <- quanteda::dfm_select(dfmt, dict[[i]])
  dfmts <- quanteda::dfm_remove(dfmt, dict) |> cbind(do.call(cbind, out))
  lss <- LSX::textmodel_lss(dfmts, seeds = dict, k = 300) # model
  terms <- LSX::bootstrap_lss(lss, mode = "terms")[1:n,]
  out <- list()
  for (i in 1:length(dict)) {
    out[[names(dict)[i]]] <- c(terms[,c(colnames(terms) %in% dict[[i]])])
  }
  out <- ifelse(lapply(out, rlang::is_empty), names(out), out)
  names(out) <- names(dict)
  un <- unlist(out)
  out <- Map(`[`, out, utils::relist(!duplicated(un), skeleton = out))
  # remove duplicate words
  class(out) <- c("related_subjects", class(out))
  out
  # todo: make it work with annotated data frames
  # todo: fix issue with multiple word subjects
}

# helper function
.as_dictionary <- function(a) {
  names(a) <- ifelse(stringr::str_detect(a, "\\|"),
                     stringr::str_split_i(a, "\\|", i = 1), a)
  out <- list()
  for (i in names(a)) {
    out[[i]] <- ifelse(stringr::str_detect(a[[i]], "\\|"),
                       stringr::str_split(a[[i]], "\\|"), a[[i]])
  }
  quanteda::dictionary(out)
}

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
