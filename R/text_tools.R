#' Extract a list of possible names of individuals in texts
#'
#' @param v A text vector.
#' @importFrom dplyr distinct filter %>% summarize group_by
#' @importFrom stringr str_squish
#' @import spacyr
#' @return A list of individual names.
#' @details The function relies on NLP models and, therefore, results
#' might not be accurate or consistent.
#' @examples
#' #extract_names(US_News_Conferences_1960_1980[20, 3])
#' @export
extract_names <- function(v) {
  thisRequires("stringdist")
  ent_type <- text <- s <- NULL
  out <- spacyr::spacy_extract_entity(v, type = "named") %>%
    dplyr::filter(ent_type == "PERSON") %>%
    dplyr::mutate(names = .clean_token(text)) %>%
    dplyr::group_by(names) %>%
    dplyr::summarise(count = sum(length))
  # check if similar names are the same person
  s <- stringdist::stringsimmatrix(out$names, out$names, method = "cosine", q = 2)
  diag(s[, seq_len(ncol(s))]) <- 0
  s <- ifelse(s > 0.5, out$names, "")
  out$similar_names <- stringr::str_squish(apply(s, 2, paste, collapse = " "))
  # to do: setup plotting method (as a network)
  spacyr::spacy_finalize()
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

#' Extract dates from text
#'
#' Wrapper function for `messydates::as_messydates`.
#' @param v Text vector
#' @return A vector of the dates in text
#' @examples
#' #extract_date("Today is the twenty six of February of two thousand and twenty four")
#' @export
extract_date <- function(v) {
  thisRequires("messydates")
  messydates::as_messydate(v)
}

#' Extract location from strings
#'
#' @param v Text variable/object
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_extract
#' @importFrom purrr map_chr
#' @details Works well for Brazilian states and other countries.
#' Texts must be in English or Portuguese.
#' @return A list of the first location mentioned in texts.
#' @details If more than one location is found,
#' returns only the first match.
#' @examples
#' extract_location(c("This is the United States", "This is Sao Paulo",
#' "I was in Rio de Janeiro and Sao Paulo, then back to the United States"))
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
  v <- purrr::map_chr(v, 2)
  v
  # todo: find a better list of countries/cities/locations in the world to use
  # todo: use NLP to identify location entity
}

#' Extract text matches
#'
#' Get texts in which certain "matches" occur.
#' @param v Text vector or annotated data frame.
#' @param match A regex match for a word(s) or expression.
#' For multiple words, please use "|" to divide them.
#' @param invert Do you want texts without certain matches to be returned?
#' By default FALSE.
#' @param ignore.case Should case be ignored?
#' By default, TRUE.
#' @importFrom purrr map_chr
#' @importFrom dplyr group_by summarise select %>%
#' @return A list the same length as text variable
#' @examples
#' \donttest{
#' extract_match(c("This function was created on the 29 September 2021",
#' "Today is October 12, 2021"), "October")
#' }
#' @export
extract_match <- function(v, match, invert = FALSE,
                          ignore.case = TRUE) {
  doc_id <- token <- text <- NULL
  if (inherits(v, "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- v[["sentence"]]
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::group_by(doc_id) %>%
        dplyr::summarise(text = paste(token, collapse = " ")) %>%
        dplyr::select(text)
    }
  }
  if (invert == TRUE && ignore.case == FALSE) {
    t <- lapply(v, function(x) grep(match, x, value = TRUE,
                                       ignore.case = FALSE, invert = TRUE))
  } else if (invert == TRUE && ignore.case == TRUE) {
    t <- lapply(v, function(x) grep(match, x, value = TRUE,
                                       ignore.case = TRUE, invert = TRUE))
  } else if (invert == FALSE && ignore.case == FALSE) {
    t <- lapply(v, function(x) grep(match, x, value = TRUE, ignore.case = FALSE))
  } else {
    t <- lapply(v, function(x) grep(match, x, value = TRUE, ignore.case = TRUE))
  }
  t
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
#' @importFrom dplyr group_by summarise select %>%
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
  if (inherits(v, "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- dplyr::group_by(doc_id) %>%
        dplyr::summarise(text = paste(sentence, collapse = " ")) %>%
        dplyr::select(text)
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::group_by(doc_id) %>%
        dplyr::summarise(text = paste(token, collapse = " ")) %>%
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

#' Extract similarities and differences in texts/segments
#'
#' @param v Text vector or annotated data frame.
#' @param comparison Would you like to extract similarities or differences
#' between treaties?
#' Options are "similarities" or "differences".
#' Defaults to "similarities".
#' @param method A method for checking similarities or differences.
#' For similarities, defaults to "correlation" method.
#' Other methods from `quanteda.textstats::textstat_simil()`
#' include "cosine", "jaccard", "ejaccard", "dice", "edice",
#' "simple matching", and "hamann".
#' For differences, defaults to "euclidean".
#' Other methods from `quanteda.textstats::textstat_dist()` include
#' "manhattan", "maximum", "canberra", and "minkowski".
#' @importFrom dplyr group_by summarise select %>%
#' @examples
#' #extract_text_similarities(US_News_Conferences_1960_1980[1:2,3])
#' @export
extract_text_similarities <- function(v, comparison = "similarities", method) {
  thisRequires("quanteda.textstats")
  doc_id <- token <- text <- NULL
  if (inherits(v, "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- v[["sentence"]]
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::group_by(doc_id) %>%
        dplyr::summarise(text = paste(token, collapse = " ")) %>%
        dplyr::select(text)
    }
  }
  v <- quanteda::corpus(v)
  if (comparison == "similarities") {
    if(missing(method)) method = "correlation"
    quanteda.textstats::textstat_simil(quanteda::dfm(quanteda::tokens(v)), method = method)
  } else {
    if(missing(method)) method = "euclidean"
    quanteda.textstats::textstat_dist(quanteda::dfm(quanteda::tokens(v)), method = method)
  }
  # todo: add plotting method that plots texts similarities as a dendogram
}

#' Split texts
#'
#' Split texts into structured lists of lists according to a split sign.
#' @param v Text vector or annotated data frame.
#' @param splitsign Where do you want to split?
#' By default sentences (".").
#' This can also be words, signals or other markers you want.
#' For special characters, please use escape sign before (i.e. "\\").
#' @return A list of lists the same leghth as vector.
#' @importFrom dplyr group_by summarise select %>%
#' @examples
#' \donttest{
#' split_text("This is the first sentence. This is the second sentence.")
#' }
#' @export
split_text <- function(v, splitsign = "\\.") {
  doc_id <- sentence <- token <- text <- NULL
  if (inherits(v, "data.frame")) {
    if (!"doc_id" %in% names(v)) {
      stop("Please declare a text vector or an annotated data frame.")
    }
    if ("sentence" %in% names(v)) {
      v <- dplyr::group_by(doc_id) %>%
        dplyr::summarise(text = paste(sentence, collapse = " ")) %>%
        dplyr::select(text)
    } else if ("token_id" %in% names(v)) {
      v <- dplyr::group_by(doc_id) %>%
        dplyr::summarise(text = paste(token, collapse = " ")) %>%
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

#' Read text from PDFs
#'
#' @param path The path to a PDF file or a folder containing multiple PDFs.
#' @return A list of texts.
#' @export
read_pdf <- function(path) {
  thisRequires("pdftools")
  thisRequires("tesseract")
  if (grepl(".pdf", path)) {
    out <- paste(pdftools::pdf_ocr_text(x), collapse = " ")
  } else {
    out <- list()
    for (x in list.files(path = path, full.names = TRUE,
                         pattern = "*.pdf")) {
      out[[basename(x)]] <- paste(pdftools::pdf_ocr_text(x), collapse = " ")
    }
  }
  out
}

#' Annotate text with NLP
#'
#' This function builds upon `spacyr::spacy_parse` function to annotate texts.
#' @param v Text vector
#' @param level Do you want to parse words or sentences? Words by default.
#' @import spacyr
#' @importFrom dplyr group_by summarise ungroup %>%
#' @importFrom stringr str_squish
#' @examples
#' #annotate_text(US_News_Conferences_1960_1980[1:2, 3])
#' @export
annotate_text <- function(v, level = "words") {
  doc_id <- sentence_id <- token_id <- token <- pos <- tag <- lemma <- entity <- NULL
  suppressWarnings(spacyr::spacy_initialize(model = "en_core_web_sm"))
  parse <- spacyr::spacy_parse(v, tag = TRUE)
  suppressWarnings(spacyr::spacy_finalize())
  if (level == "sentences" | level == "sentence") {
    entity <- spacyr::entity_extract(parse) %>%
      dplyr::group_by(sentence_id, doc_id) %>%
      dplyr::summarise(entities = unique(paste(entity, collapse = " ")))
    parse <- dplyr::group_by(parse, doc_id, sentence_id) %>%
      dplyr::summarise(ntoken = max(token_id),
                       sentence = paste(token, collapse = " "),
                       poss = paste(pos, collapse = " "),
                       tags = paste(tag, collapse = " "),
                       lemmas = paste(lemma, collapse = " "),
                       adverbs = stringr::str_squish(paste(ifelse(
                         pos == "ADP", lemma, ""), collapse = " ")),
                       adjectives = stringr::str_squish(paste(ifelse(
                         pos == "ADJ", lemma, ""), collapse = " ")),
                       nouns = stringr::str_squish(paste(ifelse(
                         pos == "NOUN" & entity == "", lemma, ""),
                         collapse = " "))) %>%
      dplyr::left_join(entity, by = c("doc_id", "sentence_id")) %>%
      dplyr::ungroup()
  }
  parse
  # todo: add more informative messages/warnings for users
  # todo: add nouns, adverbs, adjectives and entities together
}

# Helper function for checking and downloading packages
thisRequires <- function(pkgname){
  if (!requireNamespace(pkgname, quietly = TRUE)) {
    if(utils::askYesNo(msg = paste("The", pkgname,
                                   "package is required to run this function. Would you like to install", pkgname, "from CRAN?"))) {
      utils::install.packages(pkgname)
    } else {
      stop(paste("Please install", pkgname, "from CRAN to run this function."))
    }
  }
}
