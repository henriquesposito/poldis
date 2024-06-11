#' Gather topic from political discourses
#'
#' @param .data A data frame, "promises" data frame, or text vector.
#' For data frames, function will search for "text" variable.
#' For promises data frames coded using `select_promises()`,
#' function will search for "promises" variable.
#' @param dictionary The dictionary of 20 major political topics from the
#' Comparative Agendas Project (Jones et al., 2023) is used by default.
#' Users can also declare a custom dictionary as a vector or a list.
#' If users declare a vector, each element is treated as a independent topic.
#' If users declare a list of subjects and related terms, function understands
#' names as topic and words as terms.
#' @import dplyr
#' @examples
#' \donttest{
#' gather_topics(US_News_Conferences_1960_1980[1:5, 3])
#' gather_topics(US_News_Conferences_1960_1980[1:5, 3],
#'                dictionary = c("military", "development"))
#' gather_topics(.data = US_News_Conferences_1960_1980[1:5, 3],
#'                dictionary = list("military" = c("military", "gun", "war"),
#'                                  "development" = c("development", "interest rate", "banks")))
#' }
#' @export
gather_topics <- function(.data, dictionary = "CAP") {
  Words <- NULL
  # get text variable
  if (inherits(.data, "promises")) {
    text <- stats::na.omit(.clean_token(getElement(.data, "promises")))
  } else if (inherits(.data, "data.frame")) {
    text <- .clean_token(getElement(.data, "text"))
  } else text <- .clean_token(.data)
  # get dictionary
  if (any(dictionary ==  "CAP")) {
    dictionary <- CAP_topics %>%
      dplyr::mutate(Words = stringr::str_replace_all(textstem::lemmatize_words(
        stringr::str_squish(tolower(Words))), ", ", "|"))
    subjects <- dictionary$Words
    names(subjects) <- dictionary$Topic
    } else if (is.list(dictionary)) {
      subjects <- lapply(dictionary, function(x) paste0(x, collapse = "|"))
    } else {
      subjects <- dictionary
      names(subjects) <- subjects
    }
  # match terms
  out <- list()
  for (i in names(subjects)) {
    out[[i]] <- stringr::str_count(text, subjects[[i]])
    }
  out <- apply(data.frame(out), 1, function(i) which(i > 0))
  out <- lapply(out, function(x) paste0(names(x), collapse = ", "))
  class(out) <- c("topics", class(out))
  out
  # todo: fix issue for when list is declared as dictionary
  # todo: get proportion of topics for texts with multiple topics?
  # todo: normalize scores by the number of words in dictionary for a topic?
  out
}

.clean_token <- function(v) {
  textstem::lemmatize_words(stringr::str_squish(tm::removePunctuation(
    tm::removeWords(tolower(v), quanteda::stopwords()))))
}

#' Gather terms related to subjects
#'
#' @param .data A data frame, promises, or text vector.
#' For data frames, function will search for "text" variable.
#' For promises data, function will search for "promises" variable.
#' @param dictionary The dictionary of 20 major political topics from the
#' Comparative Agendas Project (Jones et al., 2023) is used by default.
#' Users can also declare a custom dictionary as a vector or a list.
#' If users declare a vector, each element is treated as a independent topic.
#' If users declare a list of subjects and related terms, function understands
#' names as topic and words as terms.
#' @import quanteda
#' @import dplyr
#' @importFrom keyATM keyATM keyATM_read
#' @importFrom stringr str_detect str_remove_all
#' @examples
#' \donttest{
#' gather_related_terms(US_News_Conferences_1960_1980[1:5, 3], dictionary = "CAP")
#' gather_related_terms(US_News_Conferences_1960_1980[1:5, 3],
#'                       dictionary = c("military", "development"))
#' gather_related_terms(US_News_Conferences_1960_1980[1:5, 3],
#'                       dictionary = list("military" = c("military", "gun", "war"),
#'                                         "development" = c("development", "interest rate", "banks")))
#' }
#' @export
gather_related_terms <- function(.data, dictionary) {
  Words <- NULL
  # get text variable
  if (inherits(.data, "promises")) {
    text <- stats::na.omit(.clean_token(getElement(.data, "promises")))
  } else if (inherits(.data, "data.frame")) {
    text <- .clean_token(getElement(.data, "text"))
  } else text <- .clean_token(.data)
  # check dictionary
  if (any(dictionary ==  "CAP")) {
    subjects <- CAP_topics %>%
      dplyr::mutate(Words = stringr::str_replace_all(
        textstem::lemmatize_words(tolower(Words)), ", ", "|"))
    dictionary <- subjects$Words
    names(dictionary) <- subjects$Topic
  }
  # Get tokens and document feature matrix
  tok <- quanteda::corpus_reshape(quanteda::corpus(text), "sentences") %>%
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
                                         keywords = .as_dictionary(dictionary, tok),
                                         model = "base"))
  out <- as.list(keyATM::top_words(out))
  out <- lapply(out, function(x)
    stringr::str_squish(stringr::str_remove_all(
      ifelse(stringr::str_detect(x, " \\[([:digit:])"),  "", x),
      "\\[\\u2713\\]")))
  out <- lapply(out, function(x) x[x!=""])
  names(out) <- stringr::str_remove_all(names(out), "[0-9]|\\_")
  class(out) <- c("related_subjects", class(out))
  out
  # todo: get only words above certain threshold for topics?
  # todo: what to do with overlapping words/topics?
  # todo: what to do with small words currently excluded for similar terms?
  # todo: fix issue with multiple word subjects
}

# helper function
.as_dictionary <- function(dictionary, dfm) {
  out <- list()
  names <- ifelse(stringr::str_detect(dictionary, "\\|"), # split multiple elements
                  stringr::str_split_i(dictionary, "\\|", i = 1), dictionary)
  for (i in seq_len(length(dictionary))) {
    out[[i]] <- ifelse(stringr::str_detect(dictionary[[i]], "\\|"),
                       stringr::str_split(dictionary[[i]], "\\|"),
                       dictionary[[i]])
  }
  out <- lapply(out, function(x) stringr::str_replace_all(unname(unlist(x)),
                                                          " ", "_"))
  names(out) <- names # add names
  out[unlist(lapply(out, function(x) any(x %in% colnames(dfm))))]
  # todo: warn users that topics that do not match names in DFM are removed
}
