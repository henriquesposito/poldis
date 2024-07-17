#' Gather topic from political discourses
#'
#' @param .data A data frame, priorities data frame coded using
#' `select_priorities()`, or text vector.
#' For data frames, function will search for "text" variable.
#' For priorities data frame function will search for "priorities" variable.
#' @param dictionary The dictionary of 20 major political topics from the
#' Comparative Agendas Project (Jones et al., 2023) is used by default.
#' Users can also declare a custom dictionary as a vector or a list.
#' If users declare a vector, each element is treated as a independent topic.
#' If users declare a list of subjects and related terms, function understands
#' names as topic and words as terms.
#' @import dplyr
#' @importFrom tidyr unite
#' @return A list of topics present in each text separated by comma.
#' @examples
#' \donttest{
#' gather_topics(US_News_Conferences_1960_1980[1:5, 3])
#' gather_topics(US_News_Conferences_1960_1980[1:5, 3],
#'               dictionary = c("military", "development"))
#' gather_topics(US_News_Conferences_1960_1980[1:5, 3],
#'               dictionary = list("military" = c("military", "gun", "war"),
#'                                 "development" = c("development", "interest rate", "banks")))
#' }
#' @export
gather_topics <- function(.data, dictionary = "CAP") {
  Words <- topics <- NULL
  # get text variable
  if (inherits(.data, "priorities")) {
    text <- stats::na.omit(.clean_token(getElement(.data, "priorities")))
  } else if (inherits(.data, "data.frame")) {
    text <- .clean_token(getElement(.data, "text"))
  } else text <- .clean_token(.data)
  # get dictionary
  if (any(dictionary ==  "CAP")) {
    dictionary <- CAP_topics %>%
      dplyr::mutate(Words = stringr::str_replace_all(.clean_token(Words),
                                                     ", ", "\\\\b|\\\\b"))
    subjects <- dictionary$Words
    names(subjects) <- dictionary$Topic
    } else if (is.vector(dictionary) && is.atomic(dictionary)) {
      subjects <- dictionary
      names(subjects) <- subjects
    } else {
      subjects <- unlist(lapply(dictionary, function(x)
        paste0(x, collapse = "\\b|\\b")))
    }
  # match terms
  out <- lapply(names(subjects), function(i) stringr::str_count(text, subjects[[i]]))
  names(out) <- names(subjects)
  out <- data.frame(out) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                ~ if_else(. > 0, dplyr::cur_column(), NA))) %>%
    tidyr::unite(col = topics,  na.rm = TRUE, sep = ", ")
  out <- gsub("\\.", " ", out$topics)
  class(out) <- "topics"
  out
}

#' Gather terms related to subjects
#'
#' @param .data A data frame, priorities data frame coded using
#' `select_priorities()`, or text vector.
#' For data frames, function will search for "text" variable.
#' For priorities data frame function will search for "priorities" variable.
#' @param dictionary The dictionary of 20 major political topics from the
#' Comparative Agendas Project (Jones et al., 2023) is used by default.
#' Users can also declare a custom dictionary as a vector or a list.
#' If users declare a vector, each element is treated as a independent topic.
#' If users declare a list of subjects and related terms, function understands
#' names as topic and words as terms.
#' @import quanteda
#' @import dplyr
#' @importFrom stringr str_detect str_remove_all
#' @return A list of related terms to each of the topics declared in dictionary.
#' @details This function relies on keyword assisted topic models implemented
#' in the `\{keyATM\}` package to find related words based on the topics
#' provided and texts in which they appear.
#' @references
#' Eshima S, Imai K, and Sasaki T. 2024.
#' “Keyword-Assisted Topic Models.”
#' _American Journal of Political Science_, 68(2): 730-750.
#' \doi{10.1111/ajps.12779}
#' @export
gather_related_terms <- function(.data, dictionary) {
  Words <- NULL
  thisRequires("keyATM")
  # get text variable
  if (inherits(.data, "priorities")) {
    text <- stats::na.omit(.clean_token(getElement(.data, "priorities")))
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
  # get tokens and document feature matrix
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
}

# helper function
.as_dictionary <- function(dictionary, dfm) {
  if (length(dictionary) != sum(names(dictionary) != "",na.rm = TRUE)) {
    names <- ifelse(stringr::str_detect(dictionary, "\\|"), # split multiple elements
                    stringr::str_split_i(dictionary, "\\|", i = 1), dictionary)
  } else names <- names(dictionary)
  out <- lapply(seq_len(length(dictionary)), function(i)
    ifelse(stringr::str_detect(dictionary[[i]], "\\|"),
           stringr::str_split(dictionary[[i]], "\\|"), dictionary[[i]]))
  out <- lapply(out, function(x) stringr::str_replace_all(unname(unlist(x)), " ", "_"))
  names(out) <- names # add names
  out[unlist(lapply(out, function(x) any(x %in% colnames(dfm))))]
}
