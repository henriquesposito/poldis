#' Extract future promises from political discourses
#'
#' @param v Text vector or annotated data frame.
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#' @importFrom stats ave
#' @examples
#' \donttest{
#' extract_promises(US_News_Conferences_1960_1980[1:2,3])
#' }
#' @export
extract_promises <- function(v) {
  tags <- tokens <- sentence <- seg_id <- poss <-
    lemmas <- segment <- doc_id <- entities <- problems <- promises <-
    lemmas_p <- adv_p <- adj_p <- promise <- problem <- NULL
  if (any(class(v) == "data.frame")) {
    if ("token_id" %in% names(v))
      stop("Please declare a text vector or an annotated data frame at the sentence level.")
  } else v <- suppressMessages(annotate_text(v, level = "sentences"))
  # segment text first
  # assign IDs for segments
  v <- v |>
    dplyr::group_by(doc_id) |>
    dplyr::mutate(lemmas = tolower(lemmas)) |>
    dplyr::mutate(seg_id = ifelse(stringr::str_detect(lemmas,
                                                      "first|second|third(?!s)|
                                                        |fourth(?!s)|fifth(?!s)|
                                                        |sixth(?!s)|finally|end|
                                                        |lastly|begin by|start by|
                                                        |begin with |otherwise|apart from|
                                                        |besides|other than|contrary|
                                                        |conversely|moreover|furthermore|
                                                        |further|however|addition|alternate|
                                                        |anyway|while|ladies|gentlemen|
                                                        |distinguished|sir|now|every|another|
                                                        |one hand|on the other hand|compared to
                                                        |let me|greet"),
                                  1:dplyr::n(), NA)) |> # first attempt to identify breaks in the text
    tidyr::fill(seg_id) |>
    dplyr::mutate(seg_id = ifelse(is.na(seg_id), 0, seg_id),
                  seg_id = paste0(doc_id, "-", seg_id))
  # mark out problems/context rather than removing them
  v <- v |>
    dplyr::mutate(problem = ifelse(stringr::str_detect(lemmas,
                                                       "problem|issue|challenge|
                                                        |matter|can of worms|
                                                        |deep water|pain|hydra|
                                                        |matter|difficulty|
                                                        |trouble|killer|kink|
                                                        |pitfall|trap|hindrance|
                                                        |impediment|deterrent|
                                                        |wrinkle|puzzle|case|
                                                        |conundrum|dilemma|
                                                        |question|hazard|
                                                        |predicament|plight|
                                                        |quandary|hard time|
                                                        |stress|strain|crisis|
                                                        |mire|complex|address|
                                                        |solve|to resolve|tackle|
                                                        |fix|confront|face|
                                                        |take care of|consider|
                                                        |recognise|reject|ignore|
                                                        |act on|give attention|
                                                        |direct attention|handle|
                                                        |treat|deal with"),
                                   paste(sentence), NA))
  # identify promises
  v <- v |> dplyr::mutate(promise = ifelse(stringr::str_detect(tags, "PRP MD ")|
                                             stringr::str_detect(lemmas,
                                             "going to|need to|ready to|
                                             |is time to|commit to|promise to|
                                             |plan to|intend to|let 's"),
                                           paste(sentence), NA),
                          promise = ifelse(!is.na(problem), NA, promise)) |>
    # remove negative sentences for now
    dplyr::filter(is.na(promise) | !stringr::str_detect(promise, " not "))
    # todo: exclude past sentences that contain a modal verb/adverb
  # identify lemmas, adjectives, and adverbs that are linked to promises for scoring
  v <- v |> dplyr::mutate(lemmas_p = ifelse(!is.na(promise), lemmas, NA),
                          adv_p = ifelse(!is.na(promise), adverbs, NA),
                          adj_p = ifelse(!is.na(promise), adjectives, NA))
  # paste together potential promises connected to one another
  # and exclude instances of promises that are problems within each segment
  v <- within(v,
              {
                segment <- ave(sentence, seg_id, FUN = toString)
                poss <- ave(poss, seg_id, FUN = toString)
                tags <- ave(tags, seg_id, FUN = toString)
                lemmas <- ave(lemmas_p, seg_id, FUN = toString)
                entities <- ave(entities, seg_id, FUN = toString)
                adverbs <- ave(adv_p, seg_id, FUN = toString)
                adjectives <- ave(adj_p, seg_id, FUN = toString)
                nouns <- ave(nouns, seg_id, FUN = toString)
                ntoken <- ave(ntoken, seg_id, FUN = sum)
                problems <- ave(problem, seg_id, FUN = toString)
                promises <- ave(promise, seg_id, FUN = toString)
              }) |>
    dplyr::select(doc_id, seg_id, segment, poss, tags, lemmas, entities,
                  adverbs, adjectives, nouns, ntoken, problems, promises) |>
    dplyr::distinct() |>
    dplyr::mutate(segment = stringr::str_replace_all(segment, "[.],", "."),
                  segment = stringr::str_remove_all(segment, "\n"),
                  segment = stringr::str_squish(segment),
                  lemmas = stringr::str_remove_all(lemmas, "NA, "),
                  lemmas = stringr::str_replace_all(lemmas, "., NA", "."),
                  adverbs = stringr::str_remove_all(adverbs, "NA, |, NA"),
                  adjectives = stringr::str_remove_all(adjectives, "NA, |, NA"),
                  nouns = stringr::str_replace_all(nouns, ", ,", ","),
                  entities = stringr::str_remove_all(entities, "NA, "),
                  entities = stringr::str_remove_all(entities, ", NA"),
                  problems = stringr::str_remove_all(problems, "NA, "),
                  problems = stringr::str_replace_all(problems, "., NA", "."),
                  promises = stringr::str_remove_all(promises, "NA, "),
                  promises = stringr::str_replace_all(promises, "., NA", ".")) |>
    dplyr::ungroup()
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
#' @param n Number of terms.
#' @import LSX
#' @import quanteda
#' @examples
#' \donttest{
#' extract_related_terms(US_News_Conferences_1960_1980[1:2, 3],
#'                       subjects = extract_subjects(US_News_Conferences_1960_1980[1:2, 3]))
#' }
#' @export
extract_related_terms <- function(v, subjects, n = 5) {
  doc_id <- token <- text <- entity <- entities <- nouns <- pos <- NULL
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
        dplyr::group_by(doc_id) |>
        dplyr::summarise(text = paste(token, collapse = " ")) |>
        dplyr::select(text) |>
        dplyr::ungroup()
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
  lss <- LSX::textmodel_lss(dfmts, seeds = dict, k = sum(lengths(dict))) # model
  terms <- LSX::bootstrap_lss(lss, mode = "terms")[1:n,]
  out <- list()
  if (is.null(colnames(terms))) {
    out[[dict]] <- terms
  } else {
    for (i in 1:length(dict)) {
      out[[names(dict)[i]]] <- c(terms[,c(colnames(terms) %in% dict[[i]])])
    }
  }
  out <- ifelse(lapply(out, rlang::is_empty), names(out), out)
  names(out) <- names(dict)
  un <- unlist(out)
  out <- Map(`[`, out, utils::relist(!duplicated(un), skeleton = out))
  # remove duplicate words
  class(out) <- c("related_subjects", class(out))
  out
  # todo: fix issue with multiple word subjects
}

# helper function
.as_dictionary <- function(a) {
  out <- list()
  if (is.null(names(a))) {
    names(a) <- a
  }
  names(a) <- ifelse(stringr::str_detect(a, "\\|"),
                     stringr::str_split_i(a, "\\|", i = 1), a)
  for (i in names(a)) {
    out[[i]] <- ifelse(stringr::str_detect(a[[i]], "\\|"),
                       lapply(stringr::str_split(a[[i]], "\\|"), function(x) paste0(x, "*")),
                       paste0(a[[i]], "*"))
  }
  quanteda::dictionary(out)
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
