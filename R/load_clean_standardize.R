#' Load text from PDFs
#'
#' @param path The path to a PDF file or a folder containing multiple PDFs.
#' @return A list of texts.
#' @importFrom pdftools pdf_ocr_text
#' @export
load_pdf <- function(path) {
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

#' Parse text with NLP
#'
#' Wrapper for `spacyr::spacy_parse` function.
#' @param v Text vector
#' @param level Do you want to parse words or sentences? Words by default.
#' @import spacyr
#' @importFrom dplyr group_by summarise ungroup
#' @importFrom stringr str_squish
#' @importFrom usethis ui_info
#' @examples
#' \donttest{
#' annotate_text(US_News_Conferences_1960_1980[1:2, 3])
#' }
#' @export
annotate_text <- function(v, level = "words") {
  doc_id <- sentence_id <- token_id <- token <- pos <- tag <- lemma <- entity <- NULL
  suppressWarnings(spacyr::spacy_initialize(model = "en_core_web_sm"))
  parse <- spacyr::spacy_parse(v, tag = TRUE)
  suppressWarnings(spacyr::spacy_finalize())
  if (level == "sentences" | level == "sentence") {
    usethis::ui_info("Annotating sentences...")
    entity <- spacyr::entity_extract(parse) |>
      dplyr::group_by(sentence_id, doc_id) |>
      dplyr::summarise(entities = unique(paste(entity, collapse = " ")))
    parse <- dplyr::group_by(parse, doc_id, sentence_id) |>
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
                         collapse = " "))) |>
      dplyr::left_join(entity, by = c("doc_id", "sentence_id")) |>
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
