# Load, clean, and standardise texts

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
#' @import spacyr
#' @export
annotate_text <- function(v) {
  spacyr::spacy_initialize(model = "en_core_web_sm")
  parse <-  spacyr::spacy_parse(v)
  spacyr::spacy_finalize()
  parse
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
