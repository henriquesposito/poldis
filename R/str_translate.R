#' Translate Strings
#'
#' Tranlates strings in data and returns object of the same lenght as original string.
#' The function automacally identifies language from strings by rows
#' so that tranlations to target language are more accurate and allows
#' for text in multiple languages to be present in string.
#' @param s A character string
#' @param target_lang Which language would you like this translated to?
#' Please provide a two letter language abbreviation (e.g. "en" or "pt")
#' @param key Google API key.
#  For more information please go to: https://cloud.google.com/translate/docs/setup
#' @return A character vector of the same length of original.
#' @export
str_translate <- function(s, target_lang, key) {

  if(missing(key)) {
    stop("Please declare a Google API key.
         For more information please go to: https://cloud.google.com/translate/docs/setup")
  }

  if(missing(target_lang)||length(target_lang > 2)) {
    stop("Please declare a target language.
         Language should be decalred following ISO two letter codes.
         For more info, please go to: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes")
  }

  out <- purrr::map(s, as.character)
  . <- NULL

  source_lang <- out %>%
    vapply(., purrr::map_chr, "", cld2::detect_language) %>%
    data.frame(check.names = FALSE)
    out <- cbind(out, source_lang)

  for (k in seq_len(nrow(out))) {
    if (is.na(out$.[k])) {
      out$out[k] == out$out[k]
      print(paste("Could not tranlate", [k], ". Langauge not detect.
                  Manually input source language argument, if possible, for better results.")
    } else {
      out$out[k] <- suppressWarnings(translateR::translate(content.vec = out$out[k],
                                                           google.api.key = api_key,
                                                           source.lang = out$.[k],
                                                           target.lang = target_lang))
    }
  }
  out <- out$out
}
