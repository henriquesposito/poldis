#' Translate text
#'
#' The function automatically identifies language by rows,
#' this allows for text in multiple languages to be translated to a
#' target language.
#' @param v A text vector
#' @param target_lang Which language would you like this translated to?
#' Please provide a two letter language abbreviation (e.g. "en" or "pt")
#' @param API Google API key.
#' For more information please go to:
#' https://cloud.google.com/translate/docs/setup
#' @importFrom cld2 detect_language
#' @importFrom translateR translate
#' @importFrom purrr map_chr
#' @return A character vector of the same length of original.
#' @export
str_translate <- function(v, target_lang, API) {
  if(missing(API)) {
    stop("Please declare a Google API key.
         For more information please go to: https://cloud.google.com/translate/docs/setup")
  }
  if(missing(target_lang)) {
    stop("Please declare a target language.
         Language should be decalred following ISO two letter codes.
         For more info, please go to: https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes")
  }
  out <- purrr::map(v, as.character)
  . <- NULL
  source_lang <- out %>%
    vapply(., purrr::map_chr, "", cld2::detect_language) %>%
    data.frame(check.names = FALSE)
  out <- cbind(out, source_lang)
  for (k in seq_len(nrow(out))) {
    if (is.na(out$.[k])) {
      out$out[k] == out$out[k]
      # print(paste0("Could not translate ", [k], ", language not detected."))
    } else {
      out$out[k] <- suppressWarnings(translateR::translate(content.vec = out$out[k],
                                                           google.api.key = API,
                                                           source.lang = out$.[k],
                                                           target.lang = target_lang))
    }
  }
  out <- out$out
}
