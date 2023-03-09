urgency <- function(text, dictionary, context = "2 sentences",
                    metadata = NULL, language = "English", keep.all = TRUE) {
  if (isFALSE(keep.all)) {
    text <- extract_match(text, dictionary)
  }
  text <- extract_context(text, dictionary, context)
}
