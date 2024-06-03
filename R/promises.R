#' Extract future promises from political discourses
#'
#' @param .data A (annotated) data frame or text vector.
#' For data frames, function will search for "text" variable.
#' For annotated data frames, please declare an annotated data frame
#' at the sentence level.
#' @importFrom stringr str_detect str_remove_all
#' @examples
#' \donttest{
#' extract_promises(US_News_Conferences_1960_1980[1:2,3])
#' }
#' @export
extract_promises <- function(.data) {
  tags <- sentence <- lemmas <- sentence_id <- doc_id <- promises <- NULL
  if (inherits(.data, "data.frame")) {
    if ("token_id" %in% names(.data))
      stop("Please declare a text vector or an annotated data frame at the sentence level.")
  } else .data <- suppressMessages(annotate_text(.data, level = "sentences"))
  out <- .data |>
    dplyr::mutate(lemmas = tolower(lemmas),
                  promises = ifelse(stringr::str_detect(tags, "PRP MD ")|
                                      stringr::str_detect(lemmas,
                                                          "going to|need to|ready to|
                                     |is time to|commit to|promise to|have to|
                                     |plan to|intend to|let 's|require|want to"),
                                    paste(sentence), NA), # detect promises
                  promises = ifelse(stringr::str_detect(promises, " not |
                                                        |yesterday|last week|
                                                        |last month|last year|
                                                        |prime minister|president|
                                                        |thank|honor|honour|
                                                        |applause|greet|laugh|
                                                        |privilege to|great to|
                                                        |good to") |
                                      stringr::str_detect(tags, "MD VB( RB)? VBN|
                                                         |VBD( RB)? VBN|VBZ( RB)? VBN|
                                                         |VBD( RB)? JJ|PRP( RB)? VBD TO"),
                                    NA, promises)) |>
    dplyr::distinct()
  class(out) <- c("promises", class(out))
  out
}
