#' Select future promises from political discourses
#'
#' Political promises are statements in which actors express their
#' intent or commitment to take political action in the future.
#' @param .data A (annotated) data frame or text vector.
#' For data frames, function will search for "text" variable.
#' For annotated data frames, please declare an annotated data frame
#' at the sentence level.
#' @importFrom stringr str_detect str_remove_all
#' @importFrom dplyr mutate distinct %>%
#' @examples
#' #select_promises(US_News_Conferences_1960_1980[1:2,3])
#' @export
select_promises <- function(.data) {
  tags <- sentence <- lemmas <- promises <- NULL
  if (inherits(.data, "data.frame")) {
    if ("token_id" %in% names(.data))
      stop("Please declare a text vector or an annotated data frame at the sentence level.")
  } else .data <- suppressMessages(annotate_text(.data, level = "sentences"))
  out <- .data %>%
    dplyr::mutate(lemmas = tolower(lemmas),
                  promises = ifelse(stringr::str_detect(tags, "PRP MD ")|
                                      stringr::str_detect(lemmas,
                                                          "going to|go to |need to|ready to|
                                     |is time to|commit to|promise to|have to|
                                     |plan to|intend to|let 's|let us|urge|
                                     |require|want to"),
                                    paste(sentence), NA), # detect promises
                  promises = ifelse(stringr::str_detect(promises, " not |
                                                        |yesterday|last week|
                                                        |last month|last year|
                                                        |thank|honor|honour|
                                                        |applause|greet|laugh|
                                                        |privilege to|great to|
                                                        |good to be|good to see") |
                                      stringr::str_detect(tags, "MD VB( RB)? VBN|
                                                         |VBD( RB)? VBN|VBZ( RB)? VBN|
                                                         |VBD( RB)? JJ|PRP( RB)? VBD TO|
                                                         |VBN( RB)? VBN"),
                                    # Combinations of NLP tags to select
                                    NA, promises)) %>%
    dplyr::distinct()
  class(out) <- c("promises", class(out))
  out
}
