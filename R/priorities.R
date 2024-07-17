#' Select future priorities from political discourses
#'
#' Political priorities are statements in which actors express their
#' intent or commitment to take political action in the future.
#' @param .data A (annotated) data frame or text vector.
#' For data frames, function will search for "text" variable.
#' For annotated data frames, please declare an annotated data frame
#' at the sentence level.
#' @param na.rm Would you like political statements that do not contain a
#' political action to be removed?
#' By default, TRUE.
#' @importFrom stringr str_detect str_remove_all
#' @importFrom dplyr mutate distinct %>%
#' @return A data frame with syntax information by sentences and
#' a variable identifying which of these sentences are priorities.
#' @export
select_priorities <- function(.data, na.rm = TRUE) {
  tags <- sentence <- lemmas <- priorities <- NULL
  if (inherits(.data, "data.frame")) {
    if ("token_id" %in% names(.data))
      stop("Please declare a text vector or an annotated data frame at the sentence level.")
  } else .data <- suppressMessages(annotate_text(.data, level = "sentences"))
  out <- .data %>%
    dplyr::mutate(lemmas = tolower(lemmas),
                  priorities = ifelse(stringr::str_detect(tags, "PRP MD ")|
                                      stringr::str_detect(lemmas,
                                                          "going to|go to |need to|ready to|
                                     |is time to|commit to|promise to|have to|
                                     |plan to|intend to|let 's|let us|urge|
                                     |require|want to"),
                                    paste(sentence), NA), # detect priorities
                  priorities = ifelse(stringr::str_detect(priorities, " not |
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
                                    NA, priorities)) %>%
    dplyr::distinct()
  if (isTRUE(na.rm)) out <- filter(out, !is.na(priorities))
  class(out) <- c("priorities", class(out))
  out
}
