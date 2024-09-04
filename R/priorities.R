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
#' @examples
#' #select_priorities(US_News_Conferences_1960_1980[1:2,3])
#' @export
select_priorities <- function(.data, na.rm = TRUE) {
  tags <- sentence <- lemmas <- priorities <- NULL
  if (inherits(.data, "data.frame")) {
    if ("token_id" %in% names(.data))
      stop("Please declare a text vector or an annotated data frame at the sentence level.")
  } else .data <- suppressMessages(annotate_text(.data, level = "sentences"))
  out <- .data %>%
    dplyr::mutate(priorities = ifelse(stringr::str_detect(tags, "PRP MD ")|
                                        stringr::str_detect(lemmas, paste0(
                                          textstem::lemmatize_strings(
                                            Commitment$word[which(Commitment$grammar_function != "adjective")]),
                                          collapse = "|")),
                                      lemmas, NA), # detect priorities
                  priorities = ifelse(stringr::str_detect(priorities, " not | never ") |
                                        stringr::str_detect(tags,
                                        "MD VB( RB)? VBN|VBD( RB)? VBN|VBZ( RB)? VBN|
                                        |VBD( RB)? JJ|PRP( RB)? VBD TO|VBN( RB)? VBN"),
                  NA, sentence)) %>%
    dplyr::distinct()
  if (isTRUE(na.rm)) out <- filter(out, !is.na(priorities))
  class(out) <- c("priorities", class(out))
  out
}
