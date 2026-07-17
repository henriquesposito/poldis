#' Select future priorities from political discourses
#'
#' Political priorities are statements in which actors express their
#' intent or commitment to take political action in the future.
#' @param v Text vector or annotated data frame.
#' @param na.rm Would you like political statements that do not contain a
#' political action to be removed?
#' By default, TRUE.
#' @importFrom stringr str_detect str_remove_all
#' @importFrom dplyr mutate distinct %>%
#' @return A data frame with syntax information by sentences and
#' a variable identifying which of these sentences are priorities.
#' @examples
#' #select_priorities(US_inaugural_addresses_1993_2025$text)
#' @export
select_priorities <- function(v, na.rm = TRUE) {
  tags <- sentence <- lemmas <- priorities <- comm_lemmas <- NULL
  if (inherits(v, "data.frame") & !"token_id" %in% names(v)) {
    stop("Please declare a text vector or an annotated data frame at the sentence level.")
  } else if (!"token_id" %in% names(v)) {
    v <- annotate_text(v, level = "sentences")
  }
  comm_lemmas <- unique(textstem::lemmatize_strings(BT_models$terms[which(
    BT_models$dimension == "commitment")]))
  out <- v %>%
    dplyr::mutate(priorities = ifelse(stringr::str_detect(tags, "PRP MD ") |
                                        stringr::str_detect(lemmas, paste0(
                                          comm_lemmas, collapse = "|")),
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
