#' #' Remove Questions from dialogues
#' #'
#' #' @param datavar please declare dataset and variable
#' #' @param indicator what verctor indicates the question?
#' #' For more than one please use regex style for multiple indicators
#' #' (i.e. separated by | ) and/or word boundaries.
#' #' @return
#' #' @examples
#' #' remove_question(conf$text, indicator = ",Q.|Q.|\n,Q.")
#' #' @export
#' remove_question <- function(datavar, indicator) {
#'
#'   x <- datavar
#'
#'   dialogue <- function(d) {
#'     dat <-  unlist(strsplit(d, "\n,"))
#'     dat <- gsub(indicator, "QUT", dat)
#'     qut <- grep("\\<QUT\\>", dat)
#'     dat[qut] <- "SPLIT_HERE"
#'     dialogue <- strsplit(dat[], "SPLIT_HERE")
#'     dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
#'     d <- dialogue
#'     d
#'   }
#'
#'   t <- data.frame()
#'
#'   for (i in 1:length(x)) {
#'     dialog <- dialogue(as.character(x[i]))
#'     dialog <- rbind(t, data.frame(t, stringsAsFactors = FALSE))
#'     print(paste("Row:", i))
#'   }
#'
#'   t
#'
#' }
