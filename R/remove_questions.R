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
#'
#################### The below code works for news confernces, needs to be adapted for general use as above
#
# # For news conferences, cleaning is required before merging.
# load("~/GitHub/Poldis/data-raw/US_News_Conferences.rda")
# conf <- as.data.frame(US_News_Conferences)
#
# # Get observations since 1980
# conf$date <- lubridate::mdy(conf$date)
# conf <- conf %>% dplyr::filter(date > "1979-12-31")
#
# # Identifying questions and extracting only the when the
# # president speaks from news conferences transcripts. There are also other
# # speakers occasionaly whenever the press conference is shared with other leaders.
# # For now these are left as these do not appear to be the majority
# # of the cases and, often, the overwhelming majority of questions in these shared
# # confernecs are directed to the US president.
#
# dialogue_s <- function(x) {
#   dat <-  unlist(strsplit(x, "\n,"))
#   dat <- gsub(",Q.|Q.|\n,Q.", "QUT", dat)
#   qut <- grep("QUT", dat)
#   dat[qut] <- "SPLIT_HERE"
#   dialogue <- strsplit(dat[], "SPLIT_HERE")
#   dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
#   x <- dialogue
#   x
# }
#
# dialogue_c <- function(x) {
#   dat <- unlist(strsplit(x, "\\?"))
#   dat <- gsub(",Q.|Q.|\n,Q.", "QNT QUT", dat)
#   dat <- unlist(strsplit(dat, "QNT"))
#   qut <- grep("QUT", dat)
#   dat[qut] <- "SPLIT_HERE"
#   dialogue <- strsplit(dat[], "SPLIT_HERE")
#   dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
#   x <- dialogue
#   x
# }
#
# ctext = data.frame()
#
# for (i in 1:length(conf$text)) {
#   d <- as.character(conf$text[i])
#   if(stringr::str_detect(d, "\n,")) {
#     dialog <- dialogue_s(d)
#   } else if (!stringr::str_detect(d, "\n,")) {
#     dialog <- dialogue_c(d)
#   }
#   ctext = rbind(ctext, data.frame(dialog, stringsAsFactors = FALSE))
#   print(paste("Row:", i))
# }
#
# conf <- cbind(conf, ctext)
#
# summary(conf)

# #tofix: should the interviewer be identified from the text and then his sentences removed? Maybe if first sentence does not contain
# # "the president:" or the name of a president followed by ":"...
# dialogue_intern <- function(x) {
#   dat <-  unlist(strsplit(x, "\n,"))
#   interviewer <-  sub("([a-z0-9][?!.]).*", "\\1", x)
#   if(stringr::str_detect(interviewer, ":")) {
#     interviewer <- paste0(stringr::str_split(interviewer, ":")[[1]][1])
#     dat <- gsub(interviewer, "QUT", dat)
#   } else {
#     dat <- gsub(",Q.|Q.|\n,Q.", "QUT", dat)
#   }
#   qut <- grep("\\<QUT\\>", dat)
#   dat[qut] <- "SPLIT_HERE"
#   dialogue <- strsplit(dat[], "SPLIT_HERE")
#   dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
#   x <- dialogue
#   x
# }
#
# #tofix not working properly
# dialogue_inter <- function(x) {
#   dat <-  unlist(strsplit(x, "\\?"))
#   interviewer <-  sub("([a-z0-9][?!.]).*", "\\1", x)
#   if(stringr::str_detect(interviewer, ":")) {
#     interviewer <- paste0(stringr::str_split(interviewer, ":")[[1]][1])
#     dat <- gsub(interviewer, "QNT QUT", dat)
#   } else {
#     dat <- gsub(",Q.|Q.|\n,Q.", "QNT QUT", dat)
#   }
#   qut <- grep("QUT", dat)
#   dat[qut] <- "SPLIT_HERE"
#   dialogue <- strsplit(dat[], "SPLIT_HERE")
#   dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
#   x <- dialogue
#   x
# }
#
# itext = data.frame()
#
# for (i in 1:length(inter$text)) {
#   d <- as.character(inter$text[i])
#   if(stringr::str_detect(d, "\n,")) {
#     dialog <- dialogue_intern(d)
#   } else if (!stringr::str_detect(d, "\n,")) {
#     dialog <- dialogue_inter(d)
#   }
#   itext = rbind(itext, data.frame(dialog, stringsAsFactors = FALSE))
#   print(paste("Row:", i))
# }
#
# inter <- cbind(inter, itext)
# summary(inter)
#
# # Join datasets
# interviews <- dplyr::full_join(inter, conf)
