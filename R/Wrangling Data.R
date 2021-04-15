# Merging and Cleaning Raw Datasets

library(dplyr)
library(usethis)
library(stringr)

# Merging oral communication datasets with possible duplicates
remarks <- as.data.frame(US_Oral_Remarks)
mremarks <- as.data.frame(US_M_Remarks)

# Join datasets
oralc <- dplyr::full_join(mremarks, remarks)

# Remove title duplicates
US_oral <- dplyr::distinct(oralc, title, .keep_all = TRUE)

# Get observations since 1980
US_oral$date <- lubridate::mdy(US_oral$date)

US_oral <- US_oral %>% dplyr::filter(date > "1979-12-31")

usethis::use_data(US_oral, overwrite = TRUE)

# For Brazil, at the time being, Cesar 2020 dataset on oral remarks is being used for the time being.
# The data is scraped from the presidential library and covers all official presidential speeches
# and remarks from 1985 to 2020.

summary(BR_oral)

### Todo: extract setting from title and append to data
# This should also become a function later on
# Extract setting for tittle
# (inspired by code_agreements() perhaps should cite it)

usethis::use_data(US_oral)

# For news conferences, cleaning is required before merging.
conf <- as.data.frame(US_News_Conferences)

# Identifying questions and extracting only the when the
# president speaks from news conferences transcripts. There are also other
# speakers occasionaly whenever the press conference is shared with other leaders, bureaucrats
# or celebrities. For now these are left as these do not appear to be the majority
# of the cases and, often, the overwhelming majority of questions in these shared
# confernecs are directed to the US president.

dialogue_s <- function(x) {
  dat <-  unlist(strsplit(x, "\n,"))
  dat <- gsub(",Q.|Q.|\n,Q.", "QUT", dat)
  qut <- grep("QUT", dat)
  dat[qut] <- "SPLIT_HERE"
  dialogue <- strsplit(dat[], "SPLIT_HERE")
  dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
  x <- dialogue
  x
}

dialogue_c <- function(x) {
  dat <- unlist(strsplit(x, "\\?"))
  dat <- gsub(",Q.|Q.|\n,Q.", "QNT QUT", dat)
  dat <- unlist(strsplit(dat, "QNT"))
  qut <- grep("QUT", dat)
  dat[qut] <- "SPLIT_HERE"
  dialogue <- strsplit(dat[], "SPLIT_HERE")
  dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
  x <- dialogue
  x
}

ctext = data.frame()

for (i in 1:length(conf$text)) {
  d <- as.character(conf$text[i])
  if(stringr::str_detect(d, "\n,")) {
    dialog <- dialogue_s(d)
  } else if (!stringr::str_detect(d, "\n,")) {
    dialog <- dialogue_c(d)
  }
  ctext = rbind(ctext, data.frame(dialog, stringsAsFactors = FALSE))
  print(paste("Row:", i))
}

conf <- cbind(conf, ctext)

### todo: create a function based on the above for removing questions of dialogue text
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

# For interviews, cleaning is required before merging.
inter <- as.data.frame(US_Interviews)
inter <- inter %>% slice_head(n=2)
x = inter$text[1]

#tofix
dialogue_intern <- function(x) {
  dat <-  unlist(strsplit(x, "\n,"))
  interviewer <-  sub("([a-z0-9][?!.]).*", "\\1", x)
  if(str_detect(interviewer, ":")) {
    interviewer <- paste0(stringr::str_split(interviewer, ":")[[1]][1])
    dat <- gsub(interviewer, "QUT", dat)
  } else {
    dat <- gsub(",Q.|Q.|\n,Q.", "QUT", dat)
  }
  qut <- grep("\\<QUT\\>", dat)
  dat[qut] <- "SPLIT_HERE"
  dialogue <- strsplit(dat[], "SPLIT_HERE")
  dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
  x <- dialogue
  x
}

#tofix
dialogue_inter <- function(x) {
  dat <-  unlist(strsplit(x, "\\?"))
  interviewer <-  sub("([a-z0-9][?!.]).*", "\\1", x)
  if(str_detect(interviewer, ":")) {
    interviewer <- paste0(stringr::str_split(interviewer, ":")[[1]][1])
    dat <- gsub(interviewer, "QNT QUT", dat)
  } else {
    dat <- gsub(",Q.|Q.|\n,Q.", "QNT QUT", dat)
  }
  qut <- grep("QUT", dat)
  dat[qut] <- "SPLIT_HERE"
  dialogue <- strsplit(dat[], "SPLIT_HERE")
  dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
  x <- dialogue
  x
}

itext = data.frame()

for (i in 1:length(inter$text)) {
  d <- as.character(inter$text[i])
  if(stringr::str_detect(d, "\n,")) {
    dialog <- dialogue_intern(d)
  } else if (!stringr::str_detect(d, "\n,")) {
    dialog <- dialogue_inter(d)
  }
  itext = rbind(itext, data.frame(dialog, stringsAsFactors = FALSE))
  print(paste("Row:", i))
}

inter <- cbind(conf, itext)
