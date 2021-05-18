# Merging and Cleaning Raw Datasets

library(dplyr)
library(usethis)
library(stringr)

# Merging oral communication datasets with possible duplicates
load("~/GitHub/Poldis/data-raw/US_Oral_Remarks.rda")
remarks <- as.data.frame(US_Oral_Remarks)
load("~/GitHub/Poldis/data-raw/US_M_Remarks.rda")
mremarks <- as.data.frame(US_M_Remarks)

# Join datasets
oralc <- dplyr::full_join(mremarks, remarks)

# Remove title duplicates
US_oral <- dplyr::distinct(oralc, title, .keep_all = TRUE)

# Get observations since 1980
US_oral$date <- lubridate::mdy(US_oral$date)
US_oral <- US_oral %>% dplyr::filter(date > "1979-12-31")

# Import cleaned dataset
usethis::use_data(US_oral, overwrite = TRUE)

# For Brazil, at the time being, Cesar 2020 dataset on oral remarks is being used.
# The data is scraped from the presidential library and covers all official presidential speeches
# and remarks from January 1985 to July 2020.

load("~/GitHub/Poldis/data/BR_oral.rda")
summary(BR_oral)

### Todo: extract setting from title
# This should also become a function later on

# For news conferences, cleaning is required before merging.
load("~/GitHub/Poldis/data-raw/US_News_Conferences.rda")
conf <- as.data.frame(US_News_Conferences)

# Get observations since 1980
conf$date <- lubridate::mdy(conf$date)
conf <- conf %>% dplyr::filter(date > "1979-12-31")

# Identifying questions and extracting only the when the
# president speaks from news conferences transcripts. There are also other
# speakers occasionaly whenever the press conference is shared with other leaders.
# For now these are left as these do not appear to be the majority
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

summary(conf)

### todo: create a function based on the above for removing questions of dialogue text (remove_questions())

# For interviews, cleaning is also required before merging.
load("~/GitHub/Poldis/data-raw/US_Interviews.rda")
inter <- as.data.frame(US_Interviews)

# Get observations since 1980
inter$date <- lubridate::mdy(inter$date)
inter <- inter %>% dplyr::filter(date > "1979-12-31")

#tofix: should the interviewer be identified from the text and then his sentences removed? Maybe if first sentence does not contain
# "the president:" or the name of a president followed by ":"...
dialogue_intern <- function(x) {
  dat <-  unlist(strsplit(x, "\n,"))
  interviewer <-  sub("([a-z0-9][?!.]).*", "\\1", x)
  if(stringr::str_detect(interviewer, ":")) {
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

#tofix not working properly
dialogue_inter <- function(x) {
  dat <-  unlist(strsplit(x, "\\?"))
  interviewer <-  sub("([a-z0-9][?!.]).*", "\\1", x)
  if(stringr::str_detect(interviewer, ":")) {
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

inter <- cbind(inter, itext)
summary(inter)

# Join datasets
interviews <- dplyr::full_join(inter, conf)

# Remove title duplicates
US_interviews <- dplyr::distinct(interviews, title, .keep_all = TRUE)

# Get observations since 1980
US_interviews$date <- lubridate::mdy(US_oral$date)

US_interviews <- US_oral %>% dplyr::filter(date > "1979-12-31")

usethis::use_data(US_interviews, overwrite = TRUE)

# Campaign documents and debates also reuire some wrangling...
# First, for debates we want to isolate all parts where certain candidate speaks into one observation for each candidate.
# Second, we want to keep only discursive content and remove press conferences given by campaig staff as
# well as statements simply read by campaign staff.
# This entails removing observations that have "press release" or "campaign statement" in title.

load("~/GitHub/Poldis/data-raw/US_Campaign.rda")
camp <- as.data.frame(US_Campaign)

### todo: create a function that removes undesired observations that contain certain words in title (remove_obs())

remove_obs<- function(arg){

  arg <- lapply(camp$title, function(x) {
    dplyr::if_else(str_detect(x, "press release"), str_replace(x, NA), x)
    dplyr::if_else(str_detect(x, "campaign statement"), str_replace(x, NA), x)
  })

  unlist(arg)

  arg <- dplyr::filter(arg != "NA")
}
