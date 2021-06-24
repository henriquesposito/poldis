#' Split text by speakers
#'
#' @param textvec text vector
#' @param splitsign sign or string used to split text vector.
#' Often these are line spaces ("\n") but depending on text different
#' or even string can be chosen (i.e. ",.")
#' @param speakermark speaker marker sign.
#' Often these are colon (":") but depending on text different
#' or even string can be chosen (i.e. "Thank you" or "I").
#' @import dplyr
#' @import stringr
#' @return All text aggregated by speaker.
#' @source https://stackoverflow.com/questions/41100482/split-speaker-and-dialogue-in-rstudio
#' The function was altered from here.
#' @export
split_text <- function(textvec, splitsign, speakermark) {

  # split text with split sign
  splitText <- strsplit(textvec, paste0(splitsign))

  # get speakers by marker sign
  allSpeakers <- lapply(splitText, function(thisText){
    grep(paste0(speakermark), thisText, value = TRUE) %>%
      gsub(paste0(speakermark, ".*"), "", .) %>%
      gsub("\\(", "", .)
  }) %>%
    unlist() %>%
    unique()

  # Check lengths and reduce sizes of strings to avoid warnings
  allSpeakers <- ifelse(lengths(gregexpr("\\W+", allSpeakers)) > 5, substr(allSpeakers, 0, 20), allSpeakers)

  # initialize data frame
  notlegit <- data.frame()

  # get non legit speakers by asking users
  for (i in 1:length(allSpeakers)) {
    if (utils::askYesNo(paste(allSpeakers[i], "-", "is this speaker legit?")) == FALSE) {
      nlegit <- paste(i)
      notlegit = rbind(notlegit, nlegit)
    }
  }

  # if all speakers are legit
  if(nrow(notlegit) == 0) {
    legitSpeakers <- allSpeakers
  } else {
    # tranform in numbers column
    names(notlegit)[1] <- 'num'
    # get the equivalent from allSpeakers vector
    plist <- unique(notlegit$num)
    lst <- setNames(vector("list", length(plist)), plist)
    nn <- data.frame()
    for (i in seq_along(plist)) {
      n <- paste0(allSpeakers[i])
      nn = rbind(nn, n)
    }

    # rename column and bind data frames
    colnames(nn)[1] <- "speakers"
    nn <- cbind(nn, notlegit)

    # get the same dataset for all speakers
    ss <- data.frame(speakers = allSpeakers, num = as.character(1:length(allSpeakers)))
    # perform and anti_join
    lspeakers <- dplyr::anti_join(ss, nn, by = 'num')
    legitSpeakers <- lspeakers$speakers
  }

  # get text
  speechText <- lapply(splitText, function(thisText){

  # Remove applause and interjections (things in parentheses)
  cleanText <- grep("(^\\(.*\\)$)|(^$)", thisText, value = TRUE, invert = TRUE)

  # Split each line by a semicolor
  strsplit(cleanText, paste(speakermark)) %>%
    lapply(function(x){
      # Check if the first element is a legit speaker
      if(x[1] %in% legitSpeakers){
        # If so, set the speaker, and put the statement in a separate portion
        # taking care to re-collapse any breaks caused by additional colons
         out <- data.frame(speaker = x[1], text = paste(x[-1], collapse = paste(speakermark)))
      } else{
        # If not a legit speaker, set speaker to NA and reset text as above
        out <- data.frame(speaker = NA, text = paste(x, collapse = ":"))
      }
      # Return whichever version we made above
      return(out)
      }) %>%
      # Bind all of the rows together
      bind_rows %>%
      # Identify clusters of speech that go with a single speaker
      mutate(speakingGroup = cumsum(!is.na(speaker))) %>%
      # Group by those clusters
      group_by(speakingGroup) %>%
      # Collapse that speaking down into a single row
      summarise(speaker = speaker[1], fullText = paste(text, collapse = "\n"))
  })
  # Get into data frame
  sp <- as.data.frame(speechText)
  # Aggregate by speaker and see
  ss <- aggregate(sp$fullText, list(sp$speaker), paste, collapse =" ")
  # return data frame without punctuations but aphostrophe
  ss
}

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

### Todo: extract setting (country, region, context) and subject (topic) from title
# This should also become a function later on

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

### todo: create a function that removes undesired observations that contain certain words in title (remove_obs())

# remove_obs<- function(arg){
#
#   arg <- lapply(camp$title, function(x) {
#     dplyr::if_else(str_detect(x, "press release"), str_replace(x, NA), x)
#     dplyr::if_else(str_detect(x, "campaign statement"), str_replace(x, NA), x)
#   })
#
#   unlist(arg)
#
#   arg <- dplyr::filter(arg != "NA")
# }
