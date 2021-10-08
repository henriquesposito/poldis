#' Split text by speakers
#'
#' @param textvec text vector
#' @param splitsign sign or string used to split text vector.
#' Often these are line spaces ("/n") but depending on text different
#' or even string can be chosen (i.e. ",.")
#' @param speakermark speaker marker sign.
#' Often these are colon (":") but depending on text different
#' or even string can be chosen (i.e. "Thank you" or "I").
#' @import dplyr
#' @import stringr
#' @importFrom stats setNames aggregate
#' @return All text aggregated by speaker.
#' @source https://stackoverflow.com/questions/41100482/split-speaker-and-dialogue-in-rstudio
#' The function was altered from here.
#' @export
split_text <- function(textvec, splitsign, speakermark) {

  # initialize vectors
  . <- speaker <- text <- speakingGroup <- NULL

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
    lst <- stats::setNames(vector("list", length(plist)), plist)
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
  ss <- stats::aggregate(sp$fullText, list(sp$speaker), paste, collapse =" ")
  # return data frame without punctuations but aphostrophe
  ss
}
