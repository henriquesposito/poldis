# Data Preparation for US discursive data
# Henrique Sposito

## Oral Remarks

# Load some packages

library(dplyr)
library(usethis)
library(stringr)

# Loading oral communication datasets, miscelaneous and oral
load("~/GitHub/Poldis/data-raw/US_Oral_Remarks.rda")
remarks <- as.data.frame(US_Oral_Remarks)
load("~/GitHub/Poldis/data-raw/US_M_Remarks.rda")
mremarks <- as.data.frame(US_M_Remarks)

# Joining both datasets
oralc <- dplyr::full_join(mremarks, remarks)

# Remove title duplicates
US_oral <- dplyr::distinct(oralc, title, .keep_all = TRUE)

# Get observations since 1981 (starts from Reagan)
US_oral$date <- lubridate::mdy(US_oral$date)
US_oral <- US_oral %>% dplyr::filter(date > "1980-12-31")

# Let's take a look at speakers to see if they are presidents only
unique(US_oral$speaker)
sum(grepl("Melania Trump|Kamala Harris|Jill Biden|Mike Pence|Michelle Obama|Richard B. Cheney|Laura Bush|Jimmy Carter", US_oral$speaker))
# 1808 oral remarks we are not interested in from the 16318

# Let's remove oral remarks given by first ladies and vice-presidents from dataset
US_oral <- data.frame(US_oral[grep("Joseph R. Biden|Donald J. Trump|Barack Obama|George W. Bush|William J. Clinton|George Bush|Ronald Reagan", US_oral$speaker),])

# Import cleaned dataset
usethis::use_data(US_oral, overwrite = TRUE)

## Debates

# For debates we want to isolate all parts where certain candidate speaks into one observation for each candidate.
# But first, we need to get second round debates and not party primaries.
load("~/GitHub/Poldis/data-raw/US_Debates.rda")
US_Debates$title

# Let's keep only debates without "republican", "democrat" or "vice presidential" in title
US_debates <- data.frame(US_Debates[!grepl("republican|democratic|vice presidential|vice-presidential", US_Debates$title, ignore.case = TRUE),])
US_debates$title

# Get observations since 1980 (starts from Reagan)
US_debates$date <- lubridate::mdy(US_debates$date)
US_debates <- US_debates %>% dplyr::filter(date > "1980-01-01")
US_debates$title
# A few weird titles, let's investigate
US_debates[19,]
US_debates <- US_debates[-19,]
# This is a Democratic primary debate, should be removed
US_debates[9,]
US_debates <- US_debates[-9,]
# This is a republican debate and should be removed


# The scraping for participants showed an issue in naming participants and moderators for some observations
US_debates$participants
US_debates$moderator
# However, since the dataset as only 27 observation we can manually inpect each text to check for
# particiapants and extract them.

# Let's try and split strings in the first text column
strsplit(US_debates$text[1], ":")
# It appears that before every speaker talks, text is splited with ",.".
# Let's see if that holds for previous years
strsplit(US_debates$text[10], ",.")
# nope
strsplit(US_debates$text[20], ":")
# Let's try and see if spaces are present
strsplit(US_debates$text[20], "\n")
# Not present and this present an issue for extracting speakers programatically for the lack of consistency.
US_debates$text[1] # ",TRUMP:" and ",BIDEN:"
US_debates$text[2] # ",TRUMP:" and ",BIDEN:"
US_debates$text[3] # ",CLINTON:" and ",TRUMP:"
US_debates$text[4] # ",CLINTON:" and ",TRUMP:"
US_debates$text[5] # ",CLINTON:" and ",TRUMP:"
US_debates$text[6] # ",Gov. Romney." and ",The President." (Obama)
US_debates$text[7] # ",Gov. Romney." and ",The President." (Obama)
US_debates$text[8] # ",Gov. Romney." and ",The President." (Obama)
US_debates$text[9] # ",OBAMA:" and ",MCCAIN:"
US_debates$text[10] # ",OBAMA:" and ",MCCAIN:"
US_debates$text[11] # ",OBAMA:" and ",MCCAIN:"
US_debates$text[12] # ",Senator Kerry." and ",President Bush."
US_debates$text[13] # ",President Bush." and ",Senator Kerry."
US_debates$text[14] # ",President Bush." and ",Senator Kerry."
US_debates$text[15] # ",BUSH:" and ",GORE:"
US_debates$text[16] # ",BUSH:" and ",GORE:"
US_debates$text[17] # ",BUSH:" and ",GORE:"
US_debates$text[18] # ",The President." (Clinton) and ",Senator Dole."
US_debates$text[19] # # ",The President." (Clinton) and ",Senator Dole."
US_debates$text[20] # ",Mr. Perot." and ",Governor Clinton." and ",President Bush."
US_debates$text[21] # ",Mr. Perot." and ",Governor Clinton." and ",President Bush."
US_debates$text[22] # ",Mr. Perot." and ",Governor Clinton." and ",President Bush."
US_debates$text[23] # ",DUKAKIS:" and ",BUSH:"
US_debates$text[24] # ",DUKAKIS:" and ",BUSH:"
US_debates$text[25] # ",DUKAKIS:" and ",BUSH:"
US_debates$text[26] # ",Mr. Mondale." and ",The President." (Reagan)
US_debates$text[27] # ",THE PRESIDENT." (Carter) and ",GOVERNOR REAGAN."
US_debates$text[28] # ",ANDERSON:" (indepenedent candidate) and ",REAGAN:" (Should this be dropped?)

# Now that we know names we still ned to separate debates by speaker for analysis.
# However, because of uniqueness of how speakers at debates were coded in text,
# we have to do it text by text.
# The function was altered from:
# https://stackoverflow.com/questions/41100482/split-speaker-and-dialogue-in-rstudio
# Two versions are develop, one for the texts with ":" and the other for the debates without.
# Colon
splitText <- strsplit(US_debates$text[1], ",.")
allSpeakers <- lapply(splitText, function(thisText){
  grep(":", thisText, value = TRUE) %>%
    gsub(":.*", "", .) %>%
    gsub("\\(", "", .)
}) %>%
  unlist() %>%
  unique()

allSpeakers

legitSpeakers <-
  allSpeakers[-c(1,2,4,6,7)]

speechText <- lapply(splitText, function(thisText){

  # Remove applause and interjections (things in parentheses)
  # along with any blank lines; though you could leave blanks if you want
  cleanText <-
    grep("(^\\(.*\\)$)|(^$)", thisText
         , value = TRUE, invert = TRUE)

  # Split each line by a semicolor
  strsplit(cleanText, ":") %>%
    lapply(function(x){
      # Check if the first element is a legit speaker
      if(x[1] %in% legitSpeakers){
        # If so, set the speaker, and put the statement in a separate portion
        # taking care to re-collapse any breaks caused by additional colons
        out <- data.frame(speaker = x[1]
                          , text = paste(x[-1], collapse = ":"))
      } else{
        # If not a legit speaker, set speaker to NA and reset text as above
        out <- data.frame(speaker = NA
                          , text = paste(x, collapse = ":"))
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
    summarise(speaker = speaker[1]
              , fullText = paste(text, collapse = "\n"))
})

sp <- as.data.frame(speechText)

# Let's aggregate by speaker and see
sp <- aggregate(sp$fullText, list(sp$speaker), paste, collapse =" ")
sp$x

# # No colon
# splitText <- strsplit(US_debates$text[1], ",.")
# allSpeakers <- lapply(splitText, function(thisText){
#   grep(":", thisText, value = TRUE) %>%
#     gsub("Thank.*", "", .) %>% # word before thank you
#     gsub("\\(", "", .)
# }) %>%
#   unlist() %>%
#   unique()
#
# allSpeakers
#
# legitSpeakers <-
#   allSpeakers[-c(1,2,4,6,7)]
#
# speechText <- lapply(splitText, function(thisText){
#
#   # Remove applause and interjections (things in parentheses)
#   # along with any blank lines; though you could leave blanks if you want
#   cleanText <-
#     grep("(^\\(.*\\)$)|(^$)", thisText
#          , value = TRUE, invert = TRUE)
#
#   # Split each line by a semicolor
#   strsplit(cleanText, ":") %>%
#     lapply(function(x){
#       # Check if the first element is a legit speaker
#       if(x[1] %in% legitSpeakers){
#         # If so, set the speaker, and put the statement in a separate portion
#         # taking care to re-collapse any breaks caused by additional colons
#         out <- data.frame(speaker = x[1]
#                           , text = paste(x[-1], collapse = ":"))
#       } else{
#         # If not a legit speaker, set speaker to NA and reset text as above
#         out <- data.frame(speaker = NA
#                           , text = paste(x, collapse = ":"))
#       }
#       # Return whichever version we made above
#       return(out)
#     }) %>%
#     # Bind all of the rows together
#     bind_rows %>%
#     # Identify clusters of speech that go with a single speaker
#     mutate(speakingGroup = cumsum(!is.na(speaker))) %>%
#     # Group by those clusters
#     group_by(speakingGroup) %>%
#     # Collapse that speaking down into a single row
#     summarise(speaker = speaker[1]
#               , fullText = paste(text, collapse = "\n"))
# })

# Transform output into dataframe for each pf the 28 debates agregating text by speaker
sp1 <- as.data.frame(speechText)
sp1 <- aggregate(sp1$fullText, list(sp1$speaker), paste, collapse =" ")
sp1$x # "Biden first then Trump
debate1 <- data.frame(Title = c(paste(US_debates$title[1]), paste(US_debates$title[1])),
                      Date = c(paste(US_debates$date[1]), paste(US_debates$date[1])),
                      Speaker = c("Biden", "Trump"),
                      Text = c(paste(sp1$x[1]), paste(sp1$x[2])))

# re-run above and move to next
sp1 <- as.data.frame(speechText)
sp1 <- aggregate(sp1$fullText, list(sp1$speaker), paste, collapse =" ")
sp1$x # "Biden first then Trump
debate1 <- data.frame(Title = c(paste(US_debates$title[1]), paste(US_debates$title[1])),
                      Date = c(paste(US_debates$date[1]), paste(US_debates$date[1])),
                      Speaker = c("Biden", "Trump"),
                      Text = c(paste(sp1$x[1]), paste(sp1$x[2])))




## Interviews

# For interviews, filtering is also required before merging.
load("~/GitHub/Poldis/data-raw/US_Interviews.rda")
inter <- as.data.frame(US_Interviews)

# Get observations since 1981
inter$date <- lubridate::mdy(inter$date)
US_interviews <- inter %>% dplyr::filter(date > "1980-12-31")

# Remove title duplicates
US_interviews <- dplyr::distinct(US_interviews, .keep_all = TRUE)

# Importantly, we want to keep observations that match speakers for debates, that is, elected presidents and runner uppers.
# Let's take a look at speakers to see
unique(US_interviews$speaker)
# For some reason, only Obama and Trump are listed as presidents in the first time around...
# At this point data was re-scraped and issues of speakers was fixed (minor bug in code).

usethis::use_data(US_interviews, overwrite = TRUE)
# exporting clean data to data folder

## Campaign Remarks

# Campaign documents and debates also reuire some wrangling, besides filtering dates...
# We want to keep only discursive content and remove press conferences given by campaig staff as
# well as statements simply read by campaign staff.
# This entails removing observations that have "press release" or "campaign statement" in title.

load("~/GitHub/Poldis/data-raw/US_Campaign.rda")
camp <- as.data.frame(US_Campaign)

# Get observations since 1981
camp$date <- lubridate::mdy(camp$date)
camp <- camp %>% dplyr::filter(date > "1980-01-01")

# Take a look at speakers
unique(camp$speaker)
# All looks proper, but many speakers we do not care about...

# Investigate title
head(camp$title, 100)
# Most titles read "press release" or campaign statement, let's investigate
camp[1,]
camp[100,]
# maybe press releases are not as helpful
camp[21,]
camp[76,]


camp <- dplyr::if_else(str_detect(camp$title, "press release"), str_replace(camp$title, NA), x)
camp <- dplyr::if_else(str_detect(camp$title, "campaign statement"), str_replace(camp$title, NA), x)

camp <- dplyr::filter(camp$title != "NA")
