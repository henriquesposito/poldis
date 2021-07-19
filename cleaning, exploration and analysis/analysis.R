# Analysis

# This is where the magic takes place...
# or not since you get to see everything ;D

# Packages
library(dplyr)
library(readr)
library(stringr)

# Bind all data by speaker and year to create one big dataset for each case

# US
# Speeches
load("~/GitHub/Poldis/data/US_oral.rda")
uoral <- US_oral %>% select(speaker, date, text)
uoral$speaker <- paste0(uoral$speaker, "_", stringr::str_extract_all(uoral$date, "^[0-9]{4}")) # speaker year
uoral <- aggregate(uoral$text, list(uoral$speaker), paste, collapse = " ")
uoral <- rename(uoral, doc_id = "Group.1", text = "x")
uoral$setting <- "speeches"

# Campaign
load("~/GitHub/Poldis/data/US_campaign.rda")
ucamp <- US_campaign %>% select(speaker, date, text)
ucamp$speaker <- paste0(ucamp$speaker, "_", stringr::str_extract_all(ucamp$date, "^[0-9]{4}")) # speaker year
ucamp <- aggregate(ucamp$text, list(ucamp$speaker), paste, collapse = " ")
ucamp <- rename(ucamp, doc_id = "Group.1", text = "x")
ucamp$setting <- "campaign"

# Debates
load("~/GitHub/Poldis/data/US_debates.rda")
udeb <- US_debates %>% select(Speakers, Date, Text)
udeb$speaker <- paste0(udeb$Speakers, "_", stringr::str_extract_all(udeb$Date, "^[0-9]{4}")) # speaker year
udeb <- aggregate(udeb$Text, list(udeb$speaker), paste, collapse = " ")
udeb <- rename(udeb, doc_id = "Group.1", text = "x")
udeb$setting <- "debates"

# Interview
load("~/GitHub/Poldis/data/US_interviews.rda")
uint <- US_interviews %>% select(speaker, date, text)
uint$speaker <- paste0(uint$speaker, "_", stringr::str_extract_all(uint$date, "^[0-9]{4}")) # speaker year
uint <- aggregate(uint$text, list(uint$speaker), paste, collapse = " ")
uint <- rename(uint, doc_id = "Group.1", text = "x")
uint$setting <- "interviews"

# Bind
US <- rbind (uoral, ucamp, udeb, uint)

# Brazil
# Speeches
load("~/GitHub/Poldis/data/BR_oral.rda")
boral <- BR_oral %>% select(date, presid, text)
boral$Speaker <- paste0(boral$presid, "_", boral$date) # get speaker year
boral <- aggregate(boral$text, list(boral$Speaker), paste, collapse =" ")
boral <- rename(boral, doc_id = "Group.1", text = "x")
boral$setting <- "speeches"

# Campaign
load("~/GitHub/Poldis/data/BR_campaign.rda")
bcamp <- BR_Campaign %>% select(Speaker, Date, Text)
bcamp$Speaker <- paste0(bcamp$Speaker, "_", bcamp$Date) # get speaker year
bcamp <- aggregate(bcamp$Text, list(bcamp$Speaker), paste, collapse = " ")
bcamp <- rename(bcamp, doc_id = "Group.1", text = "x")
bcamp$setting <- "campaign"

# Debates
load("~/GitHub/Poldis/data/BR_debates.rda")
bdeb <- BR_debates %>% select(Speaker, Date, Text)
bdeb$Speaker <- paste0(bdeb$Speaker, "_", bdeb$Date) # get speaker year
bdeb <- aggregate(bdeb$Text, list(bdeb$Speaker), paste, collapse = " ")
bdeb <- rename(bdeb, doc_id = "Group.1", text = "x")
bdeb$setting <- "debates"

# Interviews
load("~/GitHub/Poldis/data/BR_interviews.rda")
bint <- BR_Interviews %>% select(Speaker, Date, Text)
bint$Speaker <- paste0(bint$Speaker, "_", bint$Date) # get speaker year
bint <- aggregate(bint$Text, list(bint$Speaker), paste, collapse = " ")
bint <- rename(bint, doc_id = "Group.1", text = "x")
bint$setting <- "interviews"

# Bind
BR <- rbind(boral, bcamp, bdeb, bint)

# some simple cleaning (this took a few hours)
# remove all punctuations. Because this takes a while
# Rds versions of this data can be found in the data folder.
# They are called US.Rds and BR.Rds. You can load these if
# you do not want to run the following lines (90-96).
textus <- purrr::map(US$text, as.character)
textus <- gsub("[[:punct:]]", "", textus, perl=TRUE)
US$text <- textus
textbr <- purrr::map(BR$text, as.character)
textbr <- gsub("[[:punct:]]", "", textbr, perl=TRUE)
BR$text <- textbr

# load dictionaries and get frequencies for speaker-year
