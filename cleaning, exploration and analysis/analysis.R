# Analysis

# This is where the magic takes place...
# or not since you get to see everything ;D

# Packages
library(dplyr)
library(readr)
library(stringr)
library(stringi)

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

# Let's create a length and dates column for later on
BR$length <- nchar(BR$text)
BR$date <- stringr::str_extract(BR$doc_id, "[0-9]{4}")
US$length <- nchar(US$text)
US$date <- stringr::str_extract(US$doc_id, "[0-9]{4}")

# Load dictionaries and get frequencies for speaker-year
# Shall we start with the US?
# Dictionary of terms for authenticity performances for US

truth_telling <- ("the truth is|this is the truth|not lying|is honest|are honest|honesty|is sincere|are sincere|is true|are true|not a liar|bottom of my heart|I swear|I promise|reassure|I know|is evident”, “are evident”, “I am sure”, “trust me”, “frank”, “up-front”, “come clean”, “coming clean”, “is upfront”, “are upfront|is straightforward|are straightforward")

lie_accusations <- ("not truth|not true|untruthful|is lying|are lying|is a liar|are liars|is dishonest|are dishonest|dishonesty|is fake|are fake|is corrupt|are corrupt|full of lies|not sincere|not honest|is cheating|is a cheater|are cheaters|are tricking|is tricking|be deceived|is deceiving|are deceiving|are a hypocrite|is a hypocrite|is crooked|are crooked|is misleading|has double-standards|are sneaky|is sneaky|two-faced|you are wrong|not correct|fooled by")

consistency <- ("we delivered|check and see|keep my words|kept my word|keep promises|I am responsible|I take responsibility|we are accountable|we are responsible|our duty|my duty|give my word|giving my word|my word|own up my|owning up my|accept responsibility|accept the blame|recognize my mistakes|admit I was wrong")

finger_pointing <- ("inconsistent|irresponsible|their fault|not my fault|they left us with|are responsible|is responsible|costed us|false promises|lack accountability|lacking accountability|not kept|not recognize|he made mistakes|she made mistakes|they made mistakes|not take responsibility|not accountable|blame them|their blame")

origins <- ("I was born|I come from|grew up|my parents|my mom|my mother|my father|my dad|my family|raised|my background|being surrounded by|being exposed to|my siblings|going to school|our local church|Sunday mass|Saturday mass|family tradition|tradition in my house|growing up")

common_sense <- ("is common sense|are common sense|everyone knows|it is undeniable|stating the obvious|everyone agrees|we all know|wisdom|from experience|it is my experience|sound judgment|practical|pragmatic|down-to-earth|realistic|realistically|et me tell you about|is obvious|are obvious")

anti_pc<- ("politically correct|political correctness|PC|plain speaking|speaking my mind|speak my mind|say what I think|saying what I think|going to pretend|not pretend|speak what you think|what you want to hear|butter up|beat around the bush|cut to the chase")

territory <- ("have seen in|have been to|have visited|to be in|came all the way to|back from|was in|we were in|went to|will visit|saw first-hand|to see first-hand|walked around|walked in|people in|will be in|we visited|I visited|travelled to|spend a few days in|spent some time in|I lived|spent time|stayed in")

# Get each performance as frequencies per row
US$truth <- stringr::str_count(US$text, paste0("(?i)", truth_telling))
US$lies <- stringr::str_count(US$text, paste0("(?i)", lie_accusations))
US$consistency <- stringr::str_count(US$text, paste0("(?i)", consistency))
US$fpoint <- stringr::str_count(US$text, paste0("(?i)", finger_pointing))
US$origins <- stringr::str_count(US$text, paste0("(?i)", origins))
US$common_sense <- stringr::str_count(US$text, paste0("(?i)", common_sense))
US$anti_PC <- stringr::str_count(US$text, paste0("(?i)", anti_pc))
US$territory <- stringr::str_count(US$text, paste0("(?i)", territory))

# take a quick look at the findings
US_1985 <- US %>%  filter(date == 1985) %>% select(-text)
US_1985
US_1995 <- US %>%  filter(date == 1995) %>% select(-text)
US_1995
US_2005 <- US %>%  filter(date == 2005) %>% select(-text)
US_2005
US_2015 <- US %>%  filter(date == 2015) %>% select(-text)
US_2015

truth_time <- US %>% select(truth, date, setting, length) # Get obs per year and plot
