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

# Let's keep only debates without "republican", "democrat" or "vice" in title
US_debates <- data.frame(US_Debates[!grepl("republican|democratic|vice", US_Debates$title, ignore.case = TRUE),])
US_debates$title

# Get observations since 1980 (starts from Reagan)
US_debates$date <- lubridate::mdy(US_debates$date)
US_debates <- US_debates %>% dplyr::filter(date > "1980-01-01")
US_debates$title
# A few weird titles, let's investigate
US_debates[19,]
# This is a Democratic primary debate, should be removed
US_debates[9,]
# This is a republican debate and should be removed
US_debates <- US_debates[c(-19, 9),]

# The scraping for participants showed an issue in naming participants and moderators for some observations
US_debates$participants
US_debates$moderator
# However, since the dataset as only 26 observation we can manually inpect each text to check for
# particiapants and extract them



## Interviews

# For interviews, filtering is also required before merging.
load("~/GitHub/Poldis/data-raw/US_Interviews.rda")
inter <- as.data.frame(US_Interviews)

# Get observations since 1981
inter$date <- lubridate::mdy(inter$date)
US_interviews <- inter %>% dplyr::filter(date > "1980-12-31")

# Remove title duplicates
US_interviews <- dplyr::distinct(interviews, title, .keep_all = TRUE)

# Importantly, we want to keep observations that match speakers for debates, that is, elected presidents and runner uppers.
# Let's take a look at speakers to see
unique(US_interviews$speaker)

# Let's keep only speakers we are interested about
US_interviews <- US_interviews %>% dplyr::filter(speaker = c("Joseph R. Biden", "Donald J. Trump", "Barack Obama", "George W. Bush" , "William J. Clinton", "George Bush", "Ronald Reagan"))


## Campaign Remarks

# Campaign documents and debates also reuire some wrangling, besides filtering dates...
# We want to keep only discursive content and remove press conferences given by campaig staff as
# well as statements simply read by campaign staff.
# This entails removing observations that have "press release" or "campaign statement" in title.

load("~/GitHub/Poldis/data-raw/US_Campaign.rda")
camp <- as.data.frame(US_Campaign)

# Filter for "1980-12-31"

camp <- dplyr::if_else(str_detect(camp$title, "press release"), str_replace(camp$title, NA), x)
camp <- dplyr::if_else(str_detect(camp$title, "campaign statement"), str_replace(camp$title, NA), x)

camp <- dplyr::filter(camp$title != "NA")
