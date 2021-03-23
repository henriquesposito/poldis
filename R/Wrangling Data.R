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

### Todo: extract setting from title and append to data
# This should also become a function later on
usethis::use_data(US_oral)

# For interviews and new conferences, cleaning is required before merging.
conf <- as.data.frame(US_News_Conferences)
interv <- as.data.frame(US_Interviews)

# Identifying questions and extracting only the when the
# president speaks from news conferences transcripts. There are also other
# speakers occasionaly whenever the press conference is shared with other leaders, bureaucrats
# or celebrities. For now these are left as these do not appear to be the majority
# of the cases and, often, the overwhelming majority of questions in these shared
# confernecs are directed to the US president.
### todo: create a function for removing questions of text
dat <- unlist(strsplit(conf$text, "\n,"))
dat <- gsub(",Q.|Q.", "QUT", dat)
qut <- grep("\\<QUT\\>", dat)
dat[qut] <- "SPLIT_HERE"
dialogue <- strsplit(dat[], "SPLIT_HERE")
dialogue <- stringr::str_squish(paste(dialogue, collapse = " "))
conf$textc <- dialogue

# Adding Setting
