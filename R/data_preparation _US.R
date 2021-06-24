# Data Preparation for US discursive data
# Henrique Sposito

# Load some packages
library(dplyr)
library(usethis)
library(stringr)

### Oral Remarks

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

### Interviews

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
# Note that no programatic cleaning is done for interviews for two reasons,
# first intreviewers usually talk little and, second, doing so programatically risks losing
# some importnat portions for interviewees.
usethis::use_data(US_interviews, overwrite = TRUE)
# exporting clean data to data folder

### Debates

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

# Because of the inconsistencies, but since dataset is small,
# let's just inspect each text observation and copy the speakers
# to a respective debate.
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
US_debates$text[19] # ",The President." (Clinton) and ",Senator Dole."
US_debates$text[20] # ",Mr. Perot." and ",Governor Clinton." and ",President Bush."
US_debates$text[21] # ",Mr. Perot." and ",Governor Clinton." and ",President Bush."
US_debates$text[22] # ",Mr. Perot." and ",Governor Clinton." and ",President Bush."
US_debates$text[23] # ",DUKAKIS:" and ",BUSH:"
US_debates$text[24] # ",DUKAKIS:" and ",BUSH:"
US_debates$text[25] # ",Mr. Mondale." and ",The President." (Reagan)
US_debates$text[26] # ",Mr. Mondale." and ",The President." (Reagan)
US_debates$text[27] # ",THE PRESIDENT." (Carter) and ",GOVERNOR REAGAN."
US_debates$text[28] # ",ANDERSON:" (indepenedent candidate) and ",REAGAN:" (Should this be dropped?)

# Now that we know names we still ned to separate debates by speaker for analysis.
# However, because of uniqueness of how speakers at debates were coded in text,
# we have to do it text by text. Or at least we have to make them a bit more standard ...
# We use the split_text() function here (see the text_wrangling_functions sript for more info).
# The function gives you some flexibility to work with, as arguments can be set for split marks
# (splitsign) and for speakers markers (markersign)
# Let's see how it works:
debate1 <- split_text(US_debates$text[1], ",.", ":") # inspected and works great
debate2 <- split_text(US_debates$text[2], ",.", ":") # inspected

# Though we can do this text by text, I prefer subbing how speakers are
# marked in the texts, since we know the speakers, and add a for loop
# just to avoid human error. I will add colon to indicate speakers after names
# and ".," before to split text.
x <- as.character(US_debates$text)
x <- gsub(",TRUMP:", "xxx Donald Trump:", x)
x <- gsub(",BIDEN:", "xxx Joe Biden:", x)
x <- gsub(",CLINTON:", "xxx Hillary Clinton:", x)
x <- gsub(",Gov. Romney.", "xxx Mitt Romney:", x)
x <- gsub(",The President.", "xxx The President P:", x) #### can you gsub this only in specific text?
x <- gsub(",OBAMA:", "xxx Barack Obama:", x)
x <- gsub(",MCCAIN:", "xxx John McCain:", x)
x <- gsub(",Senator Kerry.", "xxx John Kerry:", x)
x <- gsub(",President Bush.", "xxx President Bush:", x) # ambiguous, will have to fix later = George W. Bush
x <- gsub(",BUSH:", "xxx BUSH:", x) # ambiguous, will have to fix later = George W. Bush
x <- gsub(",GORE:", "xxx Al Gore:", x)
x <- gsub(",Senator Dole.", "xxx Bob Dole:", x)
x <- gsub(",Mr. Perot.", "xxx Ross Perot:", x)
x <- gsub(",Governor Clinton.", "xxx Bill Clinton:", x)
x <- gsub(",President Bush.", "xxx President Bush:", x) # ambiguous, will have to fix later = George Bush
x <- gsub(",DUKAKIS:", "xxx Michael Dukakis:", x)
x <- gsub(",BUSH:", "xxx BUSH:", x) # ambiguous, will have to fix later = George Bush
x <- gsub(",Mr. Mondale.", "xxx Walter Mondale:", x)
x <- gsub(",THE PRESIDENT.", "xxx Jimmy Carter:", x)
x <- gsub(",GOVERNOR REAGAN.", "xxx Ronald Reagan:", x)
x <- gsub(",ANDERSON:", "xxx John Anderson:", x)
x <- gsub(",REAGAN:", "xxx Ronald Reagan:", x)
x <- gsub("\n", " ", x)
spk <- as.data.frame(x)

# initialize a deta frame
s_debates <- data.frame()
# add a loop and good luck
# Please note that the function is interactive and, thus,
# I only select (click on "yes") for speakers that are runner
# upper for elections (as named above)
# or appear as the president.
# No other names are slected as these are likely moderators.
# For random sentences identified, make sure to click "no" as well.
for (i in 1:nrow(spk)) {
  debate <- split_text(spk$x[i], "xxx", ":")
  s_debates = rbind(s_debates, data.frame(debate))
  print(i)
}
# In theory, I should have 59 speakers. That is 2 speakers for
# 25 debates and 3 for 3 debates.
# Let's see:
s_debates
# first time around we get 64 rows (speakers) ...
# "The president was clicked as legit were he should not have, is my intuition.
# just re run with the list of debates and speakers (above lines 104-132) next to me.
# Now we need to clean all of that and merge with US_debates data for titles and dates.
# With careful clicking, we are now at 59 observations.
# The issue was "the president" being mathed as a speaker where he should not have.
# Now, let's bind US_debates data with the new debates data by speaker

### Campaign Remarks

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
# let's remove some speakers we know we do not care about
# Let's keep all present in the list of debate speakers plus Bernie Sanders just in case.
speakers <- unique(camp$speaker)
sp <- speakers[c(1,2,7,8,9,13,18,19,40, 44, 48,49,51,52, 54,55,56,57)]
sp
# paste together
ss <- paste0(sp, sep = "|", collapse = " ")
ss
# remove white space and last brace
ss <- gsub("\\| ", "|", ss)
ss <- gsub("\\|$", "", ss)
ss
# copy and paste output in function
camp <- data.frame(camp[grep("Donald J. Trump|Joseph R. Biden|Hillary Clinton|Barack Obama|William J. Clinton|Bernie Sanders|Mitt Romney|George W. Bush|John McCain|Albert Gore, Jr.|John F. Kerry|Robert Dole|George Bush|H. Ross Perot|Ronald Reagan|Michael S. Dukakis|Walter F. Mondale|Jimmy Carter", camp$speaker),])

# Investigate title
head(camp$title, 100)
tail(camp$title, 100)
# Most titles in first 100 read "press release" or campaign statement, let's investigate
camp[1,]
camp[54,]
camp[98,]
# maybe press releases are not as helpful as they appear to be usually written documents
# or, in some cases, read by campaign staff...
camp[21,]
camp[76,]
# some campaing statements appear to be delivered by candidates...

# let's remove press releases from campaign docs first
camp <- data.frame(camp[grep("press release", camp$title, ignore.case = TRUE, invert = TRUE),])

# Let's investigate again
head(camp$title, 100)
camp[67,]
camp[3,]
camp [24,]
camp [61,]
# Most campaign statements for Trump appear very short and are delivered by his campaign staff.
# While Biden appears to deliver most his campaign statements...
