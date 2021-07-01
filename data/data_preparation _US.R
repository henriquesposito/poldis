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

# Importantly, when presidential candidates provided interviews at campaign periods,
# these were recorded under the US_campaign data.
# Let's extract that information here.
load("~/GitHub/Poldis/data-raw/US_Campaign.rda")
camp_inter <- as.data.frame(US_Campaign)
# Get observations since 1981
camp_inter$date <- lubridate::mdy(camp_inter$date)
camp_inter <- camp_inter %>% dplyr::filter(date > "1980-01-01")
# Let's select only observations that have the word interview in title
camp_inter <- data.frame(camp_inter[grep("interview", camp_inter$title, ignore.case = TRUE),])
# Let's inspect the 180 observations
camp_inter$title
camp_inter$speaker # We do not want some of these speakers
unique(camp_inter$speaker)
# Let's keep only the speakers we are interested in
camp_inter <- data.frame(camp_inter[grep("Donald J. Trump|Bernie Sanders|Hillary Clinton|John McCain|Barack Obama|Joseph R. Biden|Mitt Romney|Robert Dole" , camp_inter$speaker, ignore.case = TRUE),])

# bind datasets together
US_interviews <- rbind(US_interviews, camp_inter)
# double check
unique(US_interviews$speaker)
# exporting clean data to data folder
usethis::use_data(US_interviews, overwrite = TRUE)

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
# copy and paste output in function, also good for double chacking
camp <- data.frame(camp[grep("Donald J. Trump|Joseph R. Biden|Hillary Clinton|Barack Obama|William J. Clinton|Bernie Sanders|Mitt Romney|George W. Bush|John McCain|Albert Gore, Jr.|John F. Kerry|Robert Dole|George Bush|H. Ross Perot|Ronald Reagan|Michael S. Dukakis|Walter F. Mondale|Jimmy Carter", camp$speaker),])

# Remove interviews which were added to the US_interviews dataset above
camp <- data.frame(camp[grep("interview", camp$title, ignore.case = TRUE, invert = TRUE),])

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
camp[24,]
camp[61,]
camp[17,]
camp[82,]
camp[15,]
# Most campaign statements for Trump appear very short and are often delivered by
# his campaign staff.
# While Biden appears to deliver most his campaign statements...
# The ones delivered by candidates appear to have the word "by" in title.
# Let's investigate once more
tail(camp$title, 100)
sample(camp$title, 100)
# For last 100 titles are a bit different.
# Let's start by removing observations that contain "biden campaign statement"
# or "trup campign statement" in title.
camp <- data.frame(camp[grep("Biden Campaign Statement|Trump Campaign Statement", camp$title, ignore.case = TRUE, invert = TRUE),])
head(camp$title, 100)
sample(camp$title, 100)
# Remove as well "Biden for President Statement", ""Further Statement from the Trump Campaign"
# and "Merry Christmas from the Trump Campaign".
camp <- data.frame(camp[grep("Biden for President Statement|Further Statement from the Trump Campaign|Merry Christmas from the Trump Campaign", camp$title, ignore.case = TRUE, invert = TRUE),])
sample(camp$title, 100)
# Remove "Trump Campaign Message", "Statement from Biden for President", "Michelle Obama: Address",
# "E-mail Message" and "Campaign Statement - Trump".
camp <- data.frame(camp[grep("Trump Campaign Message|Statement from Biden for President|Michelle Obama: Address|E-mail Message|Campaign Statement - Trump", camp$title, ignore.case = TRUE, invert = TRUE),])
sample(camp$title, 100)
# Remove "Letter to Superdelegates|Happy New Year from the Trump Campaign"
camp <- data.frame(camp[grep("Letter to Superdelegates|Happy New Year from the Trump Campaign", camp$title, ignore.case = TRUE, invert = TRUE),])
sample(camp$title, 100)
# Lastly, remove "Commencement Address|News Conference"
camp <- data.frame(camp[grep("Commencement Address|News Conference", camp$title, ignore.case = TRUE, invert = TRUE),])
sample(camp$title, 100)
# Most titles left are "remarks at/in" (usually refer to campaign rallies)
# and "statement by" (statement delivered by candidate).
# export clean data
US_campaign <- camp
usethis::use_data(US_campaign, overwrite = TRUE)

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
debate1 <- split_text(US_debates$text[1], ",.", ":") # inspected and works
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
x <- gsub(",The President.", "xxx The President P:", x) # ambiguous, will have to fix later = Obama
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
# Specifically, in debates 12, 13, 14, 20, 27 I was asked if "Prsident P" - was a ligit speaker
# and answerd no since for those cases the speakers were coded differently
# (for all legit speakers per debate rfer to lines 104-132 above).

# Now, let's bind US_debates data with the new debates data by speaker
# First, we need to extract the titles from all the debates twice.
US_debates$title # copy titles and duplicates names for all but 20, 21, 22
# for which need to triplicate for the three speakers
td <- data.frame(Title = c("Presidential Debate at Belmont University in Nashville, Tennessee",
                           "Presidential Debate at Belmont University in Nashville, Tennessee",
                           "Presidential Debate at Case Western Reserve University in Cleveland, Ohio",
                           "Presidential Debate at Case Western Reserve University in Cleveland, Ohio",
                           "Presidential Debate at the University of Nevada in Las Vegas",
                           "Presidential Debate at the University of Nevada in Las Vegas",
                           "Presidential Debate at Washington University in St. Louis, Missouri",
                           "Presidential Debate at Washington University in St. Louis, Missouri" ,
                           "Presidential Debate at Hofstra University in Hempstead, New York",
                           "Presidential Debate at Hofstra University in Hempstead, New York",
                           "Presidential Debate in Boca Raton, Florida" ,
                           "Presidential Debate in Boca Raton, Florida",
                           "Presidential Debate in Hempstead, New York",
                           "Presidential Debate in Hempstead, New York",
                           "Presidential Debate in Denver, Colorado",
                           "Presidential Debate in Denver, Colorado",
                           "Presidential Debate at Hofstra University in Hempstead, New York",
                           "Presidential Debate at Hofstra University in Hempstead, New York",
                           "Presidential Debate at Belmont University in Nashville, Tennessee",
                           "Presidential Debate at Belmont University in Nashville, Tennessee" ,
                           "Presidential Debate at the University of Mississippi in Oxford",
                           "Presidential Debate at the University of Mississippi in Oxford",
                           "Presidential Debate in Tempe, Arizona",
                           "Presidential Debate in Tempe, Arizona",
                           "Presidential Debate in St. Louis, Missouri",
                           "Presidential Debate in St. Louis, Missouri",
                           "Presidential Debate in Coral Gables, Florida",
                           "Presidential Debate in Coral Gables, Florida",
                           "Presidential Debate in St. Louis",
                           "Presidential Debate in St. Louis",
                           "Presidential Debate in Winston-Salem, North Carolina",
                           "Presidential Debate in Winston-Salem, North Carolina",
                           "Presidential Debate in Boston",
                           "Presidential Debate in Boston",
                           "Presidential Debate in San Diego",
                           "Presidential Debate in San Diego",
                           "Presidential Debate in Hartford",
                           "Presidential Debate in Hartford",
                           "Presidential Debate in East Lansing, Michigan", # 3
                           "Presidential Debate in East Lansing, Michigan", # 3
                           "Presidential Debate in East Lansing, Michigan", # 3
                           "Presidential Debate at the University of Richmond", # 3
                           "Presidential Debate at the University of Richmond", # 3
                           "Presidential Debate at the University of Richmond", # 3
                           "Presidential Debate in St. Louis", # 3
                           "Presidential Debate in St. Louis", # 3
                           "Presidential Debate in St. Louis", # 3
                           "Presidential Debate at the University of California in Los Angeles",
                           "Presidential Debate at the University of California in Los Angeles",
                           "Presidential Debate in Winston-Salem, North Carolina",
                           "Presidential Debate in Winston-Salem, North Carolina",
                           "Debate Between the President and Former Vice President Walter F. Mondale in Kansas City, Missouri",
                           "Debate Between the President and Former Vice President Walter F. Mondale in Kansas City, Missouri",
                           "Debate Between the President and Former Vice President Walter F. Mondale in Louisville, Kentucky",
                           "Debate Between the President and Former Vice President Walter F. Mondale in Louisville, Kentucky",
                           "Presidential Debate in Cleveland",
                           "Presidential Debate in Cleveland",
                           "Presidential Debate in Baltimore (Reagan-Anderson)",
                           "Presidential Debate in Baltimore (Reagan-Anderson)"))

# Now let's do the same for dates
US_debates$date
dt <- data.frame(Date = c( "2020-10-22", "2020-10-22",
                           "2020-09-29", "2020-09-29",
                           "2016-10-19", "2016-10-19",
                           "2016-10-09", "2016-10-09",
                           "2016-09-26", "2016-09-26",
                           "2012-10-22", "2012-10-22",
                           "2012-10-16", "2012-10-16",
                           "2012-10-03", "2012-10-03",
                           "2008-10-15", "2008-10-15",
                           "2008-10-07", "2008-10-07",
                           "2008-09-26", "2008-09-26",
                           "2004-10-13", "2004-10-13",
                           "2004-10-08", "2004-10-08",
                           "2004-09-30", "2004-09-30",
                           "2000-10-17", "2000-10-17",
                           "2000-10-11", "2000-10-11",
                           "2000-10-03", "2000-10-03",
                           "1996-10-16", "1996-10-16",
                           "1996-10-06", "1996-10-06",
                           "1992-10-19", "1992-10-19", "1992-10-19", #3
                           "1992-10-15", "1992-10-15", "1992-10-15", #3
                           "1992-10-11", "1992-10-11", "1992-10-11", #3
                           "1988-10-13", "1988-10-13",
                           "1988-09-25", "1988-09-25",
                           "1984-10-21", "1984-10-21",
                           "1984-10-07", "1984-10-07",
                           "1980-10-28", "1980-10-28",
                           "1980-09-21", "1980-09-21"))

# bind things together
US_debates <- cbind(td, dt, s_debates)

# rename columns
US_debates <- rename(US_debates, Speakers = Group.1, Text = x)
# rename some ambuiguos speakers
US_debates$Speakers
US_debates$Speakers[12] <- " Barack Obama"
US_debates$Speakers[14] <- " Barack Obama"
US_debates$Speakers[16] <- " Barack Obama"
US_debates$Speakers[24] <- " George W. Bush"
US_debates$Speakers[26] <- " George W. Bush"
US_debates$Speakers[28] <- " George W. Bush"
US_debates$Speakers[30] <- " George W. Bush"
US_debates$Speakers[32] <- " George W. Bush"
US_debates$Speakers[34] <- " George W. Bush"
US_debates$Speakers[36] <- " Bill Clinton"
US_debates$Speakers[38] <- " Bill Clinton"
US_debates$Speakers[40] <- " George Bush"
US_debates$Speakers[43] <- " George Bush"
US_debates$Speakers[46] <- " George Bush"
US_debates$Speakers[48] <- " George Bush"
US_debates$Speakers[50] <- " George Bush"
US_debates$Speakers[52] <- " Ronald Reagan"
US_debates$Speakers[54] <- " Ronald Reagan"

# export clean data
usethis::use_data(US_debates, overwrite = TRUE)
