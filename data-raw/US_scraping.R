# US Presidents' Oral Remarks, adresses and weekly radio talks
# This dataset includes presidential oral speeches and remarks in US since 1970.
# Source: https://www.presidency.ucsb.edu/

library(rvest)
library(dplyr)
library(usethis)
library(stringr)

get_text <- function(source_link) {
  oral_remarks_page = read_html(source_link)
  oral_remarks_text = oral_remarks_page %>% html_nodes(".field-docs-content p") %>% html_text() %>% paste(collapse = ",")
  return(oral_remarks_text)
}

US_Oral_Remarks = data.frame()

for (page_result in seq(from = 0, to = 550, by = 1)) {

  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/presidential/spoken-addresses-and-remarks?page=", page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title a") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  speaker <- page %>% html_nodes(".margin-top a") %>% html_text()
  source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste0("https://www.presidency.ucsb.edu", . , sep = "")
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)

  US_Oral_Remarks <- rbind(US_Oral_Remarks, data.frame(title, date, speaker, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_Oral_Remarks, overwrite = TRUE)

# The text data needs to be cleaned to include only remarks by presidents and vice-presidents.
# This data and script were then moved to data-raw folder.

# Other miscelaneous oral remarks
# This dataset includes presidential miscelanneous remarks in US since 1960s.
# Source: https://www.presidency.ucsb.edu/

get_text <- function(source_link) {
  oral_mremarks_page = read_html(source_link)
  oral_mremarks_text = oral_mremarks_page %>% html_nodes(".field-docs-content p") %>% html_text() %>% paste(collapse = ",")
  return(oral_mremarks_text)
}

US_M_Remarks = data.frame()

for (page_result in seq(from = 0, to = 2200, by = 1)) {

  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/presidential/miscellaneous-remarks?page=", page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title a") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  speaker <- page %>% html_nodes(".margin-top a") %>% html_text()
  source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste0("https://www.presidency.ucsb.edu", . , sep = "")
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)

  US_M_Remarks <- rbind(US_M_Remarks, data.frame(title, date, speaker, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_M_Remarks, overwrite = TRUE)

# There is some overlap with the US_Oral_Remarks dataset and, for that reason, these are scrapped separetely
# and will later be joined using dplyr::fuzzyjoin.
# This data and script were then moved to data-raw folder.

# US Presidents' Interviews
# This dataset includes many presidential interviews in US since 1900.
# These range from radio, newspaper to television and other interviews for presidents while in office.
# Source: https://www.presidency.ucsb.edu/

library(rvest)
library(dplyr)
library(usethis)

get_text <- function(source_link) {
  interview_page = read_html(source_link)
  interview_text = interview_page %>% html_nodes(".field-docs-content p") %>% html_text() %>% paste(collapse = ",")
  return(interview_text)
}

get_speaker <- function(source_link) {
  interview_page = read_html(source_link)
  speaker = interview_page %>% html_nodes(".diet-title a") %>% html_text()
  return(speaker)
}

US_Interviews = data.frame()

for (page_result in seq(from = 0, to = 95, by = 1)) {
  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/presidential/interviews?page=",
                page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title a") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste0("https://www.presidency.ucsb.edu", . , sep = "")
  speaker <- sapply(source_links, FUN = get_speaker, USE.NAMES = FALSE)
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)

  US_Interviews = rbind(US_Interviews, data.frame(title, date, speaker, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_Interviews, overwrite = TRUE)

# This data and script were then moved to data-raw folder.

# US Campaign Documents
# This dataset includes campaogn documents from candidates including a variety of types from
# press releases to interviews to campaign rallies made during campaigns since 1980.
# Source: https://www.presidency.ucsb.edu/

get_text <- function(source_link) {
  campaign_page = read_html(source_link)
  campaign_text = campaign_page %>% html_nodes(".field-docs-content p") %>% html_text() %>% paste(collapse = ",")
  return(campaign_text)
}

US_Campaign = data.frame()

for (page_result in seq(from = 0, to = 323, by = 1)) {

  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/elections-and-transitions/campaign-documents?page=", page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title a") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  speaker <- page %>% html_nodes(".margin-top a") %>% html_text()
  source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste0("https://www.presidency.ucsb.edu", . , sep = "")
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)

  US_Campaign <- rbind(US_Campaign, data.frame(title, date, speaker, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_Campaign, overwrite = TRUE)

# There are several documents, as press releases which are not often delivered by candidates and, thus, might be cleaned in later stages.
# This data and script were then moved to data-raw folder.

# US Presidential Debates
# This dataset includes all presidential debates in US since 1960.
# These range from party debates for nominations to run-off debates.
# Source: https://www.presidency.ucsb.edu/

library(rvest)
library(dplyr)
library(usethis)

get_text <- function(source_link) {
  debate_page = read_html(source_link)
  debate_text = debate_page %>% html_nodes("p~ p+ p") %>% html_text() %>% paste(collapse = ",")
  return(debate_text)
}

get_participants <- function(source_link) {
  debate_page = read_html(source_link)
  debate_participants = debate_page %>% html_nodes("p:nth-child(1)") %>% html_text() %>% paste(collapse = ",")
  return(debate_participants)
}

get_moderator <-  function(source_link) {
  debate_page = read_html(source_link)
  debate_moderator = debate_page %>% html_nodes("p:nth-child(2)") %>% html_text() %>% paste(collapse = ",")
  return(debate_moderator)
}

US_Debates = data.frame()

for (page_result in seq(from = 0, to = 17, by = 1)) {
  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/elections-and-transitions/debates?page=",
                page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title a") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste("https://www.presidency.ucsb.edu", . , sep = "")
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)
  participants <- sapply(source_links, FUN = get_participants, USE.NAMES = FALSE)
  moderator <- sapply(source_links, FUN = get_moderator, USE.NAMES = FALSE)

  US_Debates = rbind(US_Debates, data.frame(title, date, participants, moderator, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_Debates, overwrite = TRUE)

# The debates include early on party nomination debates and these will likely be excluded for analysis and only
# final party nomination debates and national debates will be used.
# This data and script were then moved to data-raw folder.

# Weekly radio talk for US presidents
# This dataset includes weekly presidential remarks in the radio since 1982 for the US.
# Source: https://www.presidency.ucsb.edu/

get_text <- function(source_link) {
  weeklyr_page = read_html(source_link)
  weeklyr_text = weeklyr_page %>% html_nodes(".field-docs-content p") %>% html_text() %>% paste(collapse = ",")
  return(weeklyr_text)
}

US_Weekly_Radio = data.frame()

for (page_result in seq(from = 0, to = 163, by = 1)) {

  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/spoken-addresses-and-remarks/presidential/saturday-weekly-addresses-radio?page=", page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title a") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  speaker <- page %>% html_nodes(".margin-top a") %>% html_text()
  source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste0("https://www.presidency.ucsb.edu", . , sep = "")
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)

  US_Weekly_Radio <- rbind(US_Weekly_Radio, data.frame(title, date, speaker, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_Weekly_Radio, overwrite = TRUE)

# This will likely be aaded to a broader oral remarks dataset.
# This data and script were then moved to data-raw folder.

# US Presidents' News Conferences
# This dataset includes many presidential news conferences in US since 1960s.
# Source: https://www.presidency.ucsb.edu/

library(rvest)
library(dplyr)
library(usethis)

get_text <- function(source_link) {
  news_page = read_html(source_link)
  news_text = news_page %>% html_nodes(".field-docs-content p") %>% html_text() %>% paste(collapse = ",")
  return(news_text)
}

US_News_Conferences = data.frame()

for (page_result in seq(from = 0, to = 120, by = 1)) {
  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/presidential/news-conferences?page=",
                page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title a") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  president <- page %>% html_nodes(".margin-top a") %>% html_text()
  source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste0("https://www.presidency.ucsb.edu", . , sep = "")
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)

  US_News_Conferences = rbind(US_News_Conferences, data.frame(title, date, president, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_News_Conferences, overwrite = TRUE)

# These will be binded to interviews to get a more comprehensive dataset.
# This data and script were then moved to data-raw folder.
