# US President Interviews

# Source: https://www.presidency.ucsb.edu/

library(rvest)
library(dplyr)
library(usethis)

get_text <- function(source_link) {
  interview_page = read_html(source_link)
  interview_text = interview_page %>% html_nodes("#block-system-main p") %>% html_text() %>% paste(collapse = ",")
  return(interview_text)
}

get_participants <- function(source_link) {
  interview_page = read_html(source_link)
  interview_participants = interview_page %>% html_nodes("i") %>% html_text() %>% paste(collapse = ",")
  return(interview_participants)
}

for (page_result in seq(from = 0, to = 95, by = 1)) {
  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/presidential/interviews?page=",
                page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title p") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  president <- page %>% html_nodes(".margin-top a") %>% html_text()
  source_links <- page %>% html_nodes(".field-title p") %>% html_attr("href") %>% paste("https://www.presidency.ucsb.edu", . , sep = "")
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)
  participants <- sapply(source_links, FUN = get_participants, USE.NAMES = FALSE)

  US_interviews = rbind(US_interviews, data.frame(title, date, participants, moderator, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_interviews)
