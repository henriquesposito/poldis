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

usethis::use_data(US_Debates)
