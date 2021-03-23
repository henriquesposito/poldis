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
