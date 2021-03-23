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

US_Interviews = data.frame()

for (page_result in seq(from = 0, to = 95, by = 1)) {
  link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/presidential/interviews?page=",
                page_result)
  page = read_html(link)

  title <- page %>% html_nodes(".field-title a") %>% html_text()
  date <-  page %>% html_nodes(".date-display-single") %>% html_text()
  president <- page %>% html_nodes(".margin-top a") %>% html_text()
  source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste0("https://www.presidency.ucsb.edu", . , sep = "")
  text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)

  US_Interviews = rbind(US_Interviews, data.frame(title, date, president, source_links, text, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))

}

usethis::use_data(US_Interviews, overwrite = TRUE)

# This data and script were then moved to data-raw folder.
