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
