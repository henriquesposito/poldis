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
