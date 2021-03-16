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
