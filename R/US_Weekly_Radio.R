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
