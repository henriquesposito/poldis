library(rvest)
library(dplyr)
library(usethis)
library(stringr)
library(lubridate)
# # Scrape TAPP
# get_text <- function(source_link) {
#   oral_remarks_page = read_html(source_link)
#   oral_remarks_text = oral_remarks_page %>% html_nodes(".field-docs-content p") %>% html_text() %>% paste(collapse = ",")
#   return(oral_remarks_text)
# }
# US_Oral_Remarks_22_26 <- data.frame()
# for (page_result in seq(from = 0, to = 31, by = 1)) {
#   link = paste0("https://www.presidency.ucsb.edu/documents/app-categories/presidential/spoken-addresses-and-remarks?items_per_page=60&page=",
#                 page_result)
#   page = read_html(link)
#   title <- page %>% html_nodes(".field-title a") %>% html_text()
#   date <-  page %>% html_nodes(".date-display-single") %>% html_text()
#   speaker <- page %>% html_nodes(".margin-top a") %>% html_text()
#   source_links <- page %>% html_nodes(".field-title a") %>% html_attr("href") %>% paste0("https://www.presidency.ucsb.edu", . , sep = "")
#   text <- sapply(source_links, FUN = get_text, USE.NAMES = FALSE)
#   US_Oral_Remarks_22_26 <- rbind(US_Oral_Remarks_22_26, data.frame(title, date, speaker, source_links, text, stringsAsFactors = FALSE))
#   print(paste("Page:", page_result))
# }
# # Merge new and old data
# US_Oral_Remarks_21 <- US_Oral_Remarks_21 |>
#   mutate(date = lubridate::mdy(date)) |>
#   filter(date > as.Date("1993-01-19")) |>
#   select(-source_links) |>
#   arrange(desc(date))
# #saveRDS(US_Oral_Remarks_21, "US_Oral_Remarks_21.rds")
# US_Oral_Remarks_25 <- US_Oral_Remarks_22_26 |>
#   mutate(date = lubridate::mdy(date)) |>
#   filter(date > as.Date("2021-12-31"), date < as.Date("2025-01-21") ) |>
#   select(-source_links) |>
#   arrange(desc(date))
# #saveRDS(US_Oral_Remarks_25, "US_Oral_Remarks_25.rds")
US_presidential_speeches_1993_2025 <-
  rbind(US_Oral_Remarks_21, US_Oral_Remarks_25) |>
  mutate(president = case_when(speaker == "Joseph R. Biden" &
                               date > as.Date("2021-01-19") &
                               date < as.Date("2025-01-21") ~ "Biden",
                               speaker == "Joseph R. Biden, Jr." &
                                 date > as.Date("2021-01-19") &
                                 date < as.Date("2025-01-21") ~ "Biden",
                             speaker == "Donald J. Trump (1st Term)" &
                               date > as.Date("2017-01-19") &
                               date < as.Date("2021-01-21") ~ "Trump",
                             speaker == "Donald J. Trump" &
                               date > as.Date("2017-01-19") &
                               date < as.Date("2021-01-21") ~ "Trump",
                             speaker == "Barack Obama" &
                               date > as.Date("2009-01-19") &
                               date < as.Date("2017-01-21") ~ "Obama",
                             speaker == "George W. Bush" &
                               date > as.Date("2001-01-19") &
                               date < as.Date("2009-01-21") ~ "Bush",
                             speaker == "William J. Clinton" &
                               date > as.Date("1993-01-19") &
                               date < as.Date("2001-01-21") ~ "Clinton",
                             .default = NA)) |>
  filter(!is.na(president)) |>
  select(-speaker) |>
  distinct()
# Check dates
filter(US_presidential_speeches_1993_2025, !is.na(president)) |>
  group_by(president) |>
  summarise(min_date = min(date),
            max_date = max(date))
# Save data for analysis
#saveRDS(US_presidential_speeches_1993_2025, "US_presidential_speeches_1993_2025.rds")
# Save inaugural addresses data for package (merge TRUMP 2 IA as well)
US_inaugural_addresses_1993_2025 <- US_presidential_speeches_1993_2025 |>
  filter(grepl("Inaugural Address", title, ignore.case = TRUE)) |>
  rbind(trump_ia) |>
  select(president, date, text) |>
  arrange(date)
#saveRDS(US_inaugural_addresses_1993_2025, "US_inaugural_addresses_1993_2025.rds")
