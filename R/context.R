# Context

# A function for getting string matches and the context in which they occur.


# Banco Central em Discursos Presidenciais

library(dplyr)
library(stringr)

# count
banco_central <- stringr::str_count(BR_oral$text, "banco central|Banco Central|^BC$")
sum(banco_central)
# sentence level dataframe
bc <- data.frame(BR_oral[grep("banco central|Banco Central|^BC$", BR_oral$text, ignore.case = TRUE),])
bc <- bc %>% select(date, presid, party, length, text)
bc[50,]
bce <- bc
# 3 sentences
text <- gsub(".*?(([^.]+\\.){1}[^.]+(banco central|Banco Central|banco Central|Banco central).*?\\.(.*?\\.){1}).*",
             "\\1", bce$text, ignore.case = TRUE)
summary(as.factor(bce$presid))
bce$text

bca <-head(bc)
t <- stringr::str_extract_all(bca$text, ".*?(([^.]+\\.){1}[^.]+(banco central|Banco Central|banco Central|Banco central).*?\\.(.*?\\.){1}).*")

# Amazonia

library(dplyr)
library(stringr)

# count
am <- stringr::str_count(BR_oral$text, "Amazon|desmatamento|climaticas")
sum(am)
# sentence level dataframe
am <- data.frame(BR_oral[grep("Amazon|desmatamento|climaticas", BR_oral$text, ignore.case = TRUE),])
am <- am %>% select(date, presid, party, length, text)
am[50,]
ame <- am
# 3 sentences
text <- gsub(".*?(([^.]+\\.){1}[^.]+(Amazon|desmatamento|climaticas).*?\\.(.*?\\.){1}).*",
             "\\1", ame$text, ignore.case = TRUE)
summary(as.factor(ame$presid))
ame[50,]

ama <-head(am)
t <- stringr::str_extract_all(ama$text, ".*?(([^.]+\\.){1}[^.]+(Amazon|desmatamento|climaticas).*?\\.(.*?\\.){1}).*")
