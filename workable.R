#to get data

library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500 <- url %>%
     xml2::read_html() %>%
     html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
     html_table()
SP500 <- SP500[[1]]
Tix <- SP500$Symbol

set.seed(10)
stock=sample(Tix, 5)
stock
