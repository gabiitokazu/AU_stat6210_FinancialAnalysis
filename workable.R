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
stocks_tickers=sample(Tix, 5)
stocks_tickers

three_year_ago <- seq(as.Date("2020-04-01"), length = 2, by = "-3 year")[2]
getSymbols(stocks_tickers, from = three_year_ago, to = as.Date("2020-04-01"))

stock1<- na.omit(ClCl(get(stocks_tickers[1])))
stock2 <- na.omit(ClCl(get(stocks_tickers[2])))
sigma_stocks <- var(cbind(stock1, stock2))

