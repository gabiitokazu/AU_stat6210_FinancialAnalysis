#to get data

library(rvest)
library(quantmod)


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

stock1<- na.omit(ClCl(get(stocks_tickers[1])))   #what does this do? I can't run on my machine. What is the ClCl() function?
stock2 <- na.omit(ClCl(get(stocks_tickers[2])))
sigma_stocks <- var(cbind(stock1, stock2))











#--------Gabii (see discussion in Forum (Group9 page in Canvas)------------------
library(quantmod)
library(rvest)
library(BatchGetSymbols)


# 'read.html' function canvass the html page, finds the css selectors that matches the data, and extracts it, creating a xml_document. 
sp500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
# for the new object 'sp500' we track the nodes that corresponds to the selector ('html_nodes') that matches the data and extract the contents of the data into another object ('html_text').
# the %>% operator is called a pipe. It passes the object that precedes it as the first argument to the function that follows it.
sp500 %>% 
        html_nodes(".text") %>%  
        html_text() -> ticker_sp500 


SP500_symbol <- ticker_sp500[(1:499)*2+1]
SP500_symbol[SP500_symbol == "BRK.B"] <- "BRK-B"
SP500_symbol[SP500_symbol == "BF.B"] <- "BF-B"

SP500_symbol <- as.data.frame(SP500_symbol)

#gets info on all stocks on SP500:
future::plan(future::multisession, 
             workers = 4) # uses 4 cores ( to see how many cores you have available type 'future::availableCores()')
stocks_all <- BatchGetSymbols(tickers = ticker_sp500[(1:499)*2+1], 
                              first.date = '2017-04-01', 
                              do.parallel = TRUE, 
                              do.cache = FALSE)

stocks_all <- as.data.frame(stocks_all[["df.tickers"]])

list.stocks_all <- list(data.frame())

for (i in 1:dim(SP500_symbol)[1]){
        df <- as.data.frame(stocks_all %>% filter(
                stocks_all$ticker == SP500_symbol$SP500_symbol[[i]]
        ))
        if(dim(df)[1] != 0){
                list.stocks_all[[i]] <- df
        }
}

# not necessary, but helps keepint the global env tidy
rm(df, i)

list.stocks_all <- Filter(NROW, list.stocks_all)

var.stocks_all <- data.frame(stock=character(0),variance=double(0))

for (i in 1:length(list.stocks_all)[1]){
        price <- list.stocks_all[[i]]$price.close
        stock <- list.stocks_all[[i]]$ticker
        var.stock <- as.data.frame(cbind(as.character(stock[[1]]),var(price)))
        var.stocks_all <- rbind(as.data.frame(var.stocks_all), as.data.frame(var.stock))
}

# not necessary, but helps keepint the global env tidy
rm(price, stock, var.stock, i)

colnames(var.stocks_all) = c("stock","variance")

best.stocks <- var.stocks_all[order(as.double(var.stocks_all$variance)),][1:5, ] # order all stocks from low to high and get only the first 3 rows out of it
best.stocks_names <- best.stocks[[1]] # list with just the names
best.stocks_names <- as.data.frame(t(best.stocks_names))

best.stocks_data <- list(data.frame())

for (i in 1:dim(best.stocks_names)[2]){
        df <- as.data.frame(stocks_all %>% filter(
                stocks_all$ticker == best.stocks_names[[i]]
        ))
        if(dim(df)[1] != 0){
                best.stocks_data[[i]] <- df
        }
}

# rm() not necessary, but helps keepint the global env tidy
rm(df, i)
rm(best.stocks, best.stocks_names)
rm(var.stocks_all, var.stocks_ordered)
rm(ticker_sp500, SP500_symbol, sp500, stocks_all)
