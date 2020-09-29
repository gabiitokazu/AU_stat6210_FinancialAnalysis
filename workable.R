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


#clcl()function is for daily return







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






#_____________________jianfeng

library(quantmod)
library(rvest)

# symbols of all S&P500 stocks
sp500 <- read_html("https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
sp500 %>%
     html_nodes(".text") %>%
     html_text() -> ticker_sp500
SP500_symbol <- ticker_sp500[(1:499)*2+1]
SP500_symbol[SP500_symbol == "BRK.B"] <- "BRK-B"
SP500_symbol[SP500_symbol == "BF.B"] <- "BF-B"
# example 5 companies of the S&P500
stocks_considered <- c("MMM", "ALL", "MO", "GOOG", "BIO")

library(quantmod)
three_year_ago <- seq(as.Date("2020-04-01"), length = 2, by = "-3 year")[2]
stocks_tickers <- c("BIO", "GOOG","MO","ALL","MMM")
getSymbols(stocks_tickers, from = three_year_ago, to = as.Date("2020-04-01"))

BIO <- na.omit(ClCl(get(stocks_tickers[1])))
GOOG <- na.omit(ClCl(get(stocks_tickers[2])))
MO<-na.omit(ClCl(get(stocks_tickers[3])))
ALL<-na.omit(ClCl(get(stocks_tickers[4])))
MMM<-na.omit(ClCl(get(stocks_tickers[5])))
#cbind data
datacbind <- cbind(BIO, GOOG, MO)

#compute sigma and mu
sigma<-var(datacbind)
mu <- c(mean(BIO),mean(GOOG),mean(MO))
sigma_inv<-solve(sigma)
vectorp<-c(1,1,1)
vectorpT<-t(vectorp)
#weight formula
weight<-c(sigma_inv%*%vectorp)/vectorpT%*%sigma_inv%*%vectorp
#return formula
C<-1000000
weightT<-t(weight)
return<-(weightT%*%mu)*C
#risk formula
risk<-(weightT%*%sigma%*%weight)*C*C


#----------------------------------------
#this is  for boss
#one stock
##BIO
sigma_bio<-var(BIO)
vector0<-1
mu_bio<-mean(BIO)
sigma_bio_inv<-solve(sigma_bio)
vector0T<-t(vector0)
###weight
weight_bio<-as.vector(sigma_bio_inv%*%vector0)/as.vector(vector0T%*%sigma_bio_inv%*%vector0)
###expected return and risk
weight_bioT<-t(weight_bio)
return_bio<-(weight_bioT%*%mu_bio)*C
risk_bio<-(weight_bioT%*%sigma_bio%*%weight_bio)*C*C

#GOOG
sigma_GOOG<-var(GOOG)
mu_GOOG<-mean(GOOG)
sigma_GOOG_inv<-solve(sigma_GOOG)
###weight
weight_GOOG<-as.vector(sigma_GOOG_inv%*%vector0)/as.vector(vector0T%*%sigma_GOOG_inv%*%vector0)
###expected return and risk
weight_GOOGT<-t(weight_GOOG)
return_GOOG<-(weight_GOOG%*%mu_GOOG)*C
risk_GOOG<-(weight_GOOGT%*%sigma_GOOG%*%weight_GOOG)*C*C


#MO
sigma_MO<-var(MO)
mu_MO<-mean(MO)
sigma_MO_inv<-solve(sigma_MO)
###weight
weight_MO<-as.vector(sigma_MO_inv%*%vector0)/as.vector(vector0T%*%sigma_MO_inv%*%vector0)
###expected return and risk
weight_MOT<-t(weight_MO)
return_MO<-(weight_MO%*%mu_MO)*C
risk_MO<-(weight_MOT%*%sigma_MO%*%weight_MO)*C*C


#ALL
sigma_ALL<-var(ALL)
mu_ALL<-mean(ALL)
sigma_ALL_inv<-solve(sigma_ALL)
###weight
weight_ALL<-as.vector(sigma_ALL_inv%*%vector0)/as.vector(vector0T%*%sigma_ALL_inv%*%vector0)
###expected return and risk
weight_ALLT<-t(weight_ALL)
return_ALL<-(weight_ALL%*%mu_ALL)*C
risk_ALL<-(weight_ALLT%*%sigma_ALL%*%weight_ALL)*C*C


#MMM
sigma_MMM<-var(MMM)
mu_MMM<-mean(MMM)
sigma_MMM_inv<-solve(sigma_MMM)
###weight
weight_MMM<-as.vector(sigma_MMM_inv%*%vector0)/as.vector(vector0T%*%sigma_MMM_inv%*%vector0)
###expected return and risk
weight_MMMT<-t(weight_MMM)
return_MMM<-(weight_MMM%*%mu_MMM)*C
risk_MMM<-(weight_MMMT%*%sigma_MMM%*%weight_MMM)*C*C


     
#two stocks
##BIO with GOOG
cbind1<-cbind(BIO,GOOG)
vector1<-c(1,1)
sigma1<-var(cbind1)
mu1<-c(mean(BIO),mean(GOOG))
sigma1_inv<-solve(sigma1)
vector1T<-t(vector1)
###weight
weight1<-as.vector(sigma1_inv%*%vector1)/as.vector(vector1T%*%sigma1_inv%*%vector1)
####return and risk
weight1T<-t(weight1)
return1<-(weight1T%*%mu1)*C
risk1<-(weight1T%*%sigma1%*%weight1)*C*C


##BIO with MO
cbind2<-cbind(BIO,MO)
sigma2<-var(cbind2)
mu2<-c(mean(BIO),mean(MO))
sigma2_inv<-solve(sigma2)
###weight
weight2<-as.vector(sigma2_inv%*%vector1)/as.vector(vector1T%*%sigma2_inv%*%vector1)
###return and risk
weight2T<-t(weight2)
return2<-(weight2T%*%mu2)*C
risk2<-(weight2T%*%sigma2%*%weight2)*C*C


##BIO with ALL
cbind3<-cbind(BIO,ALL)
sigma3<-var(cbind3)
mu3<-c(mean(BIO),mean(ALL))
sigma3_inv<-solve(sigma3)
###weight
weight3<-as.vector(sigma3_inv%*%vector1)/as.vector(vector1T%*%sigma3_inv%*%vector1)
####return and risk
weight3T<-t(weight3)
return3<-(weight3T%*%mu3)*C
risk3<-(weight3T%*%sigma3%*%weight3)*C*C


##BIO with MMM
cbind4<-cbind(BIO,MMM)
sigma4<-var(cbind4)
mu4<-c(mean(BIO),mean(MMM))
sigma4_inv<-solve(sigma4)
####weight
weight4<-as.vector(sigma4_inv%*%vector1)/as.vector(vector1T%*%sigma4_inv%*%vector1)
####return and risk
weight4T<-t(weight4)
return4<-(weight4T%*%mu4)*C
risk4<-(weight4T%*%sigma4%*%weight4)*C*C


##GOOG with MO
cbind5<-cbind(GOOG,MO)
sigma5<-var(cbind5)
mu5<-c(mean(GOOG),mean(MO))
sigma5_inv<-solve(sigma5)
###weight
weight5<-as.vector(sigma5_inv%*%vector1)/as.vector(vector1T%*%sigma5_inv%*%vector1)
###return and risk
weight5T<-t(weight5)
return5<-(weight5T%*%mu5)*C
risk5<-(weight5T%*%sigma5%*%weight5)*C*C

##GOOG with ALL
cbind6<-cbind(GOOG,ALL)
sigma6<-var(cbind6)
mu6<-c(mean(GOOG),mean(ALL))
sigma6_inv<-solve(sigma6)
###weight
weight6<-as.vector(sigma6_inv%*%vector1)/as.vector(vector1T%*%sigma6_inv%*%vector1)
###return and risk
weight6T<-t(weight6)
return6<-(weight6T%*%mu6)*C
risk6<-(weight6T%*%sigma6%*%weight6)*C*C


##GOOG with MMM
cbind7<-cbind(GOOG,MMM)
sigma7<-var(cbind7)
mu7<-c(mean(GOOG),mean(MMM))
sigma7_inv<-solve(sigma7)
###weight
weight7<-as.vector(sigma7_inv%*%vector1)/as.vector(vector1T%*%sigma7_inv%*%vector1)
###return and risk
weight7T<-t(weight7)
return7<-(weight7T%*%mu7)*C
risk7<-(weight7T%*%sigma7%*%weight7)*C*C

##MO with ALL
cbind8<-cbind(MO,ALL)
sigma8<-var(cbind8)
mu8<-c(mean(MO),mean(ALL))
sigma8_inv<-solve(sigma8)
###weight
weight8<-as.vector(sigma8_inv%*%vector1)/as.vector(vector1T%*%sigma8_inv%*%vector1)
###return and risk
weight8T<-t(weight8)
return8<-(weight8T%*%mu8)*C
risk8<-(weight8T%*%sigma8%*%weight8)*C*C

##MO with MMM
cbind9<-cbind(MO,MMM)
sigma9<-var(cbind9)
mu9<-c(mean(MO),mean(MMM))
sigma9_inv<-solve(sigma9)
###weight
weight9<-as.vector(sigma9_inv%*%vector1)/as.vector(vector1T%*%sigma9_inv%*%vector1)
###return and risk
weight9T<-t(weight9)
return9<-(weight9T%*%mu9)*C
risk9<-(weight9T%*%sigma9%*%weight9)*C*C


##ALL with MMM
cbind10<-cbind(ALL,MMM)
sigma10<-var(cbind10)
mu10<-c(mean(ALL),mean(MMM))
sigma10_inv<-solve(sigma10)
###weight
weight10<-as.vector(sigma10_inv%*%vector1)/as.vector(vector1T%*%sigma10_inv%*%vector1)
###return and risk
weight10T<-t(weight10)
return10<-(weight10T%*%mu10)*C
risk10<-(weight10T%*%sigma10%*%weight10)*C*C

#three stocks

