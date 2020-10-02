rm(list =ls())

library(rvest)
library(quantmod)
library(tidyverse)


#####
#####---------------------------------------------------------------------------
## Final code, used on the reports, begin:
#####---------------------------------------------------------------------------
#####


url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500 <- url %>%
        xml2::read_html() %>%
        html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
        html_table()
SP500 <- SP500[[1]]
Tix <- SP500$Symbol

rm(url, SP500)
#randomly select 5 stocks
set.seed(100)
stocks_names <- sample(Tix, 5)

# time span for last 3 years
three_year_ago <- seq(as.Date("2020-09-01"), length = 2, by = "-3 year")[2]
end <- as.Date("2020-09-01")
getSymbols(stocks_names, from = three_year_ago, to = end)

# Fetch returns

FTV <- na.omit(ClCl(get(stocks_names[1]))) # Fortive Corp
ZBH <- na.omit(ClCl(get(stocks_names[2]))) # Zimmer Biomet Holdings Inc.
ORCL <- na.omit(ClCl(get(stocks_names[3]))) # Oracle Corporation
CTXS <- na.omit(ClCl(get(stocks_names[4]))) # Citrix Systems, Inc.
XLNX <- na.omit(ClCl(get(stocks_names[5]))) # Xilinx, Inc.

# attributes the returns data to a new object
stocks_considered <- cbind(FTV, ZBH, ORCL, CTXS, XLNX)
colnames(stocks_considered) = c("FTV", "ZBH", "ORCL", "CTXS", "XLNX")

# assign total investment to variable C
C <- 1e6

# creates empty vector p
p <- c()

# creates empty data.frame to be populated with the weights for each company
# of the different sized portfolios (rows for each company-named column)
# and respective Expected Return and Risk values.!
stocks <- data.frame("FTV" = double(0),
                     "ZBH" = double(0),
                     "ORCL" = double(0),
                     "CTXS" = double(0),
                     "XLNX" = double(0),
                     "ExpReturn" = double(0),
                     "Risk" = double(0)
)


for (i in 1:3){
        # updates vector p
        p <- append(p,1) 
        #generates combinations (by3, by2, single)
        comb <- combn(1:ncol(stocks_considered), i)
        temp <- split(comb, rep(1:ncol(comb), each = nrow(comb)))
        
        # creates empty matrix for all combinations of stocks 
        summary <- matrix(nrow = ncol(comb), ncol = (ncol(stocks_considered)+2))
        colnames(summary) = c(stocks_names,"ExpReturn","Risk")
        
        # iterates for all combinations of stocks in each combination option
        for (j in 1:ncol(comb)) {
                # creates matrix based on the combinations in 'temp'
                x <- stocks_considered[,temp[[j]]] 
                # defines sigma and mu values
                sigma <- cov(x)
                mu <-sapply(x, mean)
                # Computes weight
                weight_calc <- (solve(sigma)%*%p)  / 
                        (as.numeric(t(p) %*% solve(sigma) %*% p))
                # Computes return
                return_calc <- t(weight_calc) %*% mu * C
                # Computes risk
                risk_calc <- t(weight_calc) %*% sigma %*% weight_calc * (C^2)
                
                # assigns the calculated weights to the respective columns
                summary[j,temp[[j]]] = c(t(weight_calc))
                # assigns the calculates returns and risks to the respective columns
                summary[j,6:7] = c(return_calc,risk_calc)
        }
        
        # replaces NA values with 0 
        summary <- replace_na(summary,0)
        # adds current 'summary' to the 'stocks' data.frame
        stocks <- rbind(stocks, summary)
}


min_risk <- which.min(stocks[,7])
best <- stocks[min_risk,]

#####
#####---------------------------------------------------------------------------
## Final code, used on the reports, ends.
#####---------------------------------------------------------------------------
#####

# plot options:
# 
# 
#plot------------------------------------------------------------------------------
plot(stocks[-min_risk,7],stocks[-min_risk,6],pch=1,type = "p",  col = 1, 
     xlab = "Daily Risk",
     ylab = "Daily Expected Return", main="Stock Portfolios")
points(best[7], best[6],pch = 1, col = 10, type = "p")

legend(5.8e8,1500, c("Possible portfolio", "Min-Variance Portfolio"), col = c(1,10),
       lty = c(-2, -1), pch = c(1, 1))


# another plot ---------------------------------------------------------------

min_risk <- which.min(stocks$Risk)
max_return <- which.max(stocks$ExpReturn)

stocks$col = "Options Analyzed"
min = which.min(stocks[,1]) # shows the minimum value (row)
stocks$col[min_risk] = "Lowest Risk"
stocks$col[max_return] = "Highest Return"
library(ggplot2)
ggplot(stocks, aes(x=Risk, y=ExpReturn, color = col )) + 
        geom_point() + 
        theme_test() +
        theme(legend.title = element_blank(), legend.position= c(0.85,0.20), 
              plot.title = element_text(hjust = 0.5)) +
        xlab("Portfolio Risk") + 
        ylab("Portfolio Expected Returns") +
        ggtitle("Your options")

# another plot... -------------------------------------------------------------

library(plotly) 

min_var <- stocks[which.min(stocks$Risk),]

p <- stocks %>%
ggplot(aes(x = Risk, y = ExpReturn)) +
        geom_point() +
        theme_classic() +
        labs(x = 'Portfolio Risk',
             y = 'Portfolio Returns',
             title = "Portfolio Optimization") +
        geom_point(aes(x = Risk,
                       y = ExpReturn), data = min_var, color = 'red') +
        annotate(geom = 'segment', x = 0.14, xend = 0.135,  y = 0.01, 
                 yend = 0.06, color = 'red', arrow = arrow(type = "open")) +
        annotate(geom = 'segment', x = 0.22, xend = 0.2275,  y = 0.405, 
                 yend = 0.365, color = 'red', arrow = arrow(type = "open"))


ggplotly(p)














#----------------------------------

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
##MMM,ALL and MO
cbind11<-cbind(MMM,ALL,MO)
sigma11<-var(cbind11)
mu11 <- c(mean(MMM),mean(ALL),mean(MO))
sigma_inv11<-solve(sigma11)
vectorp<-c(1,1,1)
vectorpT<-t(vectorp)
C<-1000000
###weight formula
weight11<-as.vector(sigma_inv11%*%vectorp)/as.vector(vectorpT%*%sigma_inv11%*%vectorp)
###return formula
weight11T<-t(weight11)
return11<-(weight11T%*%mu11)*C
###risk formula
risk11<-(weight11T%*%sigma11%*%weight11)*C*C


##MMM,ALL and GOOG
cbind12<-cbind(MMM,ALL,GOOG)
sigma12<-var(cbind12)
mu12 <- c(mean(MMM),mean(ALL),mean(GOOG))
sigma_inv12<-solve(sigma12)
###weight formula
weight12<-as.vector(sigma_inv12%*%vectorp)/as.vector(vectorpT%*%sigma_inv12%*%vectorp)
###return formula
weight12T<-t(weight12)
return12<-(weight12T%*%mu12)*C
###risk formula
risk12<-(weight12T%*%sigma12%*%weight12)*C*C


##MMM,ALL and BIO
cbind13<-cbind(MMM,ALL,BIO)
sigma13<-var(cbind13)
mu13 <- c(mean(MMM),mean(ALL),mean(BIO))
sigma_inv13<-solve(sigma13)
###weight formula
weight13<-as.vector(sigma_inv13%*%vectorp)/as.vector(vectorpT%*%sigma_inv13%*%vectorp)
###return formula
weight13T<-t(weight13)
return13<-(weight13T%*%mu13)*C
###risk formula
risk13<-(weight13T%*%sigma13%*%weight13)*C*C


##MMM,MO and GOOG
cbind14<-cbind(MMM,MO,GOOG)
sigma14<-var(cbind14)
mu14 <- c(mean(MMM),mean(MO),mean(GOOG))
sigma_inv14<-solve(sigma14)
###weight formula
weight14<-as.vector(sigma_inv14%*%vectorp)/as.vector(vectorpT%*%sigma_inv14%*%vectorp)
###return formula
weight14T<-t(weight14)
return14<-(weight14T%*%mu14)*C
###risk formula
risk14<-(weight14T%*%sigma14%*%weight14)*C*C


##MMM,MO and BIO
cbind15<-cbind(MMM,MO,BIO)
sigma15<-var(cbind15)
mu15 <- c(mean(MMM),mean(MO),mean(BIO))
sigma_inv15<-solve(sigma15)
###weight formula
weight15<-as.vector(sigma_inv15%*%vectorp)/as.vector(vectorpT%*%sigma_inv15%*%vectorp)
###return formula
weight15T<-t(weight15)
return15<-(weight15T%*%mu15)*C
###risk formula
risk15<-(weight15T%*%sigma15%*%weight15)*C*C


##MMM, GOOG and BIO
cbind16<-cbind(MMM,GOOG,BIO)
sigma16<-var(cbind16)
mu16 <- c(mean(MMM),mean(GOOG),mean(BIO))
sigma_inv16<-solve(sigma16)
###weight formula
weight16<-as.vector(sigma_inv16%*%vectorp)/as.vector(vectorpT%*%sigma_inv16%*%vectorp)
###return formula
weight16T<-t(weight16)
return16<-(weight16T%*%mu16)*C
###risk formula
risk16<-(weight16T%*%sigma16%*%weight16)*C*C

#ALL, MO and GOOG
cbind17<-cbind(ALL,MO,GOOG)
sigma17<-var(cbind17)
mu17 <- c(mean(ALL),mean(MO),mean(GOOG))
sigma_inv17<-solve(sigma17)
###weight formula
weight17<-as.vector(sigma_inv17%*%vectorp)/as.vector(vectorpT%*%sigma_inv17%*%vectorp)
###return formula
weight17T<-t(weight17)
return17<-(weight17T%*%mu17)*C
###risk formula
risk17<-(weight17T%*%sigma17%*%weight17)*C*C

#ALL, MO and BIO
cbind18<-cbind(ALL,MO,BIO)
sigma18<-var(cbind18)
mu18 <- c(mean(ALL),mean(MO),mean(BIO))
sigma_inv18<-solve(sigma18)
###weight formula
weight18<-as.vector(sigma_inv18%*%vectorp)/as.vector(vectorpT%*%sigma_inv18%*%vectorp)
###return formula
weight18T<-t(weight18)
return18<-(weight18T%*%mu18)*C
###risk formula
risk18<-(weight18T%*%sigma18%*%weight18)*C*C

#ALL, GOOG and BIO
cbind19<-cbind(ALL,GOOG,BIO)
sigma19<-var(cbind19)
mu19 <- c(mean(ALL),mean(GOOG),mean(BIO))
sigma_inv19<-solve(sigma19)
###weight formula
weight19<-as.vector(sigma_inv19%*%vectorp)/as.vector(vectorpT%*%sigma_inv19%*%vectorp)
###return formula
weight19T<-t(weight19)
return19<-(weight19T%*%mu19)*C
###risk formula
risk19<-(weight19T%*%sigma19%*%weight19)*C*C

#MO, GOOG and BIO
cbind20<-cbind(MO,GOOG,BIO)
sigma20<-var(cbind20)
mu20 <- c(mean(MO),mean(GOOG),mean(BIO))
sigma_inv20<-solve(sigma20)
###weight formula
weight20<-as.vector(sigma_inv20%*%vectorp)/as.vector(vectorpT%*%sigma_inv20%*%vectorp)
###return formula
weight20T<-t(weight20)
return20<-(weight20T%*%mu20)*C
###risk formula
risk20<-(weight20T%*%sigma20%*%weight20)*C*C


#Martix for stock size is 1 

     | stock         |    expected return      |     risk     |
     |-|-|-|
     |BIO           | `r return_bio`          |  `r risk_bio` |
     |GOOG          | `r return_GOOG`         |  `r risk_GOOG`|
     |MO            | `r return_MO`           |  `r risk_MO`  |
     |MMM           | `r return_MMM`          |  `r risk_MMM` |
     
     
#Matrix for stock size is 2
     
     | stocks         |    expected return      |     risk     |  weight   |
     |-|-|-|-|
     |BIO&GOOG        |    `r return1`         |  `r risk1`  | `r weight1` |
     |BIO&MO          |    `r return2`         |  `r risk2`  | `r weight2` |
     |BIO&ALL         |    `r return3`         |  `r risk3`  | `r weight3` |
     |BIO&MMM         |    `r return4`         |  `r risk4`  | `r weight4` |
     |GOOG&MO         |    `r return5`         |  `r risk5`  | `r weight5` |
     |GOOG&ALL        |    `r return6`         |  `r risk6`  | `r weight6` |
     |GOOG&MMM        |    `r return7`         |  `r risk7`  | `r weight7` |
     |MO&ALL          |    `r return8`         |  `r risk8`  | `r weight8` |
     |MO&MMM          |    `r return9`         |  `r risk9`  | `r weight9` |
     |ALL&MMM         |    `r return10`        |  `r risk10` | `r weight10`|
     
     
#Matrix for stock size is 3
     
     | stocks             |    expected return      |     risk     |  weight     |
     |-|-|-|-|
     |MMM&ALL&MO          |    `r return11`         |  `r risk11`  | `r weight11` |
     |MMM&ALL&GOOG        |    `r return12`         |  `r risk12`  | `r weight12` |
     |MMM&ALL&BIO         |    `r return13`         |  `r risk13`  | `r weight13` |
     |MMM&MO&GOOG         |    `r return14`         |  `r risk14`  | `r weight14` |
     |MMM&&MO&BIO         |    `r return15`         |  `r risk15`  | `r weight15` |
     |MMM&GOOG&BIO        |    `r return16`         |  `r risk16`  | `r weight16` |
     |ALL&MO&GOOG         |    `r return17`         |  `r risk17`  | `r weight17` |
     |ALL&MO&BIO          |    `r return18`         |  `r risk18`  | `r weight18` |
     |ALL&GOOG&BIO        |    `r return19`         |  `r risk19`  | `r weight19` |
     |MO&GOOG&BIO         |    `r return20`         |  `r risk20`  | `r weight20` |
     

#Plot
allRisk<-c(risk_bio,risk_GOOG,risk_MO,risk_ALL,risk_MMM,risk1,risk2,risk3,risk4,risk5,risk6,risk7,risk8,risk9,risk10,risk11,risk12,risk13,risk14,risk15,risk16,risk17,risk18,risk19,risk20)
allreturn<-(return_bio,return_GOOG,return_MO,return_ALL,return_MMM,return1,return2,return3,return4,return5,return6,return7,return8,return9,return10,return11,return12,return13,return14,return15,return16,return17,return18,return19,return20)

