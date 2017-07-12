library(httr)
library(jsonlite)
## 
## Attaching package: 'jsonlite'
## 
## The following object is masked from 'package:utils':
## 
##     View
library(lubridate)

options(stringsAsFactors = FALSE)

url  <- "https://bittrex.com/api/1/"
path <- "C:/Users/Jack/Documents/R/Crypto"

names2 <- GET(url = 'https://bittrex.com/api/v1.1/public/getmarkethistory?market=BTC-ETH')
charNames <- rawToChar(names2$content)
this.content <- fromJSON('https://bittrex.com/api/v1.1/public/getmarkethistory?market=BTC-ETH')
test.df<-as.data.frame(this.content[[3]])
Organized <- data.frame(1)
toAdd <- data.frame(1)
for(i in 1:200){
  Organized<-rbind(Organized,test.df$Price[201-i])
}
Organized<- as.data.frame(Organized[2:201,])
Money = 10;
ownedAt = 0;
Quantity = 0;
start = TRUE
wasAbove = FALSE
above = FALSE
upTrend = FALSE
mov20 = mean(Organized[(1):(20),])
mov5 = mean(Organized[(1):(5),])
Organized[1,]
if(mov20>mov5){
  wasAbove = TRUE
} else {wasAbove = FALSE}
for(i in 1:180){
  mov20= mean(Organized[(1+i):(15+i),])
  mov5= mean(Organized[(1+i):(5+i),])
  if(mov20>mov5){
    above = TRUE
  } else {above = FALSE}
  if(start){
    if(wasAbove && !above){
      Quantity = trunc(Money,Organized[1+i,])
      Money = Money - Organized[1+i,]*Quantity
      OwnedAt = Organized[1+i,]
      print(c("buying at",OwnedAt,Money,Quantity))
    } else if (!wasAbove && above){
        Money = Money + Organized[1+i,]*Quantity
        OwnedAt = 0
        Quantity = 0
        print(c("selling at",Organized[1+i,],Money,Quantity))
    }
  }
  wasAbove = above

}
Money = Money + OwnedAt*Quantity

Quantity = 0
OwnedAt = 0
