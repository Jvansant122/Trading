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

names2 <- GET(url = 'https://bittrex.com/api/v1.1/public/getmarkethistory?market=BTC-MTL')
charNames <- rawToChar(names2$content)
this.content <- fromJSON('https://bittrex.com/api/v1.1/public/getmarkethistory?market=BTC-MTL')
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
size1 = 2
size2 = 15
wasAbove = FALSE
above = FALSE
upTrend = FALSE
mov20 = mean(Organized[(1):(size1),])
mov5 = mean(Organized[(1):(size2),])
Organized[1,]
if(mov20>mov5){
  wasAbove = TRUE
} else {wasAbove = FALSE}
for(i in 1:150){
  mov20= mean(Organized[(1+i):(size1+i),])
  mov5= mean(Organized[(1+i):(size2+i),])
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

Money = 11.01749164
ownedAt = 0
Quantity = 0
start = TRUE
while(TRUE){
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
  size1 = 50
  size2 = 15
  mov20 = mean(Organized[(199-size1):(199),])
  mov5 = mean(Organized[(199-size2):(199),])
  Organized[1,]
  if(mov20>mov5 && start){
    wasAbove = TRUE
    start = FALSE
  } else if(start) {wasAbove = FALSE
    start = FALSE
  }
  mov20= mean(Organized[(200-size1):(200),])
  mov5= mean(Organized[(200-size2):(200),])
  if(mov20>mov5){
    above = TRUE
  } else {above = FALSE}
  if(wasAbove && !above){
      Quantity = trunc(Money/Organized[200,])
      Money = Money - Organized[200,]*Quantity
      OwnedAt = Organized[200,]
      print(c("buying at",OwnedAt,Money,Quantity))
    } else if (!wasAbove && above){
      Money = Money + Organized[200,]*Quantity
      OwnedAt = 0
      Quantity = 0
      print(c("selling at",Organized[200,],Money,Quantity))
    }
  wasAbove = above
}
