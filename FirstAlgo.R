scoreFunction <- function(period,intensity){
path <- "C:/Users/Jack/Documents/R/"

data <- read.csv("C:/Users/Jack/Documents/R/Gold.csv", header = FALSE)

names(data) <- c("Name","Date","Open","High","Low","Close")
dayCheck = 2
Multiplier = 1.03
behind <- data[1:dayCheck,]
range <- behind[,4] - behind[,5]
range<- as.data.frame(range)
toBreak <-Multiplier*mean(range[,1])
dayRange <- data[dayCheck,4]-data[dayCheck,5]
start = TRUE
upTrend = FALSE
money = 100;
ownedAt = 0;
Quantity = 0;
for(i in 1:(135-dayCheck)){
  if(abs(dayRange) > toBreak){
    if(start){
      if(data[dayCheck+i,6] > data[dayCheck-1+i,6] ){
        start = FALSE;
        upTrend = TRUE;
        Quantity = trunc(money/data[dayCheck+i,6])
        money = money - data[dayCheck+i,6]*Quantity
        ownedAt = data[dayCheck+i,6]
        print(c("buying at",ownedAt,money))
      } else {
        upTrend = FALSE;
      } 
    } else {
      if(upTrend){
        upTrend = FALSE;
        if(ownedAt != 0){
          money = money + data[dayCheck+i,6]*Quantity
          Quantity = 0
          ownedAt = 0
          print(c("selling at",data[dayCheck+i,6],money))
        }
      } else {
        upTrend = TRUE
        if(ownedAt == 0 ){
          Quantity = trunc(money/data[dayCheck+i,6])
          money = money - data[dayCheck+i,6]*Quantity
          ownedAt = data[dayCheck+i,6]
          print(c("buying at",ownedAt,money))
        }
      }
    }
  }
  behind <- data[(1+i):(dayCheck+i),]
  range <- behind[,4] - behind[,5]
  range<- as.data.frame(range)
  toBreak <- Multiplier*mean(range[,1])
  dayRange <- data[dayCheck+i,4]-data[dayCheck+i,5]
}
money = money +ownedAt*Quantity
ownedAt = 0
Quantity = 0
return(money)
}

a<-2
b<-2
c<-0
Individuals <- data.frame(a,b,c)
names(Individuals)<- c("Period","Intensity","Score")
for(j in 1:19){
  individual <- c(runif(1, 2.0, 15.0),runif(1, 1.0, 2.5),0)
  Individuals <- rbind(Individuals,individual)
}
total_score = 0
k = 0 
while(k<500){
  newIndividuals <- Individuals
  total_score = 0;
  l = 0
  for(l in 1:20){
    Individuals[l,3] = scoreFunction(Individuals[l,1],Individuals[l,2])
    total_score = total_score+Individuals[l,3]
  }

  picked = 1
  while(picked<21){
    current_Score = sample(1:total_score,1)
    m = sample(1:20,1)
      while(current_Score>0){
        if((current_Score - Individuals[m,3]) <= 0){
          place = sample(1:2,1)
          grab = 3-place
          other = sample(1:20,1)
          mutation = .05
          if(runif(1,0,1.0)>mutation){
          newIndividuals[picked,place] = Individuals[m,place]
          } else { newIndividuals[picked,place] = runif(1,1.0,3.0)}
          if(runif(1,0,1.0)>mutation){
            newIndividuals[picked,grab] = Individuals[other,grab]
          } else { newIndividuals[picked,grab] = runif(1,1.0,3.0)}

          
        }
        current_Score = current_Score - Individuals[m,3]
        if(m+1 < 21){
          m = m+1
        } else {m = 1}
        }
     picked = picked + 1
  }
  k= k+1
  if(total_score<2001){
    Individuals <- newIndividuals
  }
  for(l in 1:20){
    Individuals[l,3] = scoreFunction(Individuals[l,1],Individuals[l,2])
  }
}