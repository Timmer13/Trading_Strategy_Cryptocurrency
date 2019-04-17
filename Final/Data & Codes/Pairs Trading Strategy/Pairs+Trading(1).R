setwd("~/Documents/MATH_5010/project")
ETH <- read.csv("ETH.csv")
BTC <- read.csv("BTC.csv")
LTC <- read.csv("LTC.csv")
XRP <- read.csv("XRP.csv")
PETH <- ETH[,"close"]
PBTC <- BTC[,"close"]
PLTC <- LTC[,"close"]
PXRP <- XRP[,"close"]
library("ggplot2")
library(reshape2)
library(tseries)

#ggplot(data,aes(x=ETH....time.., group = 1)) + 
  #geom_line(aes(y=scale(PETH),color = "ETH")) +
  #geom_line(aes(y=scale(PBTC), color = "BTC")) +
  #scale_colour_discrete(name  ="Asset") +
  #ylab("Price") + 
  #xlab("time") +
  #theme(axis.text.x=element_blank())



data <- data.frame(PETH,PBTC,PLTC,PXRP)

# Define functions
coint <- function(data1, data2){
  reg <- lm(log(data1)~log(data2))
  res <- reg$residuals[2:length(reg$residuals)]
  nres <- reg$residuals[1:length(reg$residuals)-1]
  return(1>confint(lm(res ~ nres),"nres",level=0.975)[2])
}

find.closetime <- function(St, close.target = 0){
  #Find all possible close time
  close <- c()
  for(i in 1:(length(St)-1)){
    if(St[i]<=close.target && St[i+1]>=close.target){
      close <- append(close,i)
    }
    if(St[i]>=close.target && St[i+1]<=close.target){
      close <- append(close,i)
    }
  }
  return(close)
}

find.low.opentime <- function(St, q = 0){
  #Find all possible open(low) position
  lopen <- c()
  for(i in 1:length(St)){
    if(St[i]< -q){
      lopen <- append(lopen,i)
    }
  }
  return(lopen)
}

find.high.opentime <- function(St, q = 0){
  #Find all possible open(high) position
  hopen <- c()
  for(i in 1:length(St)){
    if(St[i]> q){
      hopen <- append(hopen,i)
    }
  } 
  return(hopen)
}

profit <- function(St, lopen, hopen, close, i = 1, p = 0, n = 0, coinA, coinB){
  while(i < length(St)){
    if(i %in% lopen & n==0){ #A overvalued
      A <- -1 #Short 1 share of A
      B <- coinA[i]/coinB[i] #Long gamma share of B
      n <- 1 #Change position
    }
    if(i %in% hopen & n==0){ #B overvalued
      B <- -1 #Short 1 share of B
      A <- coinB[i]/coinA[i] #Long gamma share of A
      n <- 1 #Change position
    }
    if(i %in% close & n==1){
      p <- p +A*coinA[i] + B*coinB[i]
      n <- 0
    }
    i=i+1
    if (i == length(St) & n==1){
      p <- p +A*coinA[i] + B*coinB[i]
    }
  }
  return(p)
}

######################################################
##### Cointegration Method
total.coin <- 4
coint.matrix <- matrix(rep(NA, total.coin^2), nrow = total.coin, ncol = total.coin)

for(i in 1:4){
  for(j in i:4){
    coint.matrix[i,j] <- coint(as.numeric(data[,i]),as.numeric(data[,j]))
  }
}

coint.matrix


### Pair 1&2
#A=ETH B=BTC
St <- -lm(log(PETH)~log(PBTC))$residual
close.target <- 0
close <- find.closetime(St = St, close.target = close.target)

threshold <- seq(min(St)-sd(St)/10,max(St)+sd(St)/10,by=sd(St)/10)
q_var<- matrix(ncol = 2, nrow = length(threshold))
for(t in 1:length(threshold)){
  lopen <- find.low.opentime(St = St, q = threshold[t])
  hopen <- find.high.opentime(St = St, q = threshold[t])
  
  p <- profit(St = St, lopen = lopen, hopen = hopen, close = close, 
              i = 1, p = 0, n = 0, coinA = PETH, coinB = PBTC)
  q_var[t,1] <- threshold[t]
  q_var[t,2] <- p
  #print(p)
}

a1 <- plot(x = threshold, y = q_var[,2], type="l", ylab = "profit",main = "Cointegration Method")
max(q_var[,2])
q_var[q_var[,2]==max(q_var[,2])][1]
q_var[q_var[,2]==mean(q_var[,2])][1]
b1 <- plot(St,type = "l",xlab="time",main="Cointegration Method")


######################################################
### Pair 1&3

#A=ETH B=LTC
St <- -lm(log(PETH)~log(PLTC))$residual
close.target <- 0
close <- find.closetime(St = St, close.target = close.target)

threshold <- seq(0.3,max(St),by=0.01)
q_var<- matrix(ncol = 2, nrow = length(threshold))
for(t in 1:length(threshold)){
  lopen <- find.low.opentime(St = St, q = threshold[t])
  hopen <- find.high.opentime(St = St, q = threshold[t])
  
  p <- profit(St = St, lopen = lopen, hopen = hopen, close = close, 
              i = 1, p = 0, n = 0, coinA = PETH, coinB = PLTC)
  q_var[t,1] <- t
  q_var[t,2] <- p
  #print(p)
}

plot(x = threshold, y = q_var[,2], type="l")
q_var

b2 <- plot(St,type = "l")
abline(0,0)
abline(1,0)
abline(-1,0)



#######################################################################################

### Distance Method
total.coin <- ncol(data)
dist.matrix <- matrix(rep(NA, total.coin^2), nrow = total.coin, ncol = total.coin)

for (i in 1:4){
  data[,i+4] <- as.vector(scale(data[,i]))
  colnames(data)[i+4] <- paste0("N", colnames(data)[i], collapse = "")
}



distance <- function(data1, data2){
  return(sum((data1 - data2)^2))
}
for(i in 1:4){
  for(j in i:4){
    dist.matrix[i,j] <- distance(as.numeric(data[,i+4]),as.numeric(data[,j+4]))
  }
}

dist.matrix  # Pair 1&3, 1&4, 3&4 have the smallest distances

### Pair 1&2

#A=ETH B=BTC
St <- (data$PBTC - data$PETH)
close.target <- mean(St)
close <- find.closetime(St = St, close.target = close.target)

q_var<- c()  # total profit
threshold <- seq(min(St),max(St),by=sd(St)/10)
for(t in threshold){
  lopen <- find.low.opentime(St = St, q = t)
  hopen <- find.high.opentime(St = St, q = t)
  
  p <- profit(St = St, lopen = lopen, hopen = hopen, close = close, 
              i = 1, p = 0, n = 0, coinA = PETH, coinB = PBTC)
  q_var <- append(q_var,p)
  #print(p)
}

plot(x = threshold, y = q_var, type="l", ylab = "profit",main = "Distance Method")
which.max(q_var)
threshold[which.max(q_var)]
max(q_var)
mean(threshold)

plot(St,type = "l",xlab="time",main="Distance Method")



### Pair 1&3

#A=ETH B=LTC
St <- -(data$NPETH - data$NPLTC)
close.target <- 0
close <- find.closetime(St = St, close.target = close.target)

q_var<- c()  # total profit
threshold <- seq(0.3,3,by=0.01)
for(t in threshold){
  lopen <- find.low.opentime(St = St, q = t)
  hopen <- find.high.opentime(St = St, q = t)
  
  p <- profit(St = St, lopen = lopen, hopen = hopen, close = close, 
              i = 1, p = 0, n = 0, coinA = PETH, coinB = PLTC)
  q_var <- append(q_var,p)
  #print(p)
}

plot(x = threshold, y = q_var, type="l")
q_var

#######################################################################################

### Ratio Method
total.coin <- ncol(data)
ratio.matrix <- matrix(nrow = 4, ncol = 4)
library(tseries)
for(i in 1:4){
  for(j in 1:4){
    ratio.matrix[i,j] <- max(adf.test(data[,i+4]-data[,j+4])$p.value,adf.test(data[,j+4]-data[,i+4])$p.value)
  }
}

colnames(ratio.matrix) <- colnames(data)[1:4]
rownames(ratio.matrix) <- colnames(data)[1:4]

ratio.matrix

for (i in 1:4){
  data[,i+4] <- as.vector(scale(data[,i]))
  colnames(data)[i+4] <- paste0("N", colnames(data)[i], collapse = "")
}
ratio_test <- function(data1, data2){
  rat <- data1/data2
  n <- length(rat)
  rat1 <- rat[1:n-1]
  rat2 <- rat[2:n]
  return(lm(rat2~rat1)$coefficients["rat1"])
}
ratio <- function(data1, data2){
  return(sum((data1 / data2))/length(data1))
}
for(i in 1:4){
  for(j in 1:4){
   ratio.matrix[i,j] <- ratio_test(as.numeric(data[,i+4]),as.numeric(data[,j+4]))
  }
}

colnames(ratio.matrix) <- colnames(data)[1:4]
rownames(ratio.matrix) <- colnames(data)[1:4]

ratio.matrix

### Pair 1&2

#A=ETH B=BTC
St <- PETH/PBTC
close.target <- mean(St)
close <- find.closetime(St = St, close.target = close.target)

q_var<- c()  # total profit
threshold <- seq(min(St),max(St),by=sd(St)/10)
for(t in threshold){
  lopen <- find.low.opentime(St = St, q = t)
  hopen <- find.high.opentime(St = St, q = t)
  
  p <- profit(St = St, lopen = lopen, hopen = hopen, close = close, 
              i = 1, p = 0, n = 0, coinA = PETH, coinB = PBTC)
  q_var <- append(q_var,p)
  #print(p)
}

plot(x = threshold, y = -q_var, type="l", ylab = "profit",main = "Ratio Method")
q_var <- -q_var
threshold[which.max(q_var)]
mean(threshold)

max(q_var)

plot(St,type = "l",xlab="time",main="Ratio Method")


### Pair 1&3

#A=ETH B=LTC
St <- data$NPLTC/data$NPETH
close.target <- mean(St)
close <- find.closetime(St = St, close.target = close.target)

q_var<- c()  # total profit
threshold <- seq(min(St),max(St),by=sd(St)/10)
for(t in threshold){
  lopen <- find.low.opentime(St = St, q = t)
  hopen <- find.high.opentime(St = St, q = t)
  
  p <- profit(St = St, lopen = lopen, hopen = hopen, close = close, 
              i = 1, p = 0, n = 0, coinA = PETH, coinB = PLTC)
  q_var <- append(q_var,p)
  #print(p)
}

plot(x = threshold, y = q_var, type="l")
q_var


