setwd("~/Documents/MATH_5010/project")
library("ggplot2")
library(reshape2)
ETH <- read.csv("ETH.csv")
BTC <- read.csv("BTC.csv")
LTC <- read.csv("LTC.csv")
XRP <- read.csv("XRP.csv")
PETH <- ETH[,"close"]
PBTC <- BTC[,"close"]
PLTC <- LTC[,"close"]
PXRP <- XRP[,"close"]



GBM <- function(mu,s,d,z){#stochastic method
  return(mu*d+s*sqrt(d)*z)
}

nGBM <- function(mu,s,z){#nonstochastic method
  return(mu+s*z)
}

data1 <- PBTC
data2 <- PETH
r1 <- c()
for(i in 1:(length(data1)-1)){
  r1 <- append(r1,log(data1[i+1]/data1[i]))
}#calculate log return

r2 <- c()
for(i in 1:(length(data2)-1)){
  r2 <- append(r2,log(data2[i+1]/data2[i]))
}

mu1 <- mean(r1)
sigma1 <- sd(r1)

mu2 <- mean(r2)
sigma2 <- sd(r2)



T <- 250 #total time
delta = 1 #step size
h <- T/delta #num of steps
n=100 #num of paths
x1 <- matrix(nrow = n,ncol = h+1) 
x2 <- matrix(nrow = n,ncol = h+1) 

for (i in 1:h+1){#simulate future log return
  for(j in 1:n){
    zz <- rnorm(1)
    x1[j,i] <- GBM(mu = mu1,s = sigma1, d = delta, z=zz) #simulate using stochastic method w/ same z
    x2[j,i] <- GBM(mu = mu2,s = sigma2, d = delta, z=zz) #simulate using stochastic method w/ same z
  }
}



sim1 <- matrix(rep(data1[length(data1)],n*(h+1)),nrow = n, ncol = h+1)#create a matrix w/ init. price
sim2 <- matrix(rep(data2[length(data2)],n*(h+1)),nrow = n, ncol = h+1)#create a matrix w/ init. price
for(i in 1:n){
  for(j in 2:h+1){
    sim1[i,j] <- sim1[i,j-1]*exp(x1[i,j]) #get back price using log return
    sim2[i,j] <- sim2[i,j-1]*exp(x2[i,j]) #get back price using log return
  }
}

ndata1 <- matrix(rep(data1,n),byrow = TRUE, nrow = n) #replicate original price for n paths
total1 <- as.matrix(data.frame(ndata1,sim1)) #put original price and simulation together
rownames(total1) <- NULL
colnames(total1) <- NULL
#total 1 is the total price movement+simulated price for data 1


ndata2 <- matrix(rep(data2,n),byrow = TRUE, nrow = n) #replicate original price for n paths
total2 <- as.matrix(data.frame(ndata2,sim2)) #put original price and simulation together
rownames(total2) <- NULL
colnames(total2) <- NULL
#total 2 is the total price movement+simulated price for data 2


sim_BTC <- total1[,2001:ncol(total1)] #take out only simulated values

sim_ETH <- total2[,2001:ncol(total2)]

# Define functions

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

q_var <- matrix(nrow = 100, ncol = 21) #100 paths, 20 possible threshholds

for(i in 1:100){
  St <- -lm(log(sim_BTC[i,])~log(sim_ETH[i,]))$residual
  close.target <- 0
  close <- find.closetime(St = St, close.target = close.target)
  
  threshold <- seq(min(St),max(St),by=abs(max(St)-min(St))/20)
  for(t in 1:length(threshold)){
    lopen <- find.low.opentime(St = St, q = threshold[t])
    hopen <- find.high.opentime(St = St, q = threshold[t])
    
    p <- profit(St = St, lopen = lopen, hopen = hopen, close = close, 
                i = 1, p = 0, n = 0, coinA = PETH, coinB = PBTC)
    q_var[i,t] <- p
  }
}

plot(q_var[1,],type = "l") #sample test for first pair of simulated data

rownames(q_var) <- NULL
colnames(q_var) <- NULL

#plot to test threshhold(ignore the actual threshhold value)
q<-melt(as.matrix(q_var)) 
q$rowid <- 1:100
ggplot(q, aes(Var2, value, group=factor(rowid))) + 
  geom_line(aes(color=factor(rowid))) +
  labs(colour = "Sample",x="threshhold", y="profit",title = "Test for different threshhold")
max <- apply(q_var[, 1:21], 1, max)
mean(max)


#plot of data 1 price movement
data1_sim<-melt(as.matrix(total1)) 
data1_sim$rowid <- 1:100
ggplot(data1_sim, aes(Var2, value, group=factor(rowid))) + 
  geom_line(aes(color=factor(rowid)), show.legend=F) +
  labs(x="Time",title = "Simulated BTC price")

#plot of data 2 price movement
data2_sim<-melt(as.matrix(total2)) 
data2_sim$rowid <- 1:100
ggplot(data2_sim, aes(Var2, value, group=factor(rowid))) + 
  geom_line(aes(color=factor(rowid)), show.legend=F) +
  labs(colour = "Sample",x="Time",title = "Simulated ETH price")

