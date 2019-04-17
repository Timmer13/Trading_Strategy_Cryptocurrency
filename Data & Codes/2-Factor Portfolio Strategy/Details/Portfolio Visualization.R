library(ggplot2)

setwd("/Users/DreaMiTer/Desktop/Columbia University/Spring 2018/MATH 5010 - INTRO TO THE MATH OF FINANCE/Project/Momentum Strategy")

export1 <- read.csv("Export_Comparison.csv", header = T, stringsAsFactors = F)
export1$Date <- as.Date(export1$Date, format = "%m/%d/%Y")
str(export1)

ggplot(export1, aes(x = Date, group = 1)) + 
  geom_line(aes(y = Momentum, color = "Momentum")) + 
  geom_line(aes(y = Value, color = "Value")) +
  geom_line(aes(y = Combined, color = "Combined")) +
  ylab("Cumulative Return") + 
  ggtitle("Cumulative Return among strategies (1% Risk)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete(name  ="Portfolios")


export2 <- read.csv("Export_Performance.csv", header = T, stringsAsFactors = F)
export2$Date <- as.Date(export2$Date, format = "%m/%d/%Y")
str(export2)

ggplot(export2, aes(x = Date, group = 1)) + 
  geom_line(aes(y = Combined, color = "Pre-Transaction Return")) + 
  geom_line(aes(y = Post.Cost, color = "Post-Transaction Return")) +
  ylab("Cumulative Return") + 
  ggtitle("Cumulative Return of Portfolio (Scaled to 5% Risk)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete(name="Types") +
  geom_text(data=subset(export2, Date > as.Date("03/30/2018", format = "%m/%d/%Y")), 
            aes(Date,Post.Cost,label=Post.Cost), color = "black",vjust=1)


export3 <- read.csv("Export_Risk Table.csv", header = T, stringsAsFactors = F)
str(export3)

ggplot(export3, aes(x = Target.Risk, group = 1)) + 
  geom_line(aes(y = Annualized.Return, color = "Annualized Return")) + 
  geom_line(aes(y = Annualized.Risk, color = "Annualized Volatility")) +
  geom_line(aes(y = Sharpe.Ratio, color = "Sharpe Ratio")) +
  geom_point(aes(y = Annualized.Return, color = "Annualized Return")) + 
  geom_point(aes(y = Annualized.Risk, color = "Annualized Volatility")) +
  geom_point(aes(y = Sharpe.Ratio, color = "Sharpe Ratio")) +
  ylab("Percentage Rate") + xlab("Target Risk Level") + 
  ggtitle("Comparison at different Target Risk Level") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete(name="Metric")



setwd("/Users/DreaMiTer/Desktop/Columbia University/Spring 2018/MATH 5010 - INTRO TO THE MATH OF FINANCE/Project/Raw Data/Motivation")
motivation_BTC <- read.csv("BTC-USD.csv", header = T, stringsAsFactors = F)
str(motivation_BTC)
motivation_VTI <- read.csv("VTI.csv", header = T, stringsAsFactors = F)
str(motivation_VTI)
motivation_BND <- read.csv("BND.csv", header = T, stringsAsFactors = F)
str(motivation_BND)

motivation <- motivation_BTC[,c(1,6)]
colnames(motivation)[2] <- "BTC"

index3 <- NULL
for (i in 1:nrow(motivation)){
  if (motivation$Date[i] %in% motivation_VTI$Date){
    index1 <- which(motivation$Date[i] == motivation_VTI$Date)
    motivation$VTI[i] <- motivation_VTI$Adj.Close[index1]
  } else {
    motivation$VTI[i] <- NA
    index3 <- c(index3, i)
  }
  if (motivation$Date[i] %in% motivation_BND$Date){
    index2 <- which(motivation$Date[i] == motivation_BND$Date)
    motivation$BND[i] <- motivation_BND$Adj.Close[index2]
  } else {
    motivation$BND[i] <- NA
  }
}
motivation <- motivation[-index3,]
motivation$Date <- as.Date(motivation$Date, format = "%Y-%m-%d")
str(motivation)

write.csv(motivation, "motivation.csv")

######################################################################

motivation <- read.csv("motivation.csv", header = T, stringsAsFactors = F)
motivation$Date <- as.Date(motivation$Date, format = "%m/%d/%Y")
str(motivation)

ggplot(motivation, aes(x = Date)) + 
  geom_line(aes(y = BTC..Log, color = "Bitcoin")) + 
  geom_line(aes(y = VTI..Log, color = "US equity, VTI")) +
  geom_line(aes(y = BND..Log, color = "US inv grade bonds, BND")) +
  ylab("Cumulative Return (Log Scale)") + 
  ggtitle("Cumulative Return among BTC, VTI, BND (Log Scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_colour_discrete(name  ="Asset")



