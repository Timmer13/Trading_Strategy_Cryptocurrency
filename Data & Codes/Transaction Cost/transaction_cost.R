
setwd("~/Documents/MATH_5010/project")

ltc.text <- readtext("ltc.txt")
bit.text <- readtext("bitcoin.txt")
eth.text <- readtext("eth.txt")
dash.text <- readtext("dash.txt")

ltc.text2 <- ltc.text$text
bit.text2 <- bit.text$text
eth.text2 <- eth.text$text
dash.text2 <- dash.text$text


refer <- read.csv("BTC.csv", header = T)
b <- refer$Fee.per.Transaction..USD.[1:1188]

coin.value <- function(data.name){
  express <- "[0-9]{4}/[0-9]{2}/[0-9]{2}.{4}[0-9]*.?[0-9]+"
  matches <- gregexpr(pattern = express, text = data.name)
  match.temp <- regmatches(data.name, matches)
  
  date_express <- "[0-9]{4}/[0-9]{2}/[0-9]{2}"
  matches1 <- gregexpr(pattern = date_express, text = match.temp[[1]])
  match.date <- regmatches(match.temp[[1]], matches1)
  
  value_express <- ",[0-9]*.?[0-9]+"
  matches2 <- gregexpr(pattern = value_express, text = match.temp[[1]])
  match.value <- regmatches(match.temp[[1]], matches2)
  match.value <- as.character(match.value)
  match.value <- substring(match.value, first = 2)
  
  final <- as.data.frame(cbind(match.date, match.value))
  colnames(final) <- c("date", "Average transaction fee")
  return(final)
}

bit.output <- coin.value(bit.text2)
bit.final <- as.data.frame(bit.output[which(bit.output$date == "2015/11/04"):which(bit.output$date == "2018/04/02"), ])
n.b <- nrow(bit.final)


ltc.output <- coin.value(ltc.text2)
ltc.final <- as.data.frame(ltc.output[which(ltc.output$date == "2015/11/04"):which(ltc.output$date == "2018/04/02"), ])
n.l <- nrow(ltc.final)


eth.output <- coin.value(eth.text2)
eth.final <- eth.output[which(eth.output$date == "2015/11/04"):which(eth.output$date == "2018/04/02"), ]
n.e <- nrow(eth.final)


dash.output <- coin.value(dash.text2)
dash.final <- as.data.frame(dash.output[which(dash.output$date == "2015/11/04"):which(dash.output$date == "2018/04/02"), ])
n.d <- nrow(dash.final)

Coinfinal <- as.data.frame(cbind(as.character(bit.final$date), as.numeric(bit.final$`Average transaction fee`), as.numeric(ltc.final$`Average transaction fee`), as.numeric(eth.final$`Average transaction fee`), as.numeric(dash.final$`Average transaction fee`)))
colnames(Coinfinal) <- c("Date", "BIT", "LTC", "ETH", "DASH")
write.csv(Coinfinal, file = "Coin_final.csv")
