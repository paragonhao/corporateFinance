library(ggplot2)
rawdata <- read.csv("sp500EPSANDEARNINGS.csv", header = TRUE, sep=",")

epsAndEarnings <- rawdata[,c(3,8,11,12,14)]
epsAndEarnings$mkvalt <- log(epsAndEarnings$mkvalt)
ticlist <- unique(epsAndEarnings$tic)
resultSet <- c()

for(tic in ticlist){
  # get value for a single stock 
  singleStock <- epsAndEarnings[epsAndEarnings$tic==tic,]
  year <- singleStock$fyear[3]
  eps <- singleStock[singleStock$fyear==year,]$epsfx
  avgearning <- mean(diff(singleStock$ebit)/singleStock$ebit[1:2])
  #avgearning <- mean(diff(log(singleStock)))
  mktCap <- singleStock[singleStock$fyear==year,]$mkvalt
  
  result <- c(tic, eps, avgearning,mktCap)
  resultSet <-rbind(resultSet,c(tic, eps, avgearning,mktCap))
}


resultSet <- na.omit(resultSet)
colnames(resultSet) <- c("TIC","EPS","EBIT","mktCap")
resultDF <-as.data.frame(resultSet)
sortedDF <- resultDF[order(resultDF$EBIT),]
size <- dim(sortedDF)[1]
lower <- as.integer(size * 0.1)
upper <- as.integer(size * 0.9)
quantileDF <- sortedDF[lower:upper,2:4]

indx <- sapply(quantileDF, is.factor)
quantileDF[indx] <- lapply(quantileDF[indx], function(x) as.numeric(as.character(x)))

p <- ggplot(quantileDF, aes(EBIT,EPS))
p + labs(title="SP500 EPS VS EBIT", x="Change in Earnings") + geom_point(shape=21,aes(size = mktCap), fill = "blue", colour="black")
 


