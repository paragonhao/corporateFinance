library(ggplot2)
rawdata <- read.csv("sp500.csv", header = TRUE, sep=",")
rawdata$epsfx<- rawdata$epsfx/rawdata$prcc_f

epsAndEarnings <- na.omit(rawdata[,c(3,4,6,7,8)])
epsAndEarnings$mkvalt <- log(epsAndEarnings$mkvalt)
ticlist <- unique(epsAndEarnings$tic)
resultSet <- c()

for(tic in ticlist){
  # get value for a single stock 
  singleStock <- epsAndEarnings[epsAndEarnings$tic==tic,]
  year <- singleStock$fyear[3]
  eps <- singleStock[singleStock$fyear==year,]$epsfx
  #avgearning <- mean(diff(singleStock$ebit)/singleStock$ebit[1:2])
  avgearning <- mean(diff(log(singleStock$ebit)))
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
p + labs(title="SP500 E/P", x="Change in Earnings") + geom_point(shape=21,aes(size = mktCap), fill = "blue", colour="black")
 
  

