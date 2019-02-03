library(xts)
library(lubridate)
library(data.table)
library(reshape2)


stocks_raw <- read.csv("stocks.csv", sep=",", header = TRUE)

stocks_raw$mktCap <- abs(stocks_raw$SHROUT * stocks_raw$PRC)

perm <- unique(stocks_raw$PERMCO)

returnTable <- c()

for(i in 1:length(perm)){
  subdata <- stocks_raw[stocks_raw$PERMCO == perm[i],]
  stocks_xts <- xts(x=subdata$RET, ymd(subdata$date))
  
  annual <- endpoints(stocks_xts, on="years",k=1)
  
  annual_geo <- period.apply(stocks_xts, INDEX = annual, FUN = function(x) { 
     prod(as.double(x) + 1) -1
  })
  returnTable <- cbind(returnTable, annual_geo)
}

colnames(returnTable) <- perm

returnData <- returnTable

returnAnnualized <- c()
returnAnnualized <- sapply(returnData, FUN = function(x){
  data <- na.omit(x)
  append(returnAnnualized, as.numeric(data))
})

return <- unlist(returnAnnualized)
return <-return[return<1000]
hist(return, main = "Annualized Compounded Return", breaks = 5000, xlim=c(-1,5))


