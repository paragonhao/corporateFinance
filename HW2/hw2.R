library(xts)
library(lubridate)
library(data.table)

sp500_stock_data_raw <- read.csv("sp500_stock_data.csv")

result <- rep(0,41)

exdivpos <- which(!is.na(sp500_stock_data_raw$PAYDT))

n <- length(exdivpos)
counter <- 0

for(i in exdivpos){
  if( i > 20 
      && (sp500_stock_data_raw$date[i - 20] < sp500_stock_data_raw$date[i]) 
      && (sp500_stock_data_raw$date[i] < sp500_stock_data_raw$date[i+20])){
    print("TRUE")
    lowerbound <- i-20
    upperbound <- i+20
    day_range <- sp500_stock_data_raw[lowerbound:upperbound,]
    excess_ret <- as.numeric(as.character(day_range$RETX)) - as.numeric(as.character(day_range$sprtrn))
    excess_ret[is.na(excess_ret)] <- 0
    result <- result + excess_ret
    counter <- counter +1
  }
}
result <- result/counter
plot(result, x=1:41, type="l", xlab="Time", ylab="Return", main="Excess Return from -20 to +20 around Ex-Div Date")
points(result,pch=20)
abline(v=21, col="red", lwd=5, lty=2)
text(x=21,y=-0.005,"Ex-Div Date")
