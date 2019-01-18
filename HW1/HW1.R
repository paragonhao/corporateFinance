library(xts)
library(lubridate)
library(data.table)
library(xts)

ArithmeticReturn <- function(portfolio){
  return(sum(portfolio)/length(portfolio))
}

##################### import daily return data to calculate geometric ##################### 
index_ret_raw <- read.csv("sp500daily.csv",header = TRUE, sep=",")
xts_daily_ret <- xts(x=index_ret_raw$vwretd, ymd(index_ret_raw$caldt))
colnames(xts_daily_ret) <- "return";
avgdays <- length(xts_daily_ret)/42

# daily Geometric
daily_geo_ret <- prod(xts_daily_ret+1)^(1/length(xts_daily_ret)) -1 
daily_geo_ret_annualized <- (daily_geo_ret+1)^avgdays - 1

# monthly Geometric
monthly <- endpoints(xts_daily_ret, on="months",k=1)
monthly_geo <- period.apply(xts_daily_ret, INDEX = monthly, FUN = function(x) {prod(x+1) -1})
monthly_geo_ret <- prod(monthly_geo+1)^(1/length(monthly_geo)) -1 
monthly_geo_ret_annualized <- (monthly_geo_ret+1)^12 - 1

# annual Geometric
annual <- endpoints(xts_daily_ret, on="years",k=1)
annual_geo <- period.apply(xts_daily_ret, INDEX = annual, FUN = function(x) {prod(x+1) -1})
annual_geo_ret <- prod(annual_geo + 1)^(1/length(annual_geo)) -1

fiveyears <- endpoints(annual_geo, on="years",k=5)
fiveyears_geo <- period.apply(annual_geo, INDEX = fiveyears, FUN = function(x) {prod(x+1) -1})
fiveyears_geo_ret <- prod(fiveyears_geo + 1)^(1/length(fiveyears_geo)) -1
fiveyears_geo_ret_annualized <-  (1+fiveyears_geo_ret)^(1/5) -1


##################### daily return data to calculate arithmetic ##################### 

# daily arithmetic
daily_arith <- ArithmeticReturn(xts_daily_ret$return)
daily_arith_annualize <- avgdays * daily_arith

# monthly arithmetic
monthly <- endpoints(xts_daily_ret, on="months",k=1)
monthly_ret <- period.apply(xts_daily_ret, INDEX = monthly, FUN = sum)
colnames(monthly_ret) <- "return";
monthly_arith <- ArithmeticReturn(monthly_ret$return)
monthly_arith_annualize <- 12 * monthly_arith

# annual arithmetic
annual <- endpoints(xts_daily_ret, on="years", k=1)
annual_ret <- period.apply(xts_daily_ret, INDEX = annual, FUN = sum)
colnames(annual_ret) <- "return";
annual_arith <- ArithmeticReturn(annual_ret$return)

# five years arithmetic
fiveYearly <- endpoints(xts_daily_ret, on="years",k=5)
fiveYearly_ret <- period.apply(xts_daily_ret, INDEX = fiveYearly, FUN = sum)
colnames(fiveYearly_ret) <- "return";
fiveYearly_arith <- ArithmeticReturn(fiveYearly_ret$return)
fiveYearly_arith_annualized <- fiveYearly_arith/5

DT <- data.table(
  "Return Period Annualized" = c("Daily", "Monthly", "Annual", "5 years"),
  "Arithmetic Mean" = c(daily_arith_annualize,monthly_arith_annualize,annual_arith,fiveYearly_arith_annualized),
  "Geometric Mean" = c(daily_geo_ret_annualized, monthly_geo_ret_annualized, annual_geo_ret, fiveyears_geo_ret_annualized)
)
DT



## excess rate 



