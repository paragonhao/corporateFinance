library(xts)
library(lubridate)
library(data.table)
library(xts)

ArithmeticReturn <- function(portfolio){
  return(sum(portfolio)/length(portfolio))
}

##################### import daily return data to calculate geometric ##################### 
index_ret_raw <- read.csv("HW1/sp500daily.csv",header = TRUE, sep=",")
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
monthly_arith <- ArithmeticReturn(monthly_geo$return)
monthly_arith_annualize <- 12 * monthly_arith

# annual arithmetic
annual_arith <- ArithmeticReturn(annual_geo$return)

# five years arithmetic
fiveYearly_arith <- ArithmeticReturn(fiveyears_geo$return)
fiveYearly_arith_annualized <- fiveYearly_arith/5

DT <- data.table(
  "Return Period Annualized" = c("Daily", "Monthly", "Annual", "5 years"),
  "Arithmetic Mean" = c(daily_arith_annualize,monthly_arith_annualize,annual_arith,fiveYearly_arith_annualized),
  "Geometric Mean" = c(daily_geo_ret_annualized, monthly_geo_ret_annualized, annual_geo_ret, fiveyears_geo_ret_annualized)
)
DT

##################### Calcualte excess return ##################### 
rfmonthly <- read.csv("HW1/rfmonthlyData.csv",header = TRUE, sep=",")
colnames(rfmonthly) <- c("caldt", "ret5yr", "ret1yr", "ret30d");
rfmonthly_xts<- xts(x=rfmonthly, ymd(rfmonthly$caldt))[,-1]

ffr_raw <- read.csv("HW1/FederalFunds.csv",header = TRUE, sep=",")
ffr <- ffr_raw[-which(is.na(ffr_raw$FF_O)),]
ffr_xts <- xts(x=ffr$FF_O, ymd(ffr$date))/100

# daily sp500 return (geometric )  - monthly rf free 30days
ffr_xts_daily <- (ffr_xts + 1)^(1/avgdays) -1
daily_ecess_return <- xts_daily_ret - ffr_xts_daily 
# geometric return
daily_ecess_return_geo <- prod(daily_ecess_return$return + 1)^(1/length(daily_ecess_return)) -1
daily_ecess_return_geo_annualized <- (daily_ecess_return_geo+1)^avgdays - 1
# arithmetic return
daily_ecess_return_arith_annualized <- ArithmeticReturn(daily_ecess_return) * avgdays

# monthly sp500 return (geometric )  - monthly rf free 30days
monthly_30d <- monthly_geo$return - rfmonthly_xts$ret30d
monthly30d_geo_ret <- prod(monthly_30d$return+1)^(1/length(monthly_30d)) -1 
# geometric return
monthly30d_geo_ret_annualized <- (monthly30d_geo_ret+1)^12 - 1
# arithmetic return
monthly30d_arith_ret <- ArithmeticReturn(monthly_30d) * 12

# monthly sp500 return (geometric )  - monthly rf free 1 yr
annual <- endpoints(rfmonthly_xts, on="years",k=1)
rf1year_xts <- period.apply(rfmonthly_xts$ret1yr, INDEX = annual, FUN = function(x) {prod(x+1) -1})
monthly_1yr <- annual_geo$return - rf1year_xts$ret1yr
# geometric return
monthly1yr_geo_ret <- prod(monthly_1yr$return+1)^(1/length(monthly_1yr)) -1 
# arithmetic return 
monthly1yr_arith_ret  <-   ArithmeticReturn(monthly_1yr)

# monthly sp500 return (geometric )  - monthly rf free 5 yr
fiveyears <- endpoints(rfmonthly_xts, on="years",k=5)
rf5year_xts <- period.apply(rfmonthly_xts$ret5yr, INDEX = fiveyears, FUN = function(x) {prod(x+1) -1})
monthly_5yr <- fiveyears_geo$return - rf5year_xts$ret5yr
# geometric return
monthly5yr_geo_ret <- prod(monthly_5yr$return+1)^(1/length(monthly_5yr)) -1 
monthly5yr_geo_ret_annualize <- (monthly5yr_geo_ret+1)^(1/5) -1
# arithmetic return 
monthly5yr_arith_ret <- ArithmeticReturn(monthly_5yr)/5

excess_ret <- data.table(
  "Return Period Annualized" = c("Daily", "Monthly", "Annual", "5 years"),
  "Arithmetic Mean" = c(daily_ecess_return_arith_annualized, monthly30d_arith_ret, monthly1yr_arith_ret,monthly5yr_arith_ret),
  "Geometric Mean" = c(daily_ecess_return_geo_annualized, monthly30d_geo_ret_annualized, monthly1yr_geo_ret, monthly5yr_geo_ret_annualize)
)
excess_ret

