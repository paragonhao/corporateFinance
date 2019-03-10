#raw_data <- read.csv("data.csv", header = TRUE, sep = ",")
library(data.table)
library(zoo)
library(ggplot2)
library(scales)

rm(list =ls())
options(max.print = 99)

# Using ssh connection to download the R data and move it to local folder using the following command

# function(sql, n = -1){
#   #setup connection
#   res <- dbSendQuery(wrds, sql)
#   dbHasCompleted(res)
# 
#   #perform fetch
#   returnData <- dbFetch(res, n)
# 
#   #clear memory
#   dbClearResult(res)
#   return(returnData)
# }
# 
# sql.stockdata <- "SELECT date, CUSIP, PERMNO, (PRC*SHROUT) AS MARKETCAP, RET FROM CRSPQ.MSF ORDER BY PERMNO,date"
# sql.stocknames <- "SELECT PERMNO, NAMEDT, NAMEENDT, CUSIP,TICKER FROM CRSPQ.DSENAMES"
# stock.data <- getData(sql.stockdata)
# stock.names <- getData(sql.stocknames)
# 
# sql.fundamentals <- "SELECT GVKEY AS GVKEY, DATADATE AS Date, NI AS Earnings,PRCC_F AS Price,
#                             ACT AS CurAssets,CH AS Cash,LCT AS CurLiabilities,DLC AS CurDebt,
# TXP AS IncTxPayable,DP AS Deprectn
# FROM COMPM.FUNDA funda"
# fundamentals <- getData(sql.fundamentals)
# 
# sql.permnoLink <- "SELECT GVKEY,LPERMNO FROM CRSPA.CCMXPF_LINKTABLE"
# permnoLink <- getData(sql.permnoLink)

fundamentals <- read.csv("fundamentals.csv", sep = ",")
permnoLink <- read.csv("permnoLink.csv", sep = ",")
stockData <- read.csv("stockData.csv", sep = ",")
stockNames <- read.csv("stockNames.csv",sep = ",")

suppressWarnings(library(dplyr))
stock.annualReturns <- stockData %>% mutate(year = format(as.Date(date), "%Y")) %>% group_by(permno,year) %>% do(data.frame(AnnualReturn = prod(1+.$ret,na.rm = TRUE)-1, MarketCap = mean(.$marketcap))) %>% mutate(date = as.Date(paste(year,"12","31",sep = "/")))

stock.annualReturns.WithTicker <- stock.annualReturns %>% left_join(stockNames) %>% filter((date>=as.Date(namedt) & date <= as.Date(nameendt)) | (as.Date(paste(year,"01","01",sep = "/")) >=as.Date(namedt) & as.Date(paste(year,"01","01",sep = "/")) <= as.Date(nameendt)))


stock.annualReturns.WithTicker.Final <- as.data.frame(stock.annualReturns.WithTicker)[,c("date","permno","cusip","ticker","AnnualReturn","MarketCap")]
stock.returns <- stock.annualReturns.WithTicker.Final%>%filter(complete.cases(.))

# Write the stock return to a csv
write.csv(stock.returns, file="finalStockReturn.csv")

#
fundamentals.formatted <- fundamentals%>%filter(complete.cases(.))%>%
  mutate(CurAssetsCh = curassets - lag(curassets),
         CashCh = cash - lag(cash),
         CurLiabCh = curliabilities - lag(curliabilities))%>%
  mutate(PriceInv = 1/price,PE = price/earnings,
         Accruals=CurAssetsCh-CashCh-CurLiabCh)%>%
  select(gvkey,date,PriceInv,PE,Accruals) 


fund.Complete <- fundamentals.formatted %>% left_join(permnoLink %>% na.omit(),by= c("gvkey"="gvkey"))%>%filter(complete.cases(.))

# Returns for apple
stock.returns%>%filter(permno=="14593")
# Fundamentals for apple
fund.Complete%>%filter(lpermno=="14593")

stock.returns <- stock.returns%>%mutate(dateReq = as.numeric(format(date,"%Y")))
fund.format <- fund.Complete%>%mutate(Date=as.Date(date))%>%
  mutate(DateReq=as.numeric(format(Date,"%Y"))-1)%>%
  distinct%>%filter(is.finite(PE))

finaldata <- stock.returns%>%
  left_join(fund.format,by =c("permno"="lpermno","dateReq"="DateReq"))%>%
  select(Date,permno,AnnualReturn,MarketCap,PriceInv,PE,Accruals)%>%
  filter(complete.cases(.))

famaMcb.TSReg <- finaldata %>% group_by(permno)%>%
  do(data.frame(avgRet = mean(.$AnnualReturn),
                betas=matrix((lm(.$AnnualReturn~.$MarketCap + .$PriceInv +
                                   .$PE + .$Accruals))$coefficients,nrow=1)))%>%
  na.omit%>%as.data.frame


lambdas <- lm(famaMcb.TSReg$avgRet~(famaMcb.TSReg$betas.2+famaMcb.TSReg$betas.3+famaMcb.TSReg$betas.4+famaMcb.TSReg$betas.5 - 1))$coefficients
names(lambdas) <- c("Lambda_MarketCap","Lambda_PriceInverse","Lambda_PERatio","Lambda_Accruals")
lambdas



