library(data.table)
library(lubridate)

xh_hw2 <- read.csv("hw2_xh.csv", header = TRUE, sep = ",")
harry_hw2 <- read.csv("hw2_harry.csv", header = TRUE, sep = ",")
xh_hw3_raw<- read.csv("hw3_xh.csv", header = TRUE, sep = ",")
harry_hw3_raw <- read.csv("hw3_harry.csv", header = TRUE, sep = ",")

####################################################### HW 2 #######################################################################
# total_hw2 <- merge(xh_hw2, harry_hw2, by = c("date", "PERMNO"), all = T)
overlap_hw2 <- merge(xh_hw2, harry_hw2, by = c("date", "PERMNO"))

####################################################### HW3 #########################################################################
xh_hw3 <- xh_hw3_raw[substr(xh_hw3_raw$PERMNO, 1, 3)==100,]
harry_hw3 <- harry_hw3_raw[substr(harry_hw3_raw$PERMNO, 1, 3)==100,]

# total_hw3 <- merge(xh_hw3, harry_hw3, by = c("PERMNO", "year"), all = T)
overlap_hw3 <- merge(xh_hw3, harry_hw3, by = c("PERMNO", "year"))
overlap_hw3[which(overlap_hw3$ANNRET!=overlap_hw3$yr),]