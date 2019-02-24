data1 <- read.csv("~/Documents/ucla/Dropbox/Quarter2/Corporate Finance/Computer Assignment/HW4/data3.csv", header = TRUE, sep = ",")
head(data1)
data1_INDL <- data1[data1$indfmt!="FS",]
library(dplyr)
complete_data <- data1_INDL[complete.cases(data1_INDL),]
complete_data <- complete_data[complete_data$at!=0,]
complete_data$lr_mkt <- (complete_data$dlc + complete_data$dltt)/complete_data$at
complete_data$lr_book <- abs((complete_data$dlc + complete_data$dltt)/(complete_data$dlc + complete_data$dltt + complete_data$ceq))

hist(complete_data$lr_mkt, xlim=c(0,2), breaks = 1000, main="Leverage Ratio Distribution (Market Value)", xlab = "Leverage To Market Ratio")
hist(complete_data$lr_book, xlim=c(0,2), breaks = 100000, main="Leverage Ratio Distribution (Book Value)",  xlab = "Leverage To Book Ratio")

industry_mkt <- complete_data %>% 
  group_by(complete_data$gsector) %>% 
  summarise(lr_mkt = mean(lr_mkt))

industry_book <- complete_data %>% 
  group_by(complete_data$gsector) %>% 
  summarise(lr_book = mean(lr_book))

boxplot(industry_mkt$lr_mkt ~ industry_mkt$`complete_data$gsector`,main="Leverage Ratio by industry (Market Value)")
boxplot(industry_book$lr_book ~ industry_book$`complete_data$gsector`,main="Leverage Ratio industry (Book Value)")

industry_book_earnings <- complete_data %>% 
  group_by(complete_data$gsector) %>% 
  summarise(earnings = sum(ebitda))
boxplot(industry_book_earnings$earnings ~ industry_book_earnings$`complete_data$gsector`, main="Earnings by industry")
