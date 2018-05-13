setwd("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset")
#https://www.kaggle.com/kardesh/bitcoin-blockchain-analysis/notebook
#https://www.kaggle.com/tyvirk/bitcoin-historical-regression-analysis/code

install.packages("knitr")
install.packages("pander")
install.packages("tidyverse")
install.packages("broom")
install.packages("scatterplot3d")
install.packages("DataCombine")
install.packages("corrplot")
install.packages("caret")

#unit testing and statistical testing
#john rogers

library(knitr)
library(pander)
library(tidyverse)
library(broom)
library(scatterplot3d)
library(DataCombine)
library(corrplot)
library(caret)
library(anytime)

BitcoinData <- read.csv("bitcoin_dataset.csv", stringsAsFactors = F)

dim(BitcoinData)
#PRE-PROCESSING
BitcoinData$Days <- 1:nrow(BitcoinData) #ADD COLUMN FOR COUNT OF DAYS FOR EACH ROW
BitcoinData$Date <- as.Date(BitcoinData$Date) #FORMAT DATE
BitcoinData2 <- subset(BitcoinData, BitcoinData$btc_median_confirmation_time>0) #SUBSET OF DATA THAT IS CLEAN

plot(df$Blockhash, df$Hash.Number.of.Transactions)

#HISTORICAL BITCOIN VALUE
ggplot(BitcoinData, aes(BitcoinData$Date, BitcoinData$btc_market_price))+
  geom_point(color="firebrick")+
  ggtitle('Bitcoin Value vs. Time')+
  theme(plot.title = element_text(size=20, face="bold",
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Date", y="USD")+
  theme(axis.text.x=element_text(angle=50, vjust=0.5))+
  theme(panel.background=element_rect(fill= 'grey75'))

#CORRELATION BETWEEN VARIABLES
dim(BitcoinData)
cor <- cor(BitcoinData[,c(2:4,6:25)])

colnames(cor) <- c("MarketPrice", "TotalBTC", "MarketCap", "BlocksSize","AvgBlockSize",
                   "OrphanedBlocks", "Ntransactions","MedConfirmTime","HashRate","BTCDifficulty",
                   "MinersRevenue","TransactionFees","CostPerTransaction%","CostPerTransaction",
                   "NumUniqueAddresses","NumTransactions","NumTansactionsTotal","TransactionsExPop",
                   "TransactionsExLong100","OutputVolume","EstTransactionVolume","EstTransactionVolUSD",
                   "Days")
rownames(cor) <- c("MarketPrice", "TotalBTC", "MarketCap", "BlocksSize","AvgBlockSize","OrphanedBlocks",
                   "Ntransactions","MedConfirmTime","HashRate","BTCDifficulty","MinersRevenue",
                   "TransactionFees","CostPerTransaction%","CostPerTransaction","NumUniqueAddresses",
                   "NumTransactions","NumTansactionsTotal","TransactionsExPop","TransactionsExLong100",
                   "OutputVolume","EstTransactionVolume","EstTransactionVolUSD","Days")
corrplot(cor, method = "square", tl.srt = 70, tl.col = "black", 
         tl.cex=0.4, title = "Correlation of Variables", mar=c(0,0,1,0))

#Market Price vs. Market Cap
ggplot(BitcoinData2, aes(BitcoinData2$btc_market_cap, BitcoinData2$btc_market_price)) + 
  geom_point(color="firebrick") +
  ggtitle('BTC Market Capitalization vs. Market Price') +
  theme(plot.title = element_text(size=19.5, face="bold", 
  margin = margin(10, 0, 10, 0)))+
  labs(x="Market Cap (USD)", y="Market Price (USD)")+
  theme(axis.text.x=element_text(angle=50, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  stat_smooth(method = "lm",  formula = y ~ x, col = "yellow")


#REGRESSION MODEL - MARKET PRICE & EST TRANS VOL (USD)
lmfit2 <- 
  lm(BitcoinData2$btc_market_price~poly(BitcoinData2$btc_estimated_transaction_volume_usd, 2))
panderOptions("digits", 2)
pander(lmfit2, caption = "Linear Model: Market Price ~ Estimated Transaction Volume (USD) Squared")

R2=summary(lmfit2)$r.squared
cat("R-Squared =", R2)

##Residuals
plot(lmfit2, pch=16, which=1)

#REGRESSION MODEL  - the model is bottom heavy in both the x and y direction a log transform should help.
lmfit2b <- 
  lm(log(BitcoinData2$btc_market_price)~log(BitcoinData2$btc_estimated_transaction_volume_usd))

panderOptions("digits", 2)
pander(lmfit2b, caption = "Linear Model: Market Price ~ Estimated Transactions Volume(USD) Squared")
R22=summary(lmfit2)$r.squared
cat("R-Squared=", R22)

par(mfrow=c(2,2))
plot(lmfit2b)

##Market Price ~ Miners Revenue 
ggplot(BitcoinData2, aes(BitcoinData2$btc_miners_revenue/1000000, BitcoinData2$btc_market_price)) + 
  geom_point(color="firebrick") +
  ggtitle('BTC Miners Revenue vs. Market Price') +
  theme(plot.title = element_text(size=19.5, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Miners Revenue MM (USD)", y="Market Price (USD)")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  stat_smooth(method = "lm",  formula = y ~ poly(x,2), col = "yellow")

##Regression Model Summary ***HERE***
lmfit3<-lm(BitcoinData2$btc_market_price~poly(BitcoinData2$btc_miners_revenue,2))
panderOptions("digits", 2)
pander(lmfit3, caption = "Linear Model: Market Price ~ Miners Revenue (USD) Squared")
R3=summary(lmfit3)$r.squared
cat("R-Squared = ", R3)

##Residuals

plot(lmfit3, pch=16, which=1)


#Residuals have clustering, and not the best overall dispersion including heteroscedasticity.


##Difficulty vs. Market Price
ggplot(BitcoinData2, aes(BitcoinData2$btc_difficulty/1000000000, BitcoinData2$btc_market_price)) + 
  geom_point(color="firebrick") +
  ggtitle('BTC Difficulty vs. Market Price') +
  theme(plot.title = element_text(size=19.5, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Block Find Difficulty (Billions)", y="Market Price (USD)")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  stat_smooth(method = "lm",  formula = y ~ poly(x,2), col = "yellow")

##Regression Model Summary
lmfit4<-lm(BitcoinData2$btc_market_price~poly(BitcoinData2$btc_difficulty,2))
panderOptions("digits", 2)
pander(lmfit4, caption = "Linear Model: Market Price ~ Block Find Difficulty Squared")
R4=summary(lmfit4)$r.squared
cat("R-Squared = ", R4)
##Residuals
plot(lmfit4, pch=16, which=1)

#Interesting patterns formed, not random dispersion.


##Hash Rate vs. Market Price
ggplot(BitcoinData2, aes(BitcoinData2$btc_hash_rate/1000000, BitcoinData2$btc_market_price)) + 
  geom_point(color="firebrick") +
  ggtitle('BTC Hash Rate vs. Market Price') +
  theme(plot.title = element_text(size=19.5, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Hash Rate (Millions)", y="Market Price (USD)")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  stat_smooth(method = "lm",  formula = y ~ poly(x,2), col = "yellow")

##Regression Model Details
lmfit5<-lm(BitcoinData2$btc_market_price~poly(BitcoinData2$btc_hash_rate,2))
panderOptions("digits", 2)
pander(lmfit2b, caption = "Linear Model: Market Price ~ Hash Rate")
R5=summary(lmfit5)$r.squared
cat("R-Squared = ", R5)

##Residuals
plot(lmfit5, pch=16, which=1)

#Some heteroscedasticity, but relatively flat line. 


#What is the signifigance of these variables to the market price?

##All variables
lmBTCm <- lm(BitcoinData2$btc_market_price~., BitcoinData2)

panderOptions("digits", 2)
pander(lmBTCm, caption = "Linear Model: Market Price vs. All Variables")
Rb1=summary(lmBTCm)$r.squared
cat("R-Squared = ", Rb1)

##Residuals
plot(lmBTCm, pch=16, which=1)


#Most of the volume is located in the lesser x-region, but the trend line relatively flat.

##Highly Correlated Variables only
lmBTCm2 <- lm(BitcoinData2$btc_market_price~BitcoinData2$btc_market_cap+BitcoinData2$btc_hash_rate+BitcoinData2$btc_difficulty+BitcoinData2$btc_miners_revenue+BitcoinData2$btc_estimated_transaction_volume_usd, BitcoinData2)

panderOptions("digits", 2)
pander(lmBTCm2, caption = "Linear Model: Market Price vs. Highly Correlated Variables")
Rb2=summary(lmBTCm2)$r.squared
cat("R-Squared = ", Rb2)

#It appears that all of the highly correlated vairables to Market Price (Market Cap, Hash Rate, BTC Difficulty, Miners Revenue, and Estimated Transaction Volume USD) are significant.

##Residuals
plot(lmBTCm2, pch=16, which=1)


#Neither even dispersion nor a flat trend line.  Model adjustment needed.

##Narrowing Down Variables for the Model


lmBTCm3 <- lm(BitcoinData2$btc_market_price~BitcoinData2$btc_difficulty+BitcoinData2$btc_miners_revenue+BitcoinData2$btc_estimated_transaction_volume_usd, BitcoinData2)

panderOptions("digits", 2)
pander(lmBTCm3, caption = "Linear Model: Market Price vs. Highly Correlated Variables")
Rb3=summary(lmBTCm3)$r.squared
cat("R-Squared = ", Rb3)

##Residuals
plot(lmBTCm3, pch=16, which=1)

#Market Capitalization and Estimated Transaction Volume are highly correlated, only one will be included in the model.  Also, because Difficulty and Hash Rate are highly correlated, the model will only include one.

lmfit6 <- lm(BitcoinData2$btc_market_price ~ BitcoinData2$btc_estimated_transaction_volume_usd + BitcoinData2$btc_miners_revenue)

panderOptions("digits", 2)
pander(lmfit6, caption = "Linear Model: Market Price vs. Transaction Volume and Miners Revenue")
Rb6=summary(lmfit6)$r.squared
cat("R-Squared = ", Rb6)

##Residuals
plot(lmfit6, pch=16, which=1)

#Some heteroscedasticity, but the best the model so far.


##3-D Plot of the Regression Plane
s3d <- scatterplot3d(BitcoinData2$btc_estimated_transaction_volume_usd, BitcoinData2$btc_miners_revenue, BitcoinData2$btc_market_price, 
                     pch=16, highlight.3d = TRUE, type = "h", 
                     main = "Multi-Variable Regression 
                     \nMarket Price ~ Transaction Volume + Miners Revenue", 
                     xlab="Transaction Volume", 
                     ylab="Miners Revenue", 
                     zlab="Value (USD)", 
                     angle=35)
s3d$plane3d(lmfit6)


#3-dimensional plot based on the variables in the above regression model. The plane is based on the linear model lmfit6.


# Polynomial Multivariable

lmfit7 <- lm(BitcoinData2$btc_market_price ~ poly(BitcoinData2$btc_estimated_transaction_volume_usd, 2) + poly(BitcoinData2$btc_miners_revenue, degree=2))

panderOptions("digits", 2)
pander(lmfit7, caption = "Linear Model: Market Price ~ Miners Revenue Squared + Count of Transactions Squared")
R7=summary(lmfit7)$r.squared
cat("R-Squared = ", R7)

#Improvement in R-Squared value over the non-polynomial multivariable regression.

##Residuals
plot(lmfit7, pch=16, which=1)


#With polynomials introduced it results in a flatter line, and the best model so far.  Not as much dispersion as recommended.


#Train Model and Test

##Creating the Training Subset and Test Subset
set.seed(1)
train.index<-sample(1:nrow(BitcoinData2),0.80*nrow(BitcoinData2), replace=FALSE)
train <- BitcoinData2[train.index, ]
test  <- BitcoinData2[-train.index,]

##Training and Testing
#Using the same model settings as above for lmfit7.

lmtrain <- lm(btc_market_price~poly(btc_estimated_transaction_volume_usd,2) + poly(btc_miners_revenue,2) , train)
test$p1 <- predict(lmtrain,test)

ggplot(test, aes(test$Days)) + 
  geom_point(aes(y=test$btc_market_price),color="Firebrick") +
  geom_line(aes(y=test$p1), color="Blue")+
  ggtitle('BTC Prediction vs. Actuals') +
  theme(plot.title = element_text(size=16, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Days", y="Market Price (USD)")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  labs(title = paste("BTC Linear Regression Model Prediction vs. Actuals",
                     "\n\nAdj R2 = ",signif(summary(lmtrain)$adj.r.squared, 5),
                     " P =",signif(summary(lmtrain)$coef[2,4], 2)))

#Model does a good job tracking the test set.


#Percent Change

#{R, message=F, warning=F }
BitcoinData<-mutate(BitcoinData, pChange=(BitcoinData$btc_market_price-lag(BitcoinData$btc_market_price))/lag(BitcoinData$btc_market_price)*100) #Add column for percentage of change

ggplot(BitcoinData, aes(BitcoinData$Date, BitcoinData$pChange)) + 
  geom_point(color="firebrick") +
  ggtitle('BTC Percent Change vs. Time') +
  theme(plot.title = element_text(size=20, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Date", y="Pct. Change")+
  theme(axis.text.x=element_text(angle=50, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))

##Mutate Data in Percent Change
#{R, message=F, warning=F}
install.packages("zoo")
library(zoo)

BitcoinData2$AVGtransactionvolume<-BitcoinData2$btc_estimated_transaction_volume_usd
BitcoinData2$AVGminersrevenue<-BitcoinData2$btc_miners_revenue
BitcoinData2$AVGmarketcap<-BitcoinData2$btc_market_cap
BitcoinData2$pctChange<-BitcoinData2$btc_market_price
BitcoinData2$AVGtotalbitcoins<-BitcoinData2$btc_total_bitcoins

BitcoinData3<- subset(BitcoinData2, BitcoinData2$AVGminersrevenue>0)
BitcoinData3<-mutate(BitcoinData3, pctChange=(BitcoinData2$btc_market_price-lag(BitcoinData2$btc_market_price))/lag(BitcoinData2$btc_market_price)*100)
BitcoinData3<-mutate(BitcoinData3, AVGtransactionvolume=(BitcoinData2$AVGtransactionvolume-lag(BitcoinData2$AVGtransactionvolume))/lag(BitcoinData2$AVGtransactionvolume)*100)
BitcoinData3<-mutate(BitcoinData3, AVGminersrevenue=(BitcoinData2$AVGminersrevenue-lag(BitcoinData2$AVGminersrevenue))/lag(BitcoinData2$AVGminersrevenue)*100)
BitcoinData3<-mutate(BitcoinData3, AVGmarketcap=(BitcoinData2$AVGmarketcap-lag(BitcoinData2$AVGmarketcap))/lag(BitcoinData2$AVGmarketcap)*100)
BitcoinData3<-mutate(BitcoinData3, AVGtotalbitcoins=(BitcoinData2$AVGtotalbitcoins-lag(BitcoinData2$AVGtotalbitcoins))/lag(BitcoinData2$AVGtotalbitcoins)*100)

ggplot(BitcoinData2, aes(BitcoinData3$Date, BitcoinData3$pctChange)) + 
  geom_point(color="firebrick") +
  ggtitle('BTC Percent Change vs. Time') +
  theme(plot.title = element_text(size=20, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Date", y="Pct. Change")+
  theme(axis.text.x=element_text(angle=50, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))

##Correlation Analysis of Transformed Variables
cor2 <- cor(BitcoinData3[c(2:2167),c(26:30)]) #selecting variables to include in correlation analysis

colnames(cor2) <- c("% Change Transx Volume", "% Change Miner Rev.","% Change Market Cap", "% Change Market Price","Change Total Coins")
rownames(cor2) <- c("% Change Transx Volume", "% Change Miner Rev.","% Change Market Cap","% Change Market Price", "Change Total Coins")

corrplot(cor2, method = "square",  tl.srt = 50, tl.col = "black", tl.cex = 0.6, title = "Correlation of Variables", mar=c(0,0,1,0))


#Percent change in market price is highly correlated with percent change in market capitalization.


#What is the significance of these variables to the percent change in market price?

lmfit8 <- lm(pctChange ~ AVGmarketcap+AVGtotalbitcoins, BitcoinData3)

panderOptions("digits", 2)
pander(lmfit8, caption = "Linear Model: Market Price Change ~ Market Cap Change + Total Coins Change")
R8=summary(lmfit8)$r.squared
cat("R-Squared = ", R8)

##Residuals
plot(lmfit8, pch=16, which=1)


#Model is flat with no obvious pattern other than a few outliers causing low dispersion.  Will move forward with this model.


#Train Model and Test Percent Change

##Creating the training subset and test subset
set.seed(1)
train2.index<-sample(1:nrow(BitcoinData3),0.90*nrow(BitcoinData3), replace=FALSE)
train2 <- BitcoinData3[train2.index, ]
test2  <- BitcoinData3[-train2.index,]

##Training and testing
lmtrain2 <- lm(pctChange~AVGmarketcap+AVGtotalbitcoins, train2)
test2$p1 <- predict(lmtrain2,test2)
error=mean(abs(test2$p1-test2$pctChange))
cat("Mean Error = ", error)

#Very small amount of error.


ggplot(test2, aes(test2$Date)) + 
  geom_point(aes(y=test2$pctChange),color="Firebrick") +
  geom_line(aes(y=test2$p1), color="Blue")+
  ggtitle('') +
  theme(plot.title = element_text(size=10, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Time", y="Percent Change in Value")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  labs(title = paste("BTC Market Price % Change ~ BTC Market Cap % Change + Total Coins % Change",
                     "\n\nAdj R2 = ",signif(summary(lmtrain2)$adj.r.squared, 5),
                     " P =",signif(summary(lmtrain2)$coef[2,4], 2)))
