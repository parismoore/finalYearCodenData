setwd("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset")

Txtin <- read.csv("TxtIn.csv", header=TRUE)
names(Txtin)
Tx <- read.table("txin.txt", sep="\t")
View(Txtin)

df <- data.frame(Txtin)
table(df$TxID) / nrow(df)

#Split Txtin into "Training"(20%) and "Testing"(5%)
IndepenpentInput <- sample(2, nrow(Txtin), replace=TRUE, prob = c(0.2, 0.05)) 
trainInput <- Txtin[IndepenpentInput==1, ]
testInput <- Txtin[IndepenpentInput==2, ]
xbarStrat <- NA
set.seed(1234)

#for ( i in 1:1000) {xbarStrat[i] <- mean( c(df[sample(6000,30),"TxID"], df[sample,6001:10000,20), "TxID"]}) 
install.packages("ggplot2")
library("ggplot2")
library(caret)


set.seed(123)
index3 <- sample(5:nrow(Txtin), 1000)
names(index3)

sample <- createDataPartition(Txtin$value, p=.05)
names(sample)
#sample2 <- createDataPartition(Txtin$addrID, p=.05)

scatter.smooth(x=Txtin$value, y=Txtin$addrID, main="Address ~ Value")

sample3 <- createDataPartition(Txtin$value, p=.05)
sample4 <- createDataPartition(Txtin$addrID, p=.05)

bitcoindf <- Txtin[sample3, ]
sample4

scatter.smooth(x=Txtin$addrID, y=Txtin$value, main="Value ~ Address")


par(mfrow=c(1, 2))  # divide graph area in 2 columns
boxplot(Txtin$value, main="Value", sub=paste("Outlier rows: ", boxplot.stats(Txtin$value)$out)) 

#install.packages("e1071")
library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(Txtin$value), main="Density Plot: Value", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(Txtin$value), 2)))  # density plot for 'value'
polygon(density(Txtin$value), col="red")
