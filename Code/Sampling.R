setwd("C:\\Users\\paris\\Desktop\\College\\College\\Paris Moore Software Project\\Code")
setwd("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset")
#Date source - http://www.vo.elte.hu/bitcoin/zipdescription.htm 

#Txtin - list of all transaction inputs (sums sent by the users), 65714232 rows, 3 columns: 
#Split Txtin into "Training"(20%) and "Testing"(5%)
Txtin <- read.csv("TxtIn.csv", header=TRUE)

TransInput <- sample(2, nrow(Txtin), replace=TRUE, prob = c(0.10, 0.025)) 
trainInput <- Txtin[TransInput==1, ]
testInput <- Txtin[TransInput==2, ]

write.csv(trainInput, file = "trainInput.csv")
write.csv(testInput, file = "testInput.csv")

#blockhash - enumeration of all blocks in the blockchain, 277443 rows, 4 columns:
blockhash <- read.table("blockhash.txt", header=TRUE)

Blocks <- sample(2, nrow(blockhash), replace=TRUE, prob = c(0.10, 0.025)) 
trainHash <- blockhash[Blocks==1, ]
testHash <- blockhash[Blocks==2, ]

write.csv(trainHash, file = "trainHash.csv")
write.csv(testHash, file = "testHash.csv")

#txhash - transaction ID and hash pairs, 30048983 rows, 2 columns: 

library(ggplot2)
library(caret)
install.packages("bigmemory")
install.packages("bigmemory.sri")
library(bigmemory.sri)
library(bigmemory)
install.packages("data.table")
library(data.table)

txhash = fread("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset/txhash.txt")

TransHash <- createDataPartition(Txtin$value, p=.05)


bitcoindf <- Txtin[sample3, ]


TransHash <- createDataPartition(txhash, p=0.025)
trainTransHash <- txhash[TransHash==1, ]
testTransHash <- txhash[TransHash==2, ]

write.csv(trainTransHash, file = "trainTransactionHash.csv")
write.csv(testTransHash, file = "testTransactionHash.csv")

#addresses - BitCoin address IDs, 24618959 rows, 2 columns:
addresses = fread("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset/addresses.txt")

BitAddresses<- sample(2, nrow(addresses), replace=TRUE, prob = c(0.10, 0.025)) 
trainAddresses <- addresses[BitAddresses==1, ]
testAddresses <- addresses[BitAddresses==2, ]

write.csv(trainAddresses, file = "trainAddresses.csv")
write.csv(testAddresses, file = "testAddresses.csv")

#tx - enumaration of all transactions, 30048983 rows, 4 columns: 
tx = fread("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset/tx.txt")

Transactions <- sample(2, nrow(tx), replace=TRUE, prob = c(0.10, 0.025)) 
trainTX <- tx[Transactions==1, ]
testTX <- tx[Transactions==2, ]

write.csv(trainTX, file = "trainTX.csv")
write.csv(testTX, file = "testTX.csv")

#txtime - transaction timestamps (obtained from the blockchain.info site), 30048983 rows, 2 columns: 
txtime = fread("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset/txtime.txt")
TransactionTime <- sample(2, nrow(txtime), replace=T, prob = c(0.10, 0.025))
TrainTime <- txtime[TransactionTime==1, ]
TestTime <- txtime[TransactionTime==2, ]

write.csv(TrainTime, file = "TrainTime.csv")
write.csv(TestTime, file = "TestTime.csv")

#txout - list of all transaction outputs (sums received by the users), 73738345 rows, 3 columns:  
txOut = fread("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset/txout.txt")
install.packages("bit64")
library(bit64)
print(txOut)
gc()
?bit

dim(txOut)


Output <- sample(2, nrow(txOut), replace=T, prob = c(0.00000005555555, 0.00005555))
TrainOutput <- txOut[Output==1, ]
TestOutput <- txOut[Output==2, ]

write.csv(TrainOutput, file = "TrainOutput.csv")
write.csv(TestOutput, file = "TestOutput.csv")


