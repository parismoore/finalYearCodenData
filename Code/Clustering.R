setwd("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset")
#https://www.kaggle.com/kardesh/bitcoin-blockchain-analysis/notebook
#https://www.kaggle.com/tyvirk/bitcoin-historical-regression-analysis/code


library(bigmemory.sri)
library(bigmemory)
library(knitr)
library(pander)
library(tidyverse)
library(broom)
library(scatterplot3d)
library(DataCombine)
library(corrplot)
library(caret)
library(data.table)
library(bit64)

#reading in all train datasets and combining into a Dataframe
btc_hash <- read.csv("trainHash.csv", stringsAsFactors = F)
btc_Input <- read.csv("trainInput.csv", stringsAsFactors = F)
btc_Time = fread("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset/TrainTime.csv", stringsAsFactors = F)
btc_Output <- read.csv("TrainOutput.csv", stringsAsFactors = F)
btc_Transactions = fread("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset/trainTX.csv")
btc_Addresses <- read.csv("trainAddresses.csv", stringsAsFactors = F) 
btc_Transactions_Hash  = fread("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset/trainTransactionHash.csv", stringsAsFactors = F)

#changing Column Names in the Datasets for easier reading
colnames(btc_hash)[colnames(btc_hash)=="X"] <- "Block.ID"
colnames(btc_hash)[colnames(btc_hash)=="X000000000019D6689C085AE165831E934FF763AE46A2A6C172B3F1B60A8CE26F"] <- "Block.Hash"
colnames(btc_hash)[colnames(btc_hash)=="X1231006505"] <- "Block.Time"
colnames(btc_hash)[colnames(btc_hash)=="X1"] <- "Num.Of.Transactions"

colnames(btc_Addresses)[colnames(btc_Addresses)=="X"] <- "Address.ID"
colnames(btc_Addresses)[colnames(btc_Addresses)=="x"] <- "Address"

colnames(btc_Input)[colnames(btc_Input)=="X"] <- "NULL"
colnames(btc_Input)[colnames(btc_Input)=="TxID"] <- "Transaction.ID"
colnames(btc_Input)[colnames(btc_Input)=="addrID"] <- "Address.ID"

colnames(btc_Output)[colnames(btc_Output)=="V1"] <- "Transaction.Output.ID"
colnames(btc_Output)[colnames(btc_Output)=="V2"] <- "Transaction.Output.Address"
colnames(btc_Output)[colnames(btc_Output)=="V3"] <- "Transactions.Output.Value"
write.csv(btc_Output, file = "TrainOutput.csv")

colnames(btc_Transactions)[colnames(btc_Transactions)=="X"] <- ""
colnames(btc_Transactions)[colnames(btc_Transactions)=="x"] <- ""

colnames(btc_Transactions_Hash)[colnames(btc_Transactions_Hash)=="X"] <- ""
colnames(btc_Transactions_Hash)[colnames(btc_Transactions_Hash)=="x"] <- ""

colnames(btc_Time)[colnames(btc_Time)=="X"] <- ""
colnames(btc_Time)[colnames(btc_Time)=="x"] <- ""

colnames(btc_hash)[colnames(btc_hash)=="X"] <- ""
colnames(btc_hash)[colnames(btc_hash)=="x"] <- ""

dim(btc_Addresses)
dim(btc_hash)
dim(btc_Input)
dim(btc_Output)
dim(btc_Time)
dim(btc_Transactions)
dim(btc_Transactions_Hash)

btc_Input <- btc_Input[-c(74046:839163), ]
dim(btc_Input)

btc_Addresses <- btc_Addresses[-c(74046:1048575), ]
dim(btc_Addresses)

btc_hash <- btc_hash[-c(74046:222112), ]
dim(btc_hash)

btc_Time <- btc_Time[-c(74046:1048575)]
dim(btc_Time)

btc_Transactions <- btc_Transactions[-c(74046:1048575)]
dim(btc_Transactions)

btc_Transactions_Hash <- btc_Transactions_Hash[-c(74046:1048575)]
dim(btc_Transactions_Hash)

#combining into a dataframe
df = data.frame(btc_hash, btc_Addresses, btc_Input, btc_Output, btc_Time, btc_Transactions, btc_Transactions_Hash)
colnames(df)

#writing to Csv file for furture analysis 
write.csv(df, file = "BTC_df.csv")

dim(df)
