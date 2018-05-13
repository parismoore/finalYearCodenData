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
library(anytime)
library(ggplot2)

df <- read.csv("BTC_df.csv", stringsAsFactors = F)

df$Days <- 1:nrow(df) #ADD COLUMN FOR COUNT OF DAYS FOR EACH ROW
df$Timestamp <- as.Date(df$Timestamp) #FORMAT DATE

#converting unix timestamp
anydate(df$Unix.Timestamp)
anydate(df$Unix.Timestamp)
as.numeric( Sys.time(df$Unix.Timestamp) )

#HISTORICAL DATA
ggplot(df, aes(df$Timestamp, df$Transactions.Output.Value))+
  geom_point(color="red")+
  ggtitle('BTC Output Value vs. Time')+
  theme(plot.title = element_text(size=20, face="bold",
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Date", y="BTC")+
  theme(axis.text.x=element_text(angle=50, vjust=0.5))+
  theme(panel.background=element_rect(fill= 'white'))

#number of Inputs over time
ggplot(df, aes(df$Timestamp, df$No.Of.Inputs)) + 
  geom_point(color="black") +
  ggtitle('Number of Outputs over Time') +
  theme(plot.title = element_text(size=19.5, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Time", y="Number of Outputs")+
  theme(axis.text.x=element_text(angle=50, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  stat_smooth(method = "lm",  formula = y ~ x, col = "yellow")

dim(df)

str(df)

ggplot(df, aes(df$Blockhash/1000000, df$blocktime)) + 
  geom_point(color="firebrick") +
  ggtitle('BTC Hash Rate vs. Market Price') +
  theme(plot.title = element_text(size=19.5, face="bold", 
                                  margin = margin(10, 0, 10, 0)))+
  labs(x="Hash Rate (Millions)", y="Market Price (USD)")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
  theme(panel.background = element_rect(fill = 'grey75'))+
  stat_smooth(method = "lm",  formula = y ~ poly(x,2), col = "yellow")

# = = = 1st set of graphs to visualize the complete data on LOG scale = = = 
df2 <- df[-c(1000:74045)]
dim(df2)



boxplot(log(df$Hash.Number.of.Transactions) ~ df$blocktime, range = 0, 
        las = 2, ylab = "Log(Volume BTC)", main = "Log(Volume of Transactions)/(Blocktime)
        & whiskers at ZERO", col = rainbow(9), ylim = c(-18, 8))

############################################################################################################
ggplot(df, aes(x = df$Timestamp, fill = df$Hash.Number.of.Transactions))+
  stat_count(aes(fill = df$Hash.Number.of.Transactions))+
  facet_wrap(~ blocktime)+
  ggtitle("Volume Currency each Month/Year")+
  xlab("Years")+
  ylab("Volume Currency")

