setwd("H:\\Year 4\\Software Project\\bitcoin")
BitcoinData <- read.csv("bitcoin_dataset.csv", stringsAsFactors = T)
bitstamp <- read.csv("bitstampUSD.csv", stringsAsFactors = F)

# Date : Date of observation
# btc_market_price : Average USD market price across major bitcoin exchanges.
# btc_total_bitcoins : The total number of bitcoins that have already been mined.
# btc_market_cap : The total USD value of bitcoin supply in circulation.
# btc_trade_volume : The total USD value of trading volume on major bitcoin exchanges.
# btc_blocks_size : The total size of all block headers and transactions.
# btc_avg_block_size : The average block size in MB.
# btc_n_orphaned_blocks : The total number of blocks mined but ultimately not attached to the main Bitcoin blockchain.
# btc_n_transactions_per_block : The average number of transactions per block.
# btc_median_confirmation_time : The median time for a transaction to be accepted into a mined block.
# btc_hash_rate : The estimated number of tera hashes per second the Bitcoin network is performing.
# btc_difficulty : A relative measure of how difficult it is to find a new block.
# btc_miners_revenue : Total value of coinbase block rewards and transaction fees paid to miners.
# btc_transaction_fees : The total value of all transaction fees paid to miners.
# btc_cost_per_transaction_percent : miners revenue as percentage of the transaction volume.
# btc_cost_per_transaction : miners revenue divided by the number of transactions.
# btc_n_unique_addresses : The total number of unique addresses used on the Bitcoin blockchain.
# btc_n_transactions : The number of daily confirmed Bitcoin transactions.
# btc_n_transactions_total : Total number of transactions.
# btc_n_transactions_excluding_popular : The total number of Bitcoin transactions, excluding the 100 most popular addresses.
# btc_n_transactions_excluding_chains_longer_than_100 : The total number of Bitcoin transactions per day excluding long transaction chains.
# btc_output_volume : The total value of all transaction outputs per day.
# btc_estimated_transaction_volume : The total estimated value of transactions on the Bitcoin blockchain.
# btc_estimated_transaction_volume_usd : The estimated transaction value in USD value.
 
dim(bitcoinData)
str(bitcoinData)
View(bitcoinData)
 
install.packages("ggplot2")
library(ggplot2) # Data visualization

install.packages("readr")
library(readr) 
# CSV file I/O, e.g. the read_csv function
#We can also graphically inspect the data:

install.packages("data.table")
library("data.table")

install.packages("Amelia")
#after you install a library you have to load it
library("Amelia")

#Visual representation of missing data
missmap (BitcoinData, main = "Missing values vr observed")

#list.files("../input")

install.packages("plotly")
library(plotly)
packageVersion('plotly')
#scatter plot here
p <- plot_ly(data = bitcoinData, x = ~High, y = ~Close, 
             marker = list(size = 10,
                           color = 'rgba(255, 182, 193, .9)',
                           line = list(color = 'rgba(152, 0, 0, .8)',
                                       width = 2))) %>%
  layout(title = 'Styled Scatter',
         yaxis = list(zeroline = FALSE),
         xaxis = list(zeroline = FALSE))



#end of scatter plot 
summary(bitcoinData)
str(bitcoinData)

# = = = Add columns to convert Timestamp UNIX to readable date format = = =

bitcoinData$Date.read <- as.POSIXct(bitcoinData$Date, origin = "1970-01-01", tz = "GMT")

# = = = Convert Year and Month into Factors = = = 

#bitstamp$Time.read <- as.POSIXct(bitstamp$Timestamp, origin = "1970-01-01", tz = "GMT")

#Volume of BTC from 2009 - 2017
boxplot(log(BitcoinData$btc_output_volume) ~ BitcoinData$Date,range = 0, las = 2, ylab = "Log(Volume BTC)", main = "Log(Volume BTC)/(Date) ", col = rainbow(9), ylim = c(5, 20))

boxplot(log(bitstamp$Volume_.BTC.) ~ bitstamp$Month, range = 0, ylab = "Log(Volume BTC)", main = "Log(Volume BTC)/Month cumulated across all years  *Bitstamp USD*", names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15), col = "orange")

boxplot(log(bitstamp$Volume_.Currency.) ~ bitstamp$Month, range = 0, ylab = "Log(Volume Currency)", main = "Log(Volume Currency)/Month cumulated across all years  *Bitstamp USD*", names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), at = c(1,2,3, 5,6,7, 9,10,11, 13,14,15), col = "yellow")

library(ggplot2)

ggplot(bitstamp, aes(x = Month, fill = Volume_.Currency.))+
  geom_bar(col = "white")+
  facet_wrap(~ Year)+
  ggtitle("Volume Currency each Year/Month")+
  xlab("Month")+
  ylab("Volume Currency")

ggplot(bitstamp, aes(x = Year, fill = Volume_.Currency.))+
  stat_count(aes(fill = Volume_.Currency.))+
  facet_wrap(~ Month)+
  ggtitle("Volume Currency each Month/Year")+
  xlab("Years")+
  ylab("Volume Currency")

# = = = 1st attempt to know the data >>> to be continued = = =
