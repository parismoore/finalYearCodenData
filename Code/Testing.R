install.packages("testthat")
install.packages("assertr")
library(testthat)
library(assertr)

setwd("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset")

data <- read.csv("bitcoin_dataset.csv")
data %>% 
  assertr::verify(nrow(.)>2921)
...
