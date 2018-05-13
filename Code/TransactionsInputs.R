setwd("C:/Users/paris/Desktop/College/College/Software Project/bitcoin_dataset")

library(knitr)
library(pander)
library(tidyverse)
library(broom)
library(scatterplot3d)
library(DataCombine)
library(corrplot)
library(caret)
library(ggplot2)

BitInputs <- read.table("txin.txt", header=TRUE)
names(BitInputs)

smp_size <- floor(0.25 * nrow(BitInputs))
names(smp_size)

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(BitInputs)), size = smp_size)

train <- BitInputs[train_ind, ]
test <- BitInputs[-train_ind, ]
