#1.join train.csv <- members.csv <- transactions.csv
#2.輸出檔案 'j1.csv'

library(dplyr)
library(data.table)
setwd("~/Desktop/KKBoxChurnPrediction")

t <- fread('train.csv', sep = ",", header=T, stringsAsFactors = T)
m <- fread('members.csv', sep = ",", header=T, stringsAsFactors = T)
s <- fread('transactions.csv', sep = ",", header=T, stringsAsFactors = T)

j1 <- t %>%
  left_join(m, by='msno') %>%
  left_join(s, by='msno') 

print(head(j1))
  
fwrite(j1, file = 'j1.csv', append = FALSE, quote = "auto")





