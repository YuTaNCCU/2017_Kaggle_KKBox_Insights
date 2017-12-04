#1.join members.csv <- transactions.csv
#2.輸出檔案 'j1.csv' : 15,883,148 rows x 16 variables

library(dplyr)
library(data.table)
setwd("~/Desktop/KKBoxChurnPrediction")

#t <- fread('train.csv', sep = ",", header=T, stringsAsFactors = T)
m <- fread('members.csv', sep = ",", header=T, stringsAsFactors = T)
s <- fread('transactions.csv', sep = ",", header=T, stringsAsFactors = T)

j1 <- m %>%
  left_join(s, by='msno') %>%
  filter(transaction_date > '20120101' & transaction_date < '20170401')%>%
  arrange( msno, transaction_date)
fwrite(j1 , file = 'j1.csv', append = FALSE, quote = "auto")

#產生部份數據（十萬筆）
a <- j1[1:50000,] 
b <- j1[ (15883148-50000+1) :15883148, ]
fwrite(rbind(a,b) , file = 'j1_part.csv', append = FALSE, quote = "auto")

#j1 <- fread('j1.csv', sep = ",", header=T, stringsAsFactors = T)



"""
j1 <- t %>%
  left_join(m, by='msno') %>%
left_join(s, by='msno') %>%
arrange(msno,transaction_date)
fwrite(j1 , file = 'j1.csv', append = FALSE, quote = "auto")

j2 <- j1 %>%
  group_by(is_churn,msno) %>% 
  arrange(transaction_date)
a2 <- j2[1:50000,] 
b2 <- j2[ (15883148-50000+1) :15883148, ]

j3 <- j1 %>%
  group_by(is_churn, msno) %>% 
  arrange(msno, transaction_date)
a3 <- j3[1:50000,] 
b3 <- j3[ (15883148-50000+1) :15883148, ]

j4 <- j1 %>%
  group_by(is_churn, msno) %>% 
  arrange(is_churn, msno, transaction_date)
a4 <- j4[1:50000,] 
b4 <- j4[ (15883148-50000+1) :15883148, ]
#fwrite(rbind(a4,b4) , file = 'j1_part.csv', append = FALSE, quote = "auto")

j5 <- j1 %>%
  arrange(is_churn, msno, transaction_date)
a5 <- j5[1:50000,] 
b5 <- j5[ (15883148-50000+1) :15883148, ]

"""