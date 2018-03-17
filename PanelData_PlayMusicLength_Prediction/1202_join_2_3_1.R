#1.join members.csv <- transactions.csv
#2.輸出檔案 'j1.csv' : 15,883,148 rows x 16 variables

library(dplyr)
library(data.table)
setwd("~/Desktop/KKBoxChurnPrediction")

#t <- fread('train.csv', sep = ",", header=T, stringsAsFactors = T)
m <- fread('members_v3.csv', sep = ",", header=T, stringsAsFactors = T)
s <- fread('transactions.csv', sep = ",", header=T, stringsAsFactors = T)
s2 <- fread('/Users/yuta_mac/Desktop/KKBoxChurnPrediction/old/v2/3_trans_v2/3_trans_v2.csv', sep = ",", header=T, stringsAsFactors = T)

#結合新發布的交易資料
S <- rbind(s,s2)

#joint ， 三種方式中選擇一種：
joint1 <- m %>%  #限制一條件 得19,982,667筆  或1,963,059個使用者
  left_join(S, by='msno') %>%
  #filter(payment_plan_days==7|payment_plan_days==30|payment_plan_days==31) %>%
  #filter(bd>0) %>%
  filter(transaction_date > '20120101' & transaction_date < '20170331') %>%
  arrange( msno, transaction_date)
length( unique(joint1$msno) )

joint1 <- m %>%  #限制三條件 得9,163,948筆  或853,021個使用者
  left_join(S, by='msno') %>%
  filter(payment_plan_days==7|payment_plan_days==30|payment_plan_days==31) %>%
  filter(bd>0) %>%
  filter(transaction_date > '20120101' & transaction_date < '20170331') %>%
  arrange( msno, transaction_date)
length( unique(joint1$msno) )

joint2 <- m %>%  #限制二條件   得18,620,548筆  或1,799,503個使用者
  left_join(S, by='msno') %>%
  filter(payment_plan_days==7|payment_plan_days==30|payment_plan_days==31) %>%
  #filter(bd>0) %>%
  filter(transaction_date > '20120301' & transaction_date < '20170331') %>%
  arrange( msno, transaction_date)
length( unique(joint2$msno) )

#依據 限制二條件的結果 將2017第一季仍在使用的使用者篩選出來
msno_idx_max_med <- joint2 %>%
  group_by(msno) %>%
  summarise( max_td = max(transaction_date)) %>%
  filter(max_td>'20170101' & max_med <'20170331' )
joint3 <-  joint2[joint2$msno %in% msno_idx_max_med$msno,]

#輸出
fwrite(joint3 , file = 'joint3.csv', append = FALSE, quote = "auto")

#產生部份數據（五萬個會員）
set.seed(1)
sampleMsno <- sample(unique(joint3$msno) , size=50000)
joint3_part<-joint3[joint3$msno %in% sampleMsno,]
length( unique(joint3_part$msno) )
fwrite(joint2_part , file = 'joint3_part.csv', append = FALSE, quote = "auto")



#joint1 <- fread('joint1.csv', sep = ",", header=T, stringsAsFactors = T)

 

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