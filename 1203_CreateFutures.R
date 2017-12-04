#1203_CreateFutures

library(dplyr)
library(data.table)
setwd("~/Desktop/KKBoxChurnPrediction")

#############################
### 先用一小部份的資料
#############################
p <- fread('j1_part.csv', sep = ",", header=T, stringsAsFactors = T)
length( unique(p$msno) )

#將日期欄位轉變成日期格式
p2 <- mutate(  
  p,
  med2=sapply(p$membership_expire_date, function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" )) ,
  td2=sapply(p$transaction_date, function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" ))
)

#新增自行設計的欄位
NewFeat_part<- p2 %>%  
  group_by(msno) %>%
  summarise( trans_times = length( transaction_date [is_cancel==0 ] ), #交易次數，扣除掉該次交易是取消訂閱的
             cancel_times  = length( is_cancel[is_cancel==1 ] ),
             ave_days = mean(payment_plan_days),
             total_period =  max(med2) - min(td2)
  )

View(NewFeat_part)

fwrite(NewFeat_part , file = 'NewFeat_part.csv', append = FALSE, quote = "auto")

#############################
### 用全部的資料
#############################
nf <- fread('j1.csv', sep = ",", header=T, stringsAsFactors = T)
length( unique(nf$msno) )

#將日期欄位轉變成日期格式
nf2 <- mutate( 
  nf,
  med2=sapply(nf$membership_expire_date, function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" )) ,
  td2=sapply(nf$transaction_date, function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" ))
)
fwrite(nf2 , file = 'nf2.csv', append = FALSE, quote = "auto")

#新增自行設計的欄位
NewFeat<- nf2 %>%  
  group_by(msno) %>%
  summarise( trans_times = length( transaction_date [is_cancel==0 ] ), #交易次數，扣除掉該次交易是取消訂閱的
             cancel_times  = length( is_cancel[is_cancel==1 ] ),
             ave_days = mean(payment_plan_days),
             total_period =  max(med2) - min(td2)
  )

head(NewFeat)
fwrite(NewFeat , file = 'NewFeat.csv', append = FALSE, quote = "auto")

#產生部份數據（十萬筆）

idx=sample(1:nrow(NewFeat), size=1e+5)
fwrite(NewFeat [idx, ] , file = 'NewFeat_part.csv', append = FALSE, quote = "auto")







