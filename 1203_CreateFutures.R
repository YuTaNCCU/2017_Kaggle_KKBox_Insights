#1203_CreateFutures

library(dplyr)
library(data.table)
setwd("~/Desktop/KKBoxChurnPrediction")

#############################
### 先用一小部份的資料
#############################
p <- fread('j1_part.csv', sep = ",", header=T, stringsAsFactors = T)
length( unique(p$msno) )

NewFeat<- p %>%
  group_by(msno) %>% 
  summarise( trans_times = length( transaction_date [is_cancel==0 ] ), #交易次數，扣除掉該次交易是取消訂閱的
             ave_days = mean(payment_plan_days),
             period = as.numeric( min(as.Date('membership_expire_date',format = '%Y%m%d' )) - 
                                    max(as.Date('registration_init_time',format = '%Y%m%d' ) ) )
             )
View(NewFeat)
fwrite(NewFeat , file = 'NewFeat.csv', append = FALSE, quote = "auto")







