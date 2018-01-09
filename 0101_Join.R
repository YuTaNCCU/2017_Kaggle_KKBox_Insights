#待做：把< 換成 <=
#待做：畫出Ｙ對Ｘj的散步圖

#篩選出
#join 所有的資料

library(dplyr)
library(data.table)
library(ggplot2)

setwd("~/Desktop/KKBoxChurnPrediction")



#readfile
mb_v3 <- fread('/Users/yuta_mac/Desktop/DS_KKBox/Data/members_v3.csv', sep = ",", header=T, stringsAsFactors = T)
ts <- fread('/Users/yuta_mac/Desktop/DS_KKBox/Data/transactions.csv', sep = ",", header=T, stringsAsFactors = T)
ts_v2 <- fread('/Users/yuta_mac/Desktop/KKBoxChurnPrediction/old/v2/3_trans_v2/3_trans_v2.csv', sep = ",", header=T, stringsAsFactors = T)
ul <- fread('/Users/yuta_mac/Desktop/DS_KKBox/Data/user_logs.csv', sep = ",", header=T, stringsAsFactors = T)
ul_v2 <- fread('/Users/yuta_mac/Desktop/KKBoxChurnPrediction/old/v2/4_logs_v2/4_logs_v2.csv', sep = ",", header=T, stringsAsFactors = T)


##################
#
# A: transactions 
#
##################

#merge transactions v1 & v2
ts_v1v2 <- ts %>% rbind(ts_v2)
fwrite(ts_v1v2 , file = 'Data/ts_v1v2.csv', append = FALSE, quote = "auto")
ts_v1v2 <-fread('Data/ts_v1v2.csv', sep = ",", header=T, stringsAsFactors = T)


#篩選在研究期間，有完整資料的users 
filter_1_msno <- ts_v1v2 %>%  #得275,068個使用者
  filter(payment_plan_days==7|payment_plan_days==30|payment_plan_days==31) %>% #方便依期數整合
  group_by(msno) %>%
  summarise(max_ts_d = max(transaction_date),
            min_ts_d = min(transaction_date)) %>%
  filter( '20170101' < max_ts_d  &  max_ts_d < '20170331' ) %>%
  filter(  min_ts_d < '20150331' )
fwrite(filter_1_msno , file = 'Data/filter_1_msno .csv', append = FALSE, quote = "auto")
filter_1_msno <-fread('Data/filter_1_msno .csv', sep = ",", header=T, stringsAsFactors = T)


filter_2_msno <- ts_v1v2 %>%  #得0個使用者
  filter(payment_plan_days==7|payment_plan_days==30|payment_plan_days==31) %>% #方便依期數整合
  group_by(msno) %>%
  summarise(max_ts_d = max(transaction_date),
            min_ts_d = min(transaction_date)) %>%
  filter( '20170101' < max_ts_d  &  max_ts_d < '20170331' ) %>%
  filter( min_ts_d < '20140331' )

min(ts_v1v2 $transaction_date)  #最小的 transaction_date 在 20150101
#[1] 20150101

filter_3_msno <- ts_v1v2 %>%  #得115,345個使用者
  filter(payment_plan_days==7|payment_plan_days==30|payment_plan_days==31) %>% #方便依期數整合
  group_by(msno) %>%
  summarise(max_ts_d = max(transaction_date),
            min_ts_d = min(transaction_date)) %>%
  filter( '20160101' < max_ts_d  &  max_ts_d < '20161231' ) %>%
  filter( min_ts_d < '20150331' )

#篩選出符合條件的ts資料
filter_1_ts <- ts_v1v2 %>% 
  filter(msno %in% filter_1_msno$msno)
fwrite(filter_1_ts  , file = 'Data/filter_1_ts.csv', append = FALSE, quote = "auto")
filter_1_ts <-fread('Data/filter_1_ts.csv', sep = ",", header=T, stringsAsFactors = T)

########################
#
# B: user_logs & member
#
########################

#篩選出符合條件的ul資料
filter_1_ul <- ul %>%
                  rbind( ul_v2 ) %>%
                  filter(msno %in% filter_1_msno$msno)
fwrite(filter_1_ul  , file = 'Data/filter_1_ul.csv', append = FALSE, quote = "auto")
filter_1_ul <- fread('Data/filter_1_ul.csv', sep = ",", header=T, stringsAsFactors = T)

#篩選出符合條件的mb資料
filter_1_mb <- mb_v3 %>%
  filter(msno %in% filter_1_msno$msno)
fwrite(filter_1_mb  , file = 'Data/filter_1_mb.csv', append = FALSE, quote = "auto")
filter_1_mb <- fread('Data/filter_1_mb.csv', sep = ",", header=T, stringsAsFactors = T)

######################################################
#
# C: Clean up the missing values and the outliers
#
######################################################
filter_1_ul_c <- filter_1_ul %>%  #ul_s$total_secs has many huge values, so I modified the rows
  mutate(total_secs = ifelse(filter_1_ul$total_secs <=0 , 0.000001,  #not 0, for scaling reason 
                             ifelse(filter_1_ul$total_secs > 86400, 86400, filter_1_ul$total_secs ) ) )
filter_1_mb_c <- filter_1_mb %>%  #2 col have many 0 ,and reg_init_time is meaningless, so I delete them
  select( -c(bd, gender, registration_init_time) )

fwrite(filter_1_ul_c, file = 'Data/filter_1_ul_c.csv', append = FALSE, quote = "auto")
fwrite(filter_1_mb_c, file = 'Data/filter_1_mb_c.csv', append = FALSE, quote = "auto")
filter_1_ul_c <- fread('Data/filter_1_ul_c.csv', sep = ",", header=T, stringsAsFactors = T)
filter_1_mb_c <- fread('Data/filter_1_mb_c.csv', sep = ",", header=T, stringsAsFactors = T)

##################
#
# D: Create Feat
#
##################

#choose a period (待做)
period_name<- list(
  c('2017_01_03','20170101','20170331'),
  c('2016_10_12','20161001','20161231') )

#Response
Y_temp_ts  <- filter_1_ts %>%
  filter('20170101'<=transaction_date  & transaction_date <= '20170331' ) %>%
  group_by(msno) %>%
  summarise( sum_pay = round( sum(actual_amount_paid) ,2) )
Y_temp_ul  <- filter_1_ul_c %>%
  filter('20170101'<= date  &  date <= '20170331' ) %>%
  group_by(msno) %>%
  summarise( sum_play_secs = round( sum(total_secs) ,2) )


#transction
X_temp_ts <- filter_1_ts %>%
  filter(transaction_date <= '20161231') %>%
  group_by(msno) %>%
  summarise(
    mean_pay = round( mean(actual_amount_paid) ,2),
    sum_pay = round( sum(actual_amount_paid) ,2),
    count_trans = length( transaction_date [is_cancel==0 ] ), #交易次數，扣除掉該次交易是取消訂閱的
    count_cancel = length( is_cancel[is_cancel==1 ] ),  #曾經取消的次數
    mean_duration = round( mean(payment_plan_days) ,2),     #平均訂閱的天數
    total_trans_period =  max(sapply(membership_expire_date, #交易的總時間 （ 最後一次交易日期 - 第一次交易日期）
                                     function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" ))) -
                          min(sapply(transaction_date, 
                                     function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" )))
  )
#user_log
X_temp_ul <- filter_1_ul_c %>%
  filter(date <= '20161231') %>%
  group_by(msno) %>%
  summarise(
    mean_num_25 = round( mean(num_25) ,2),
    mean_num_50 = round( mean(num_50) ,2),
    mean_num_75 = round( mean(num_75) ,2),
    mean_num_985 = round( mean(num_985) ,2),
    mean_num_100 = round( mean(num_100) ,2),
    mean_num_unq = round( mean(num_unq) ,2),
    mean_total_secs = round( mean(total_secs) ,2),
    count_date = n()
  )

####################
#
# E: Join data set
#
####################
Y_2017_01_03 <-filter_1_mb_c %>%
  inner_join(Y_temp_ts, by='msno') %>%  #確保每個會員都有 member 及 transction的資料
  left_join(Y_temp_ul, by='msno')  %>%   #會員可以有userlog的空值
  mutate_all(funs(ifelse(is.na(.), 0, .)))  %>%   #NA補 0
  select(msno,sum_pay, sum_play_secs) #不要member的欄位
fwrite(Y_2017_01_03, file = 'Data/Y_2017_01_03.csv', append = FALSE, quote = "auto")
  

X_2016_10_12 <- filter_1_mb_c %>%
  inner_join(X_temp_ts, by='msno') %>%  #確保每個會員都有 member 及 transction的資料
  left_join(X_temp_ul, by='msno')  %>%   #會員可以有userlog的空值
  mutate_all(funs(ifelse(is.na(.), 0, .)))  #NA補 0
fwrite(X_2016_10_12, file = 'Data/X_2016_10_12.csv', append = FALSE, quote = "auto")
#<- fread('Data/ ', sep = ",", header=T, stringsAsFactors = T)

####################
#
# E: Regression
#
####################

hist(Y_2017_01_03$sum_pay, labels = TRUE )
hist(Y_2017_01_03$sum_play_secs, labels = TRUE)


m1 <- lm( as.formula(paste( "Y_2017_01_03$sum_pay ~", 
                            paste( colnames(X_2016_10_12)[2:17], collapse= "+") ) ),
                            data = X_2016_10_12 )
m2 <- lm( as.formula(paste( "Y_2017_01_03$sum_play_secs ~", 
                            paste( colnames(X_2016_10_12)[2:17], collapse= "+") ) ),
                            data = X_2016_10_12 )
summary(m1)
summary(m2)

temp <- cbind(Y_2017_01_03[1:1000,2:3],X_2016_10_12[1:1000,2:17])

pairs(temp)

library(psych)
pairs.panels(temp)





