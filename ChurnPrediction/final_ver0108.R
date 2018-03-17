#packages
install.packages('adabag')
library(dplyr)
library(data.table)
library(ggplot)
library(car)
library(adabag)
library(rpart)
require(caret)
library(foreach)

#setwd("~/Desktop/DS_KKBox")

#quickly import the prcocessed data
train <- fread('data/train.csv', sep = ",", header=T, stringsAsFactors = T)
test <- fread('data/test.csv', sep = ",", header=T, stringsAsFactors = T)
tr_s <- fread('data/tr_s.csv', sep = ",", header=T, stringsAsFactors = T)
mb_s_c <- fread('data/mb_s_c.csv', sep = ",", header=T, stringsAsFactors = T)
ts_s <- fread('data/ts_s.csv', sep = ",", header=T, stringsAsFactors = T)
ul_s_c <- fread('data/ul_s_c.csv', sep = ",", header=T, stringsAsFactors = T)
joint1 <- fread('data/joint1.csv', sep = ",", header=T, stringsAsFactors = T)

##################
#
# A: Data process
#
##################

#readfile
tr <- fread('data/train.csv', sep = ",", header=T, stringsAsFactors = T)
mb <- fread('data/members_v3.csv', sep = ",", header=T, stringsAsFactors = T)
ts <- fread('data/transactions.csv', sep = ",", header=T, stringsAsFactors = T)
ul <- fread('data/user_logs.csv', sep = ",", header=T, stringsAsFactors = T)

#join the user ID (msno) to make sure all the members are in each dataframe
sp_idx_1<-tr %>% select(msno)
sp_idx_2<-mb %>% filter( msno %in% sp_idx_1$msno) %>% group_by(msno) %>% summarise()
sp_idx_3<-ts %>% filter( msno %in% sp_idx_2$msno) %>% group_by(msno) %>% summarise()
sp_idx_4<-ul %>% filter( msno %in% sp_idx_3$msno) %>% group_by(msno) %>% summarise()
#sampling 10,000 members from 1,000,000 members
set.seed(123)
sample_msno <- sample(sp_idx_4$msno, size=10000)
tr_s<-tr[tr$msno %in% sample_msno ,]
mb_s<-mb[mb$msno %in% sample_msno ,]
ts_s<-ts[ts$msno %in% sample_msno ,]
ul_s<-ul[ul$msno %in% sample_msno ,]
#make sure all the 10,000 members are all in the dataframe
length(unique(tr_s$msno))
length(unique(mb_s$msno))
length(unique(ts_s$msno))
length(unique(ul_s$msno))
#write the sampled data into files in order not to read the original files again
fwrite(tr_s, file = 'data/tr_s.csv', append = FALSE, quote = "auto")
fwrite(mb_s, file = 'data/mb_s.csv', append = FALSE, quote = "auto")
fwrite(ts_s, file = 'data/ts_s.csv', append = FALSE, quote = "auto")
fwrite(ul_s, file = 'data/ul_s.csv', append = FALSE, quote = "auto")

#Descriptive Statistics
glimpse(tr_s)
summary(tr_s)
glimpse(mb_s)
summary(mb_s)
glimpse(ts_s)
summary(ts_s)
glimpse(ul_s)
summary(ul_s)

#clean the missing values and the outliers
ul_s_c <- ul_s %>%  #ul_s$total_secs has many huge values, so I modified the rows
  mutate(total_secs = ifelse(ul_s$total_secs <=0 , 0.000001,  #not 0, for scaling reason 
                             ifelse(ul_s$total_secs > 86400, 86400, ul_s$total_secs ) ) )
mb_s_c <- mb_s %>%  #3 col have many 0 ,and reg_init_time is meaningless, so I delete them
  select( -c(bd, gender, expiration_date, registration_init_time) )
fwrite(mb_s_c, file = 'data/mb_s_c.csv', append = FALSE, quote = "auto")
fwrite(ul_s_c, file = 'data/ul_s_c.csv', append = FALSE, quote = "auto")

#choode and create features
ts_s_f <- ts_s %>%
  group_by(msno) %>%
  summarise(
    sum_pay = round( sum(actual_amount_paid) ,2),
    count_trans = length( transaction_date [is_cancel==0 ] ), #交易次數，扣除掉該次交易是取消訂閱的
    count_cancel = length( is_cancel[is_cancel==1 ] ),  #曾經取消的次數
    mean_duration = round( mean(payment_plan_days) ,2),     #平均訂閱的天數
    total_trans_period =  max(sapply(membership_expire_date, #交易的總時間 （ 最後一次交易日期 - 第一次交易日期）
                                     function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" ))) -
      min(sapply(transaction_date, 
                 function(x) as.Date(as.character(x), "%Y%m%d", origin = "1990-01-01" )))
  )

ul_s_c_f <- ul_s_c %>%
  group_by(msno) %>%
  summarise(
    sum_num_25 = round( sum(num_25) ,2),
    sum_num_50 = round( sum(num_50) ,2),
    sum_num_75 = round( sum(num_75) ,2),
    sum_num_985 = round( sum(num_985) ,2),
    sum_num_100 = round( sum(num_100 >0) ,2),
    sum_total_secs = round( sum(total_secs) ,2),
    count_date = n()
  )
fwrite(ts_s_f, file = 'data/ts_s_f.csv', append = FALSE, quote = "auto")
fwrite(ul_s_c_f, file = 'data/ul_s_c_f.csv', append = FALSE, quote = "auto")

#join the data set
joint1 <- tr_s %>% 
  left_join(mb_s_c, by='msno') %>%
  left_join(ts_s_f, by='msno') %>%
  left_join(ul_s_c_f, by='msno') 
length( unique(joint1$msno) ) #to make sure joining correctly
fwrite(joint1, file = 'data/joint1.csv', append = FALSE, quote = "auto")

#split into train and test data
set.seed(321)
sample_msno <- sample(joint1$msno, size=0.2*nrow(joint1))
test<-joint1[joint1$msno %in% sample_msno, 2:ncol(joint1)]
'%!in%' <- function(x,y)!('%in%'(x,y)) #make an operator 
train<-joint1[joint1$msno %!in% sample_msno, 2:ncol(joint1)]
fwrite(test, file = 'data/test.csv', append = FALSE, quote = "auto")
fwrite(train, file = 'data/train.csv', append = FALSE, quote = "auto")


##################
#
# B: Crerate functions
#
################## 

m_null <- function(x,y){
  ramdom_value <- runif(n = nrow(x), min = 0, max = 7450)
  train  <- ifelse(ramdom_value>550, 0, 1)
  ramdom_value <- runif(n = nrow(y), min = 0, max = 7450)
  predict  <- ifelse(ramdom_value>550, 0, 1)
  return(list(train=train, predict=predict))
}

m_logit <- function(x,y){
  m_logit <- glm(is_churn ~ . , data = x, family=binomial(link="logit"))
  train <- ifelse(predict.glm(m_logit, newdata=x, type = "response" )>0.5, 1, 0 )
  predict  <- ifelse(predict.glm(m_logit, newdata=y, type = "response" )>0.5, 1, 0 )
  return(list(train=train, predict=predict))
}

m_bag <- function(x,y){
  set.seed(123)
  length <- sum(x$is_churn == 1)
  iterations     <- floor( nrow(x) / length )
  
  pred_bag_temp <- foreach(m=1:iterations,.combine=rbind) %do% {
    idx_1 <- sample(sum(x$is_churn == 1), size=length)
    idx_0 <- sample(sum(x$is_churn == 0), size=length)
    idx <- c(idx_1, idx_0)
    glm_fit <- glm(is_churn ~ . ,
                   data=x[ idx,],
                   family=binomial(logit))
    #predict all the taining data
    pred_vector_1 <- ifelse(predict.glm(glm_fit,newdata=x, type = "response" )>0.5, 1, 0 )
    #predict all the calib/testing data
    pred_vector_2 <- ifelse(predict.glm(glm_fit,newdata=y, type = "response" )>0.5, 1, 0 )
    return( list(pred_vector_1, pred_vector_2) )
  }
  train <- as.data.frame(  pred_bag_temp[,1]  ) %>%apply(1, max ) 
  predict <- as.data.frame(  pred_bag_temp[,2]  ) %>%apply(1, max ) 
  return(list(train=train, predict=predict))
}

m_rf <-  function(x,y){
  library(randomForest)
  set.seed(123)
  x_rf <- x 
  x_rf$is_churn <- as.factor(x_rf$is_churn)
  fmodel <- randomForest(is_churn ~ ., ntree=10, nodesize=6, mtry=9 , data = x_rf ,  importance=T )
  train <- predict(fmodel, newdata=x_rf)
  predict  <- predict(fmodel, newdata=y)
  return(list(train=train, predict=predict))
}

performance <- function( x,y ){
  t1 <- table(true=y$is_churn, pred=x)
  #print(t1)
  accuracy =  (t1[1,1]+t1[2,2])/sum(t1) 
  precision = t1[2,2]/(t1[2,2]+t1[1,2]) #TP/true churn(Lost) diagnose
  recall =  t1[2,2]/(t1[2,2]+t1[2,1])  #TP/true churn(Lost)  appear
  F1Measure = (precision * recall * 2) / (precision + recall)
  performance <- round(c(accuracy=accuracy, precision=precision, recall=recall, F1Measure=F1Measure),3)
  return(performance)
}

##################
#
# C: Create folds  
#
##################

fold = 5 #set how many folds
d<-rbind(train,test)

#create segment index
set.seed(12345)
flds <- createFolds( y=d$is_churn, k = fold, list = TRUE, returnTrain = FALSE)
#call the function to choose ‘the fold‘
nfcv<-function(i){
  d_test<-d[flds[[i]],]
  if(i+1<=fold){
    d_calib<-d[flds[[i+1]],]
  }else{
    d_calib<-d[flds[[i+1-fold]],]
  }
  if(i+2<=fold){
    d_train<-d[flds[[i+2]],]
  }else{
    d_train<-d[flds[[i+2-fold]],]
  }
  for (j in  1:(fold-3) ){
    if(i+3<=fold){
      d_train<-rbind(d_train,d[flds[[i+3]],])
    }else{
      d_train<-rbind(d_train,d[flds[[i+3-fold]],])
    }
  }
  return( list(d_test,d_calib,d_train) )
}

##################
#
# D: Fit all the model
#
##################

#create a empty list to put performance
perf_summary <- list()

# run k-fold 
for (fold_i in  1:fold ){
  
  data<-nfcv(fold_i)
  d_train<-data[[3]]
  d_calib<-data[[2]]
  d_test<-data[[1]]
  
  #null model
  pred_temp <- m_null(d_train, d_calib)
  perf_summary$null_train <- rbind( perf_summary$null_train, performance(pred_temp$train, d_train) )
  perf_summary$null_calib <- rbind( perf_summary$null_calib, performance(pred_temp$predict, d_calib) )
  
  #logistic regression
  pred_temp <- m_logit(d_train, d_calib)
  perf_summary$logit_train <- rbind( perf_summary$logit_train, performance(pred_temp$train, d_train) )
  perf_summary$logit_calib <- rbind( perf_summary$logit_calib, performance(pred_temp$predict, d_calib) )
  
  #begging logistic regression
  pred_temp <- m_bag(d_train, d_calib)
  perf_summary$bag_train <- rbind( perf_summary$bag_train, performance(pred_temp$train, d_train) )
  perf_summary$bag_calib <- rbind( perf_summary$bag_calib, performance(pred_temp$predict, d_calib) )
  
  #Ramdom forests
  pred_temp <- m_rf(d_train, d_calib)
  perf_summary$rf_train <- rbind( perf_summary$rf_train, performance(pred_temp$train, d_train) )
  perf_summary$rf_calib <- rbind( perf_summary$rf_calib, performance(pred_temp$predict, d_calib) )
  
  #choose the best model for test
  ps_temp <- c()
  for(ps_i in seq(2,8,by=2)){ ps_temp = c(ps_temp,perf_summary[[ps_i]][fold_i,4] )}
  print( which.max(ps_temp) )
  switch( which.max(ps_temp),
          pred_temp <- m_null(d_train, d_calib),
          pred_temp <- m_logit(d_train, d_calib),
          pred_temp <- m_bag(d_train, d_calib),
          pred_temp <- m_rf(d_train, d_calib)
  )
  perf_summary$test <- rbind( perf_summary$test, performance(pred_temp$predict, d_calib ))
  
}
#######################
#
# E: Performance
#
#######################
#mean of performance in k folds
perf_summary_mean <- data.frame()
for(i in 1:length(perf_summary)){
  perf_summary_mean <- rbind(perf_summary_mean, apply(perf_summary[[i]], 2, mean) )
}
perf_summary_mean <- cbind( perf_summary_mean,names(perf_summary) ) 
colnames(perf_summary_mean) <- c('accuracy' , 'precision' , 'recall' , 'F1Measure', 'model' )
perf_summary_mean <- cbind(model=perf_summary_mean$model, perf_summary_mean[,1:4])
colnames(perf_summary_mean)[1] <-'model' 
print( perf_summary_mean )

#plot
library(reshape2)
df<-melt(perf_summary_mean,id='model')
df$model <- factor(df$model ,levels = names(perf_summary))
ggplot(df, aes(fill=variable, y=value, x=model, label=round(value,2))) +
  geom_bar(position="dodge",stat="identity") +
  theme(legend.position = "bottom", axis.text = element_text(size = 18), 
        axis.title = element_text(size = 0)) +
  geom_text(aes(group=variable),position=position_dodge(width=0.9),size=4.3,vjust=-0.5,hjust=0.5) 

#Chisq test
anova(m_null_8000, m_logit, test = "Chisq")
#McFadden’s pseudo-R-squared
1- as.vector(logLik(m_logit)/logLik(m_null_8000))  


# Yu Ta 2018/01/08



























