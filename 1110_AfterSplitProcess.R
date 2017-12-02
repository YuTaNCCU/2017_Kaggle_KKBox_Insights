#因為split軟體在分割後保留了Ｃolumn names，故需移除之
#結果速度不夠快，使用split軟體分割較快

n1 <- '1_members'
n2 <- '2_train'
n3 <- '3_trans'
n4 <- '4_ulogs'
n5 <- '5_sampl'


setwd( paste("~/Desktop/KKBoxChurnPrediction/",n1,sep = "") )
for(i in 12:19){
  inputfilname <-paste( substring(n1,3,nchar(n1)),'_',i,'.csv',sep = "")
  a<-read.csv(inputfilname,header=T)
  outputfilname <- paste( n1,'_',i,'.csv',sep = "")
  write.table(a,outputfilname, row.names = F, col.names=F, quote = F,sep=',')
  print( paste(i,':successfully processed' ))
}

