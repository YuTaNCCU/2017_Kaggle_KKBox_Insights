library(dplyr)
setwd("~/Desktop/KKBoxChurnPrediction")

#split column name
setwd("~/Desktop/KKBoxChurnPrediction/5_sampl")
inputfilname <-'sampl_0.csv'
a<-read.csv(inputfilname,header=T)
outputfilname <- paste('-colnam-',inputfilname) 
write.csv(a[0,],outputfilname, row.names = F,quote = F)

#numnering msno
n1 <- '1_members'
n2 <- '2_train'
n3 <- '3_trans'
n4 <- '4_ulogs'
n5 <- '5_submi'
  
setwd( paste("~/Desktop/KKBoxChurnPrediction/",n1,'/',sep = "") )
for (i in 1:102) {
  i=103
  InputFileName <-paste(n1,'-',i,'.csv',sep = '')
  a<-read.table(InputFileName,header=F,sep=',')
  #num <-c((50000*(i-1)+1) : (50000*i))
  num <-c((50000*(i-1)+1) : (50000*(i-1)+16194))  #用作最後一個分割檔案
  b<-mutate(a,NumMsno=num) #%>% select(-1)
  #head(a)
  #head(b)
  OutputFileName = paste('Msno_','Num_',n1,i,'.csv',sep = '')
  write.table(b,OutputFileName, row.names = F, col.names=F, quote = F,sep=',')
  print( paste(i,':successfully processed' ))
}

###










