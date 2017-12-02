#合併其他檔案的變數到train檔案中的使用者
library(dplyr)

df1 <- data.frame(x=c(1,2),y=c(33,44))
df2 <- data.frame(x=c(1,2,2),a=c(66,55,77))
df3 <- data.frame(x=c(1,1,2,2),b=c(888,999,456,444))

full_join(df1,df2,by="x") %>%  full_join(df3,by="x")

setwd("~/Desktop/KKBoxChurnPrediction")

InputFileName <- '2_train_v2/2_train_v2-0.csv'
a<-as.data.frame(read.table(InputFileName,stringsAsFactors = T,header=F,sep=','))
InputFileName <- '4_ulogs/4_ulogs-0.csv'
a1<-as.data.frame(read.table(InputFileName,stringsAsFactors=T,header=F,sep=','))

i=0
while(dim(b)[1] == 0){
  i=125
  InputFileName <- paste('4_ulogs/4_ulogs-',i,'.csv',sep='')
  a1<-as.data.frame(read.table(InputFileName,stringsAsFactors=T,header=F,sep=','))
  b<-filter(a1, a1$V1 == as.character(a[1,1]))
  print(i)
  print(dim(b)[1])
  i<-i+1
}

filenames <- list.files(path='.', pattern="4_ulogs-+.*csv")
## Get names without ".CSV" and store in "names"
names <- substr(filenames,1,  grep('.',filenames) )
for(i in filenames){
  names <- substr(filenames[i],1,  length(filenames) )
  #filepath <- file.path(filenames)
  #assign(i, read.csv(filepath, sep = ",", header=FALSE, skip=1))
}

