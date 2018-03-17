#合併其他檔案的變數到train檔案中的使用者
library(dplyr)

setwd("~/Desktop/KKBoxChurnPrediction")


###讀取多個檔案
### https://stackoverflow.com/questions/28652163/how-to-read-many-files-using-a-loop-in-r

##列下4_ulogs所有檔案的名稱
setwd("~/Desktop/KKBoxChurnPrediction/4_ulogs")
filenames <- list.files(path='.', pattern="4_ulogs-+.*csv")
names <- substr(filenames, 1,  nchar(filenames )-4 )
#install.packages('data.table')
library(data.table)

##讀取4_ulogs所有檔案
for(i in names){
  print(i)
  filepath <- file.path(paste(i,".csv",sep=""))
  assign(i, fread(filepath, sep = ",", header=FALSE, stringsAsFactors = T))
}


InputFileName <- '2_train_v2/2_train_v2-0.csv'
`2_train_1-0` <-fread(InputFileName,stringsAsFactors = T,header=F,sep=',')

###filter （未完成）
for(i in 1:50000){
  i=1
  b <-filter(`4_ulogs-125`, `4_ulogs-125`$V1 == as.character(as.data.frame(`2_train_1-0`) [1,1]) )
  print(b)
}


aa<-as.data.frame(`2_train_1-0`)
aa[1,1]
`2_train_1-0`[1,1]
as.character(aa[1,1])
as.character(`2_train_1-0`[1,1])
