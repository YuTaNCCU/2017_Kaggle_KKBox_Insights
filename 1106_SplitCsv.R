setwd("~/Desktop/KKBoxChurnPrediction")
system('split -l 500000 ulogs.csv') #split member.csv into files containing 50000 rows seperately
setwd("~/Desktop/KKBoxChurnPrediction/ulogs")
system('for i in *; do mv "$i" "$i.csv"; done') #注意：整個資料夾都會加上csv
