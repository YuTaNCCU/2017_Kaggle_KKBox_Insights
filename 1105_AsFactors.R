library(data.table)
PATH <- "user_logs.csv"

cat("Load data with fread")
user_logs <- fread(PATH, 
                   sep = ",", 
                   select = c('msno', 'num_unq', 'total_secs'),
                   colClasses=c(msno="factor",num_unq="numeric",total_secs="numeric"),
                   stringsAsFactors = T)

cat("Calculate means")
user_logs <- user_logs[,lapply(.SD, mean, na.rm=TRUE), by = msno]

cat("Write to csv")
write.table(user_logs, "user_logs_output.csv", sep=",", dec=".", quote=FALSE, row.names=FALSE)