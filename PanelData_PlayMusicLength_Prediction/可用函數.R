
library(tidyverse)

#read_csv 有很多好用的功能
challenge <- read_csv(readr_example("challenge.csv"))
write_csv(challenge, "challenge.csv")

# write_rds  ：store data in R’s custom binary format called RDS  下次讀取較快
write_rds(challenge, "challenge.rds")
read_rds("challenge.rds")

