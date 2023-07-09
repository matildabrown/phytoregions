library(dplyr)
library(readr)
library(data.table)

setwd("infomaps/summaries/")

list_file <- list.files() 

list_file <- rbindlist(lapply(list_file,fread),use.names=T,fill=T)

write.csv(list_file, "../all_results_summaries.csv", row.names=F)
