# load packages and data ----
suppressPackageStartupMessages(library(tidyverse)) 

args <- commandArgs(trailingOnly = TRUE) 
i = as.numeric(args[1])
test=args[2] 

print(i)
print(paste("Test:",test)) 

setwd("infomaps/edgesMinor") 

scenpattern=c("scenario1_N", 
             "scenario2_I", 
             "scenario3_E-RL-VU", 
             "scenario3_E-RL-EN",
             "scenario3_E-RL-CR", 
             "scenario4_Epred-VU-draw", 
             "scenario4_Epred-EN-draw", 
             "scenario4_Epred-CR-draw", 
             "scenario5_IE-RL-VU",
             "scenario5_IE-RL-EN", 
             "scenario5_IE-RL-CR", 
             "scenario6_IEpred-VU-draw", 
             "scenario6_IEpred-EN-draw", 
             "scenario6_IEpred-CR-draw")
             
print(scenpattern[i]) 

files<- list.files(pattern=scenpattern[i]) 

if(test) files <- files[1:5]

filesbound <- files %>% 
               lapply(read.csv, stringsAsFactors=F) %>%
               purrr::reduce(dplyr::left_join, by = c("n1","n2")) 

print("purr bit ok")
               
 filesbound <- filesbound %>% 
               rowwise() %>% 
               mutate(freq = mean(c_across(where(is.numeric)), na.rm=TRUE)) %>% 
               select(n1, n2, freq) 
               
write.csv(filesbound,paste0("../edges_minor_summary_", scenpattern[i],".csv"))

