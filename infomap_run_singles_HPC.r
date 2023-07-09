########################################################### #
#    Infomap running and summarising 13/6/23
########################################################### #

# load packages and data ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(infomapecology))


# get the input passed from the shell script
args <- commandArgs(trailingOnly = TRUE)
str(args)
cat(args, sep = "\n")

wd <- getwd()
setwd(args[5])
install_infomap()

# test if there is at least one argument: if not, return an error
if (length(args) == 0) {
  stop("At least one argument must be supplied (input file).\n", call. = FALSE)
} else {
  print(paste0("Arg input:  ", args[1]))
}

i = args[1]
print("TASK ID:")
print(i)

input.dir = args[3]
fname = args[2]
output.dir = args[4]

input.file = paste0(fname,".rds")


print(paste0("READING: ", input.file))
network_object <- readRDS(paste0(input.dir,"/",input.file))

infomap_object <- run_infomap_monolayer(network_object, infomap_executable='Infomap',
                                        flow_model = 'undirected',seed=(i),
                                        silent=T, trials=100, two_level = FALSE,
                                        verbose=FALSE)
                                        
   print("Infomap done!")
   saveRDS(infomap_object, file = paste0(output.dir,"/infomap_objects/", fname,"_run",i,".rds"))

   print("Calculating modules...")
                                        
resmod <- infomap_object$modules %>% 
  filter(node_group=="set_rows") 

nm1 <-resmod %>%  
  select(module_level1) %>%  
  n_distinct()
nm2 <- resmod %>% 
  select(module_level1,
         module_level2) %>%  
  n_distinct()
 nm3 <- resmod %>%
   select(module_level1,
          module_level2,
          module_level3) %>%  
   n_distinct()
 
 
 if(nm1<=10) resmod$major <- resmod$module_level1
 if(nm1>10 & nm2 <=10) resmod$major <- paste0(resmod$module_level1,"-",resmod$module_level2)
 if(nm2<=100) resmod$minor <- paste0(resmod$module_level1,"-",resmod$module_level2)
 if(nm2>100 & nm3 <=100) resmod$minor <- paste0(resmod$module_level1,"-",resmod$module_level2,"-",resmod$module_level3)
 
 resmod <- resmod %>% 
   add_count(major) %>% 
   rename(nMajor=n) %>%
   add_count(minor) %>% 
     rename(nMinor=n)
     
 write.csv(resmod,file = paste0(output.dir,"/modules/", fname,"run",i,".csv"), row.names=FALSE)
 print("Module results saved! Everything from here on out is time-saving but not essential")
 
scenario_data <- read.csv(paste0("/mnt/shared/scratch/mbrown/infomaps/revisions/scenario_data/", fname, ".csv"))
 
 if(fname %in% c("scenario1_N", "scenario2_I")){
  summary_infomaps <- data.frame(scenario=fname, run=i)  %>%
   `rownames<-`(NULL)
 }
 
  if(grepl("scenario3|scenario5", fname)){
  summary_infomaps <- data.frame(metadata=str_split(fname,"-")[[1]]) %>%
   `rownames<-`(c("scenario","threshold"))  %>%
   t() %>% 
   as.data.frame() %>%
   `rownames<-`(NULL) %>%
   mutate(run=i)
 }
 
  if(grepl("scenario4|scenario6", fname)){
  summary_infomaps <- data.frame(metadata=str_split(fname,"-")[[1]]) %>%
   `rownames<-`(c("scenario","threshold"))  %>%
   t() %>% 
   as.data.frame() %>%
   `rownames<-`(NULL) %>%
   mutate(run=i, draw="median")
 }
 
 summary_infomaps$speciesnumber <- length(unique(scenario_data$taxon_name))
 summary_infomaps$occurrencenumber <- nrow(unique(scenario_data))
 
 summary_infomaps$n_m1 <- nm1
 summary_infomaps$n_m2 <- nm2
 summary_infomaps$n_m3 <- nm3
 
 summary_infomaps$majorlevel <- paste0("module_level", which(c(nm1, nm2, nm3)<=10), collapse=", ")
 summary_infomaps$minorlevel <- paste0("module_level", which(c(nm1, nm2, nm3)<=100), collapse=", ")
 

edgesMajor <- data.frame(n1=rep(resmod$node_name, each=368), n2=rep(resmod$node_name, times=368), edge=0)
mods <- unique(resmod$major)

for(i in 1:length(mods)){
  mi <- resmod %>% filter(major==mods[i])
  edgesMajor$edge[which(
    edgesMajor$n1 %in% mi$node_name & edgesMajor$n2 %in% mi$node_name
  )] <- 1
}

edgesMinor <- data.frame(n1=rep(resmod$node_name, each=368), n2=rep(resmod$node_name, times=368), edge=0)
mods <- unique(resmod$minor)

for(i in 1:length(mods)){
  mi <- resmod %>% filter(minor==mods[i])
  edgesMinor$edge[which(
    edgesMinor$n1 %in% mi$node_name & edgesMinor$n2 %in% mi$node_name
  )] <- 1
}

summary_infomaps$majoredgenumber <- length(which(edgesMajor$edge==1))
summary_infomaps$minoredgenumber <- length(which(edgesMinor$edge==1))

degrees <- resmod %>% select(node_name, nMajor, nMinor) %>%
  pivot_longer(cols=c(nMajor, nMinor), 
               names_to = "type", values_to = "ninmod") %>%
  mutate(columnname = paste(node_name, type, sep=".")) %>%
  select(-node_name, -type) %>%
  pivot_wider(names_from=columnname, values_from = ninmod)

summary_infomaps <- cbind(summary_infomaps, degrees)

#summary
 write.csv(summary_infomaps,file = paste0(output.dir,"/summaries/", fname,"_run",i,".csv"), row.names=FALSE)
#edgesMajor
colnames(edgesMajor)[3] <- colnames(edgesMinor)[3] <- fname
 write.csv(edgesMajor,file = paste0(output.dir,"/edgesMajor/", fname,"_run",i,".csv"), row.names=FALSE)
#edgesMinor
 write.csv(edgesMinor,file = paste0(output.dir,"/edgesMinor/", fname,"_run",i,".csv"), row.names=FALSE)

print("All done!")



