########################################################### #
#    Infomap running
########################################################### #
#

# load packages and data ----
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(infomapecology))


# get the input passed from the shell script
args <- commandArgs(trailingOnly = TRUE)
str(args)
cat(args, sep = "\n")

wd <- getwd()
setwd(args[6])
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

input.dir = args[2]
input.file = args[3]
output.file = args [4]
output.dir = args[5]


print(paste0("READING: ",input.file))
network_object <- readRDS(paste0( input.file))

infomap_object <- run_infomap_monolayer(network_object, infomap_executable='Infomap',
                                        flow_model = 'undirected',seed=(i),
                                        silent=T, trials=100, two_level = FALSE,
                                        verbose=FALSE)
                                        
   print("Infomap done!")
   saveRDS(infomap_object, file = paste0(output.dir,"/", output.file,".rds"))

   print("Calculating modules...")
                                        
resmod <- infomap_object$modules %>% 
                  filter(node_group=="set_rows") 
                  
resmod$m2 <- paste(resmod$module_level1, resmod$module_level2, sep="-")
resmod$m2 <- factor(gsub("-NA","",resmod$m2))     

print("Modules done!")
write_csv(resmod, file = paste0(output.dir,"/", output.file, "_modules.csv"))

           
    print("Edges done!")                                     





