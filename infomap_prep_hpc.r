#####################################
# infomap prep for predicted draws
#####################################

suppressPackageStartupMessages(library(infomapecology))
suppressPackageStartupMessages(library(tidyverse))

# get the input passed from the shell script
args <- commandArgs(trailingOnly = TRUE)
str(args)
cat(args, sep = "\n")

# test if there is at least one argument: if not, return an error
if (length(args) == 0) {
  stop("At least one argument must be supplied (input file).\n", call. = FALSE)
} else {
  print(paste0("Arg input:  ", args[1]))
}



i = args[1]
print("TASK ID:")
print(i)

dir = args[2]
input.file = args[3]
output.file = args [4]

print(paste0("READING:", input.file))
d <- read.csv(input.file)
# d <- d[1:2000,] # FOR TESTING

print("Read file successfully")
nodes <- data.frame(node_name=c(unique(d$plant_name_id), unique(d$area_code_l3)),
                    node_id_original =1:length(c(unique(d$plant_name_id),
                                                 unique(d$area_code_l3))))
print("nodes ok")
d <- d %>%
  select(to=plant_name_id, from=area_code_l3) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(weight=1) %>%
  left_join(nodes, by=c("from" = "node_name")) %>%
  rename(nodeIDarea = node_id_original) %>%
  left_join(nodes, by=c("to" = "node_name")) %>%
  rename(nodeIDsp = node_id_original)

network_object <- create_monolayer_object(x=d, directed = F, bipartite = T, node_metadata = nodes)
 "saving"
saveRDS(network_object, file=paste0(output.file))






