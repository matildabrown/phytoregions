########################################################### #
#    WCVP annotation - HPC edition
#    adapted from local version 06/06/2023
########################################################### #


# load packages and data ----
library(tidyverse)

#a couple rogue codes in the data
code_corrections = c("SAO"="SOA",
                     "SRI"="SRL",
                     "QLS"="QLD")

#special issue versions
wcvp_names <- read.table("wcvp_names.txt",
                         sep="|", header=TRUE, quote = "",
                         fill=TRUE, encoding = "UTF-8")
wcvp_distributions <- read.table("wcvp_distribution.txt",
                                 sep="|", header=TRUE, quote = "",
                                 fill=TRUE, encoding = "UTF-8")

wcvp <- right_join(wcvp_names %>%
                     select(plant_name_id, taxon_name, taxon_status, taxon_rank, species_hybrid),
                   wcvp_distributions, multiple="all") %>%
  mutate(area_code_l3=str_to_upper(area_code_l3),
         area_code_l3=recode(area_code_l3, !!! code_corrections)) %>%
  filter(area_code_l3 != "",
         taxon_rank == "Species",
         taxon_status=="Accepted",
         species_hybrid == "")

rm(wcvp_names)
rm(wcvp_distributions)


# I: native + introduced occurrences ----
# because we're supplementing native and WCVP occurrences, we can just
# add in the GloNAF occurrences without worrying about which status rules

glonaf <- read.csv("glonaf-wcvp_full-dataset.csv") %>%
  left_join(wcvp %>% # add taxon rank,
              # taxon status and hybrid
              # status
              select(plant_name_id,
                     taxon_rank,
                     taxon_status,
                     species_hybrid) %>%
              unique(),
            by=c("accepted_plant_name_id"="plant_name_id")
  ) %>%
  filter(taxon_status == "Accepted",
         species_hybrid == "")


int <- wcvp %>% filter(location_doubtful==0,
                     taxon_rank == "Species",
                     taxon_status=="Accepted") %>%
  full_join(glonaf ,
            by=c("plant_name_id"="accepted_plant_name_id",
                 "taxon_name"="accepted_taxon_name",
                 "area_code_l3" = "tdwg3",
                 "taxon_status",
                 "taxon_rank",
                 "species_hybrid"
            ), multiple="all") %>%
  select(plant_name_id,
         taxon_name,
         area_code_l3) %>%
  unique() %>%
  na.omit()
  
  


# E_RL-VU: native occurrences - extinctions (conservative) ----
redlist <- read.csv("redlist-matches_v2022-1.csv")


## E2: native occurrences - extinctions (predicted) ----

# Note: this is just an example, using one of the draws (there are 1000
# in total, these are iterated through in the infomap step)
for (i in 1:3){

  threatened_cats <- c("VU","EN","CR","EW", "EX")[i:5]
  cutoff_cat <- c("VU","EN","CR","EW", "EX")[i]
    cli::cli_alert_info(paste(Sys.time(),"commencing ", cutoff_cat))
    
    e1 <- wcvp %>%
  filter(introduced==0,
         location_doubtful==0,
         extinct==0,                       # removing extinct occurrences
         taxon_rank == "Species",
         taxon_status=="Accepted",
         species_hybrid == "") %>%
  left_join(redlist, by=c("plant_name_id"="accepted_plant_name_id")) %>%
  filter(!category %in% threatened_cats ) %>%
  select(plant_name_id,
         taxon_name,
         area_code_l3) %>%
  unique() %>%
  na.omit()

  for (j in 1:1000){

  draw <- read.csv(paste0("draws/model_",cutoff_cat,"_draw",j,".csv"), colClasses=c("NULL",NA,NA,NA)) %>%
  left_join(redlist %>% select(accepted_plant_name_id, category),
            by=c("plant_name_id"="accepted_plant_name_id")) %>%
  mutate(category=na_if(category, "Data Deficient"),
         threatened=case_when(is.na(category) ~ NA_real_,
                              category %in% threatened_cats ~ 1,
                              TRUE ~ 0),
         extinct =case_when (is.na(threatened) ~ as.numeric(extinct),
                             TRUE ~ threatened)

         )

extinct <- draw %>% filter(extinct==1)

e2 <- e1 %>% filter(!plant_name_id %in% extinct$plant_name_id)

#write_csv(e2, paste0("scenario_data/scenario4_Epred-",cutoff_cat,"-draw",j,".csv"))

ie2 <- anti_join(int, extinct)
write_csv(ie2, paste0("scenario_data/scenario6_IEpred-",cutoff_cat,"-draw",j,".csv"))

  }
}



