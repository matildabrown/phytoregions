library(tidyverse)
library(brms)

df <- read.csv("wcvp_annotated.csv") %>%
  mutate_at(c("continent", "lifeform","area_code_l3"), factor) %>%
  rename(endemic_to = area_code_l3)


cats <- c("VU","EN","CR")

#VU and EN already run,  CR only
for (i in 3){
  model <- readRDS(paste0("extinction-prediction_", cats[i], "_model.rds"))

  pred <- df %>%
  tidybayes::add_predicted_draws(model, ndraws=1000, allow_new_levels=TRUE)

  for(j in 1:1000){
    predj <- pred %>% filter(.draw==j) %>%
    ungroup() %>%
      select(plant_name_id, taxon_name, extinct=.prediction)
    write.csv(predj, paste0("draws/model_",cats[i],"_draw",j,".csv"))
  }

}
