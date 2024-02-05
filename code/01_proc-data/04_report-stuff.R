
library(tidyverse)
library(janitor)

rm(list = ls())


d <- read_csv("data_tidy/td_trialkey.csv")

#--19 farmers
d %>% 
  select(first_name, last_name) %>% 
  distinct()

#--22 trials
d %>% 
  select(trial_key)
  
d2 <- read_csv("data_tidy/td_cornyields.csv")

d2 %>% 
  select(trial_key, trt, nrate_lbac) %>% 
  distinct() %>% 
  pivot_wider(names_from = trt, values_from = nrate_lbac) %>% 
  mutate(dif = typ - red,
         dif_pct = dif/typ) %>% 
  pull(dif_pct) %>% 
  summary()
