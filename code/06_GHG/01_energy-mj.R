#--figure out ghg emissions avoided
#--started 1/26/2024

library(tidyverse)
library(lubridate)

rm(list = ls())

source("code/06_GHG/fxn_conversions.R")

# avoided fertilizer ------------------------------------------------------------------

d_raw <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  select(trial_key, dif_nrate_lbac) %>% 
  distinct()

d_raw


# references --------------------------------------------------------------

#--fertilizer manufacturing energy, from greet tables
r_ferte <- 
  read_csv("code/06_GHG/ref_fert-energy.csv") 



# avoided energy use ------------------------------------------------------

#--get assumed amoutn of n application avoided
e1 <- 
  d_raw %>% 
  mutate(
    #--units are currently lb n/ac
    kgn_ha_avoided = dif_nrate_lbac * kg_per_lb * ac_per_ha) %>% 
  mutate(desc = "nitrogen")


# energy to manu the fertilizer we avoided
# this has one value for manufacturing 'N'. The different fertiilizer types are just scaled by how much N they contain

e2 <-
  e1 %>% 
  left_join(r_ferte,
            by = c("desc")) |> 
  mutate(
    mj_avoided_ha = kgn_ha_avoided * value)  %>% 
  select(trial_key, mj_avoided_ha)


e2 %>% 
  write_csv("data_tidy/td_fert-energy-avoided.csv")


