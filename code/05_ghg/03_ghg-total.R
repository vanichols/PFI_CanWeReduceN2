#--figure out ghg emissions avoided
#--started 1/26/2024

library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

rm(list = ls())

source("code/05_ghg/fxn_conversions.R")

# data --------------------------------------------------------------------

n <- read_csv("data_tidy/td_ghg-n2o.csv")

e <- read_csv("data_tidy/td_ghg-energy.csv")

y <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  select(trial_key, dif_nrate_lbac) %>% 
  distinct()

d <- 
  crossing(e, fert_type = n %>% pull(fert_type)) %>%  
  bind_rows(n) 

d %>% 
  group_by(trial_key, fert_type) %>% 
  summarise(co2e_kgha = sum(co2e_kgha)) %>%
  left_join(y, relationship = "many-to-many") %>% 
  ggplot(aes(dif_nrate_lbac, co2e_kgha)) + 
  geom_point(aes(color = fert_type)) + 
  geom_line(aes(color = fert_type))

d %>% 
  group_by(trial_key, fert_type) %>% 
  summarise(co2e_kgha = sum(co2e_kgha)) %>%
  left_join(y, relationship = "many-to-many") 
  
  3/300

#--the type of fertilizer assumed isn't that big of a deal. just take an average

res <- 
  d  %>% 
  group_by(trial_key, desc) %>% 
  summarise(co2e_kgha = mean(co2e_kgha, na.rm = T)) %>% 
  mutate(co2e_lbac = co2e_kgha * ha_per_ac * lb_per_kg) 

res %>% 
  select(-co2e_kgha) %>% 
  write_csv("data_tidy/td_co2e.csv")

res
#--multiply the lbac by 7.82 to get lb co2e/ac. 
#--FTM does 11.4 near Ames IA, 8.06 near Eau Claire WI, N2Os are different dep on soils

res %>% 
  group_by(trial_key) %>% 
  summarise(co2e_lbac = sum(co2e_lbac)) %>% 
  left_join(y) %>% 
  mutate(mf = co2e_lbac/dif_nrate_lbac)

