#--figure out ghg emissions avoided
#--started 1/26/2024

library(tidyverse)
library(lubridate)
library(readxl)

rm(list = ls())



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
  write_csv("data_tidy/td_ghg.csv")


d %>% 
  group_by(trial_key, fert_type) %>% 
  summarise(co2e_kgha = sum(co2e_kgha)) %>%
  left_join(y, relationship = "many-to-many") %>% 
  ggplot(aes(dif_nrate_lbac, co2e_kgha)) + 
  geom_point(aes(color = fert_type))

#--the type of fertilizer assumed isn't that big of a deal. just take an average

d_avg <- 
  d %>% 
  group_by(trial_key, fert_type) %>% 
  summarise(co2e_kgha = sum(co2e_kgha)) %>%
  group_by(trial_key) %>% 
  summarise(co2e_kgha = mean(co2e_kgha)) 


#--multiply the lbac by 8.7 to get kg co2e/ha
#--stefan probably wants lb/ac
d_avg %>% 
  left_join(y) %>% 
  mutate(mf = co2e_kgha/dif_nrate_lbac)

d_avg %>% 
  mutate(year = 2023,
         cumco2e_kgha = cumsum(co2e_kgha),
         trial_key = fct_inorder(trial_key)) %>% 
  ggplot(aes(trial_key, cumco2e_kgha)) + 
  geom_point()
