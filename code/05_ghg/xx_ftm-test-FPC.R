#--figure out ghg emissions avoided according to ftm
#--started 1/30/2024

library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

rm(list = ls())

source("code/06_GHG/fxn_conversions.R")

# data --------------------------------------------------------------------

ftm <- 
  read_excel("code/06_GHG/FTM-FPP-6345-2024-02-07-Comprehensive-Data-Output.xlsx", sheet = "Report Data") %>% 
  janitor::clean_names()


names(ftm)

ftm %>% 
  mutate(n_amt = parse_number(field_name)) %>% 
  select(state, n_amt, ghg_per_acre_lbs_co2e_acre) %>%
  pivot_wider(names_from = n_amt,
              values_from = ghg_per_acre_lbs_co2e_acre) %>% 
  janitor::clean_names() %>% 
  mutate(co2e_lbac = x150 - x100,
         co2e_lbac2 = x100 - x50,
         cf = co2e_lbac/50,
         cf2 = co2e_lbac2/50)
