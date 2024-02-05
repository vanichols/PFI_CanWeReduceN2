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
  read_excel("code/06_GHG/FTM-FPP-6345-2024-01-30-Comprehensive-Data-Output.xlsx", sheet = "Report Data") %>% 
  janitor::clean_names()


names(ftm)

ftm %>% 
  select(field_name, ghg_per_acre_lbs_co2e_acre) %>% 
  pivot_wider(names_from = field_name,
              values_from = ghg_per_acre_lbs_co2e_acre) %>% 
  janitor::clean_names() %>% 
  mutate(co2e_lbac = corn_150_lbs_n - corn_100_lb_n,
         co2e_lbac2 = corn_100_lb_n - corn_50_lbs_n,
         cf = co2e_lbac/50,
         cf2 = co2e_lbac2/50)
