#--3/3/2024 - did anyone adjust their manure applicaion? GHG manu would be diff

library(tidyverse)
library(janitor)
library(readxl)
library(scales)

rm(list = ls())

# who should we have data for? --------------------------------------------

d <- 
  read_csv("data_tidy/td_trialkey.csv") 

#--Stefan's file names use the 'trial label' capitalization/notation
coops <- 
  d %>% 
  mutate(trial_label = str_remove_all(trial_label, " ")) %>% #--remove space from Van Horn
  arrange(trial_label) %>% 
  select(trial_key, trial_label) 


# adjusted n timings ------------------------------------------------------

GetAdjNType <- function(i = 5){
  
  tmp.key <- coops %>% slice(i)
  
  f.data <- read_excel(paste0("data_stefan/trt_mgmt.", tmp.key$trial_label, ".ReduceN.xlsx"), 
                       sheet = "trt") %>% 
    clean_names() %>% 
    select(-details)
  
  tmp.amt <- 
    f.data %>% 
    filter(grepl("amt", x1)) %>% 
    pivot_longer(typical:reduced) %>% 
    filter(!is.na(value)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    pivot_longer(typical:reduced) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>% 
    mutate(timing = gsub(" .*$", "", x1)) %>%
    select(timing, trt = name, napp_lbac = value)
    
  tmp.type <- 
    f.data %>% 
    filter(grepl("type", x1)) %>%
    pivot_longer(typical:reduced) %>% 
    filter(!is.na(value)) %>% 
    pivot_wider(names_from = name, values_from = value) %>% 
    pivot_longer(typical:reduced) %>% 
    mutate(timing = gsub(" .*$", "", x1)) %>%
    select(timing, trt = name, n_type = value)
  
  tmp.data <- 
    tmp.amt %>% 
    left_join(tmp.type) %>%  
    mutate(trial_key = tmp.key$trial_key)
   
  return(tmp.data)
  
}


#--column names seem standardized
dat.adjn <- NULL

for (j in 1:nrow(coops)){
  
  d.tmp <- GetAdjNType(i = j)
  dat.adjn <- dat.adjn %>% bind_rows(d.tmp)
  
}


dat.adjn %>% 
  select(trial_key, everything()) %>% 
  write_csv("data_tidy/td_napplications.csv")


# were any organic sources adjusted? --------------------------------------

#--no!
dat.adjn %>% 
  filter(n_type != "chemical") %>% 
  pivot_wider(names_from = trt, values_from = napp_lbac)
