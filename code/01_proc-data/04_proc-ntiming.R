#--1/25/2024, getting N prices automatically from 'trt_transposed' tab
#--not every data sheet has a trt_transposed tab, so using trt tab
#--2/8/2024, changing such that only the N that was adjusted has prices extracted

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

#-Peterson didn't put a value for one of the N applications
#-NOTE: Abel doesn't have y in the sidedress, he has numbers 

GetAdjNTiming <- function(i = 5){
  
  tmp.key <- coops %>% slice(i)
  
  f.data <- read_excel(paste0("data_stefan/trt_mgmt.", tmp.key$trial_label, ".ReduceN.xlsx"), 
                       sheet = "trt") %>% 
    clean_names()
  
  # 
  # tmp.data <- 
  #   f.data %>% 
  #   mutate(n_type = gsub(" .*$", "", x1)) %>% 
  #   filter(grepl("amt", x1)) %>%
  #   mutate(typical = as.numeric(typical),
  #          reduced = as.numeric(reduced)) %>% 
  #   filter(typical != reduced) #---WHY ISN"T THIS WORKING?!?!
  
   
  tmp.filt <- 
    f.data %>% 
    mutate(n_type = gsub(" .*$", "", x1)) %>%
    filter(typical != reduced)
  
  if (nrow(tmp.filt >= 1)) {
    tmp.data <- 
      tmp.filt %>% 
      mutate(adjusted_n_type = n_type) %>% 
      select(adjusted_n_type) %>% 
      mutate(trial_key = tmp.key$trial_key)
  } else {
    tmp.data <- tibble(trial_key = tmp.key$trial_key, ajusted_n_typed = NA)  
    }

  
  return(tmp.data)
  
}


#--column names seem standardized
dat.adjn <- NULL

for (j in 1:nrow(coops)){
  
  d.tmp <- GetAdjNTiming(i = j)
  dat.adjn <- dat.adjn %>% bind_rows(d.tmp)
  
}

#--we have 22!
dat.adjn2 <- 
  dat.adjn %>% 
  distinct() %>% 
  group_by(adjusted_n_type) %>% 
  summarise(adj_nu = n()) %>% 
  rename("n_type" = adjusted_n_type)


# N timing for all N applications ----------------------------------------------------------------

#-NOTE: Abel doesn't have y in the sidedress, he has numbers, so he isn't getting counted
GetNTypes <- function(i = 1){
  
  tmp.key <- coops %>% slice(i)
  
  f.data <- read_excel(paste0("data_stefan/trt_mgmt.", tmp.key$trial_label, ".ReduceN.xlsx"), 
                       sheet = "trt") %>% 
    clean_names()
  
  #--filter to where typical is not equal to reduced, figure out type
  
  tmp.data <- 
    f.data %>% 
    mutate(n_type = gsub(" .*$", "", x1)) %>% 
    filter(grepl("applied", x1)) %>% 
    mutate(keepme = case_when(
      typical == "y" ~ "y",
      reduced == "y" ~ "y", 
      TRUE ~ "n")) %>% 
    filter(keepme == "y") %>% 
    mutate(trial_key = tmp.key$trial_key) %>% 
    select(trial_key, n_type) %>% 
    distinct() 
  
  return(tmp.data)
  
}


# looped N ----------------------------------------------------------------

#--column names seem standardized
dat.n <- NULL

for (j in 1:nrow(coops)){
  
  d.tmp <- GetNTypes(i = j)
  dat.n <- dat.n %>% bind_rows(d.tmp)
  
}



# combine data ------------------------------------------------------------

dat <- 
  dat.n %>% 
  group_by(n_type) %>% 
  summarise(app_nu = n()) %>% 
  left_join(dat.adjn2) %>% 
  mutate(desc = case_when(
    n_type == "atplant" ~ "Two days before planting - one week after corn planting",
    n_type == "fall" ~ "After crop harvest - Dec 14",
    n_type == "preplant" ~ "March 15 - three days before planting",
    n_type == "sidedress" ~ "Eight days after corn planting - corn canopy closure",
    n_type == "topdress" ~ "After corn canopy closure",
    n_type == "winter" ~ "Dec 15 - March 14",
    TRUE ~ NA
  ))

dat %>%
  select(n_type, desc, app_nu, adj_nu) %>% 
  mutate(n_type = factor(n_type, levels = c("fall", 
                                            "winter", 
                                            "pre-plant",
                                            "at plant", 
                                            "side-dress", 
                                            "top-dress"))) %>%
  arrange(n_type) %>% 
  write_csv("data_tidy/td_napplications-summarized.csv")



# write data for alluvial -------------------------------------------------

#--add Abels' side-dress manually

d_alluv <- 
  d %>% 
  select(trial_key) %>% 
  left_join(dat.n) %>% 
  full_join(dat.adjn, relationship = "many-to-many") %>% 
  distinct() %>% 
  add_row(trial_key = "abel_23",
          n_type = "sidedress",
          adjusted_n_type = "sidedress") %>% 
  arrange(trial_key) %>% 
  mutate(n_type = case_when(
    n_type == "atplant" ~ "At plant",
    n_type == "fall" ~ "Fall",
    n_type == "preplant" ~ "Pre-plant",
    n_type == "sidedress" ~ "Side-dress",
    n_type == "topdress" ~ "Top-dress",
    n_type == "winter" ~ "Winter",
    TRUE ~ NA
  )) %>% 
  mutate(adjusted_n_type = case_when(
    adjusted_n_type == "atplant" ~ "At plant",
    adjusted_n_type == "fall" ~ "Fall",
    adjusted_n_type == "preplant" ~ "Pre-plant",
    adjusted_n_type == "sidedress" ~ "Side-dress",
    adjusted_n_type == "topdress" ~ "Top-dress",
    adjusted_n_type == "winter" ~ "Winter",
    TRUE ~ NA
  ))



d_alluv %>% 
  write_csv("data_tidy/td_napplications.csv")
