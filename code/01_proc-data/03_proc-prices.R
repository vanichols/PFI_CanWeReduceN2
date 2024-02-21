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


# Corn prices ----------------------------------------------------------------

GetCornPrices <- function(i = 2){
  
  tmp.key <- coops %>% slice(i)
  
  f.data <- read_excel(paste0("data_stefan/trt_mgmt.", tmp.key$trial_label, ".ReduceN.xlsx"), 
                       sheet = "fieldactivities") %>% 
    clean_names()
  
  #--get corn price
  
  tmp.corn <- 
    f.data %>% 
    filter(grepl("\\$", x1)) %>%
    mutate(corn_price_bu = as.numeric(x2),
           trial_key = tmp.key$trial_key) %>% 
    select(trial_key, corn_price_bu) 

  return(tmp.corn)
  
}


# looped corn ----------------------------------------------------------------

#--column names seem standardized
dat.c <- NULL

for (j in 1:nrow(coops)){

  d.tmp <- GetCornPrices(i = j)
  dat.c <- dat.c %>% bind_rows(d.tmp)

  }


# N prices ----------------------------------------------------------------

#--note: Peterson did not report a price for his adjusted side-dressing

GetNPrices <- function(i = 1){
  
  tmp.key <- coops %>% slice(i)
  
  f.data <- read_excel(paste0("data_stefan/trt_mgmt.", tmp.key$trial_label, ".ReduceN.xlsx"), 
                       sheet = "trt") %>% 
    clean_names()
  
  #--filter to where typical is not equal to reduced, figure out type
  
  tmp.adjn <- 
    f.data %>% 
    mutate(n_type = gsub(" .*$", "", x1)) %>% 
    filter(typical != reduced) %>%
    mutate(adjusted_n_type = n_type) %>% 
    select(adjusted_n_type) %>% 
    pull()
  
  
  #--only look at those n prices  
  tmp.data <- 
    f.data %>% 
    mutate(n_type = gsub(" .*$", "", x1)) %>%
    filter(n_type %in% tmp.adjn,
           grepl("\\$", x1),
           grepl("N", x1)) %>% 
    select(n_type, typical, reduced) %>% 
    pivot_longer(typical:reduced) %>% 
    mutate(value = as.numeric(value),
           trial_key = tmp.key$trial_key) %>% 
    select(trial_key, "adj_n_type" = n_type, "cost_unit_n" = value) %>% 
    filter(!is.na(cost_unit_n)) %>% 
    distinct() 
  
  return(tmp.data)
  
}


# looped N ----------------------------------------------------------------

#--column names seem standardized
dat.n <- NULL

for (j in 1:nrow(coops)){
  
  d.tmp <- GetNPrices(i = j)
  dat.n <- dat.n %>% bind_rows(d.tmp)
  
}


coops %>% left_join(dat.n) ->a

# combine data ------------------------------------------------------------

dat <- 
  dat.n %>% 
  left_join(dat.c) 

dat %>% 
  write_csv("data_tidy/td_prices.csv")

dat %>% 
  ggplot(aes(adj_n_type)) + 
  geom_histogram(stat = "count")

dat %>% 
  group_by(adj_n_type) %>% 
  summarise(n = n())

#--note last year range, $0.60 - $1.20
dat %>% 
  arrange(cost_unit_n) %>% 
  mutate(n = 1:n(),
         trial_label = paste(trial_label, n),
         trial_label = fct_inorder(trial_label)) %>% 
  ggplot(aes(trial_label, cost_unit_n)) + 
  geom_point(aes(color = adj_n_type), size= 3) + 
  geom_hline(yintercept = 0.6, linetype = "dashed") +
  geom_hline(yintercept = 1.2, linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(labels = label_dollar()) + 
  labs(title = "2022 cooperator prices ranged from $0.60-$1.20") + 
  scale_color_viridis_d()

ggsave("figs_scratch/scratch_nprices.png")
