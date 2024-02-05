#--make tidy management data
#--write field sizes separately, mgmt is literally just a row bind of stefan's files
#--there are lots of issues here

library(tidyverse)
library(janitor)

rm(list = ls())

# who should we have data for? --------------------------------------------

d <- read_csv("data_tidy/td_trialkey.csv")

#--Stefan's file names use the 'trial label' capitalization/notation
coops <- 
  d |> 
  arrange(trial_label) %>% 
  select(trial_key, trial_label) %>% 
  mutate(trial_label = str_remove_all(trial_label, " "))

GetStripSize <- function(i = 1){
  
  tmp.key <- coops %>% slice(i)
  
  f.data <- read_excel(paste0("data_stefan/trt_mgmt.", tmp.key$trial_label, ".ReduceN.xlsx"), 
                       sheet = "fieldactivities") 
  tmp.data <- 
    f.data %>% 
    clean_names() %>% 
    filter(grepl("Strip", x1)) %>% 
    mutate(trial_key = tmp.key$trial_key)
    
  return(tmp.data)
  
}

# let's try a loop, -------------------------------------------------------

#--column names seem standardized
dat <- NULL

for (j in 1:nrow(coops)){
  d.tmp <- GetStripSize(i = j)
  dat <- dat %>% bind_rows(d.tmp)
}


ss <-
  dat %>% 
  select(-details) %>% 
  pivot_wider(names_from = x1, values_from = x2) %>% 
  clean_names()

ss2 <- 
  ss %>% 
  pivot_longer(strip_length_ft:strip_width_ft) %>%
  mutate(value = as.numeric(value)) %>% 
  group_by(name) %>% 
  summarise(value = mean(value, na.rm = T))

# 1 ft2 = 2.29568e-5 ac  
ss2 %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  mutate(ft2 = strip_length_ft * strip_width_ft,
         ac = ft2 * 2.29568e-5)
