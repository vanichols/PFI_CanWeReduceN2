#--1/25/2024, getting N prices automatically from 'trt_transposed' tab
#--not every data sheet has a trt_transposed tab, so using trt tab

library(tidyverse)
library(janitor)
library(readxl)

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

# functions ---------------------------------------------------------------

CleanData <- function(i = 2){
  
  tmp.key <- coops %>% slice(i)
  
  f.data <- read_excel(paste0("data_stefan/trt_mgmt.", tmp.key$trial_label, ".ReduceN.xlsx"), 
                       sheet = "trt") %>% 
    clean_names()
  
  tmp.data <- 
    f.data %>% 
    filter(grepl("\\$", x1),
           grepl("N", x1)) %>% 
    select(x1, typical, reduced) %>% 
    pivot_longer(typical:reduced) %>% 
    mutate(value = as.numeric(value),
           trial_label = tmp.key$trial_label) %>% 
    select(trial_label, "cost_unit_n" = value) %>% 
    filter(!is.na(cost_unit_n)) %>% 
    distinct() 
  
  return(tmp.data)
  
}

# let's try a loop, -------------------------------------------------------

#--column names seem standardized
dat <- NULL

for (j in 1:nrow(coops)){

  d.tmp <- CleanData(i = j)
  dat <- dat %>% bind_rows(d.tmp)

  }

dat %>% 
  write_csv("data_tidy/td_nprice.csv")

#--note last year range, $0.60 - $1.20
dat %>% 
  arrange(cost_unit_n) %>% 
  mutate(n = 1:n(),
         trial_label = paste(trial_label, n),
         trial_label = fct_inorder(trial_label)) %>% 
  ggplot(aes(trial_label, cost_unit_n)) + 
  geom_point() + 
  geom_hline(yintercept = 0.6, linetype = "dashed") +
  geom_hline(yintercept = 1.2, linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(labels = label_dollar()) + 
  labs(title = "2022 prices ranged from $0.60-$1.20")

ggsave("figs_scratch/scratch_nprices.png")
