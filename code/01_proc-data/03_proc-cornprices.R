#--1/25/2024, getting corn prices? From field activities?

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
                       sheet = "fieldactivities") %>% 
    clean_names()
  
  tmp.data <- 
    f.data %>% 
    filter(grepl("\\$", x1),
           grepl("Corn", x1)) %>% 
    select(x2) %>%
    mutate(x2 = as.numeric(x2),
           trial_label = tmp.key$trial_label) %>% 
    select(trial_label, "price_recieved_corn" = x2)
  
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
  write_csv("data_tidy/td_cornprice.csv")

#--corn price in 2023 https://www.extension.iastate.edu/agdm/crops/pdf/a2-11.pdf (December not updated?)
crn_hi <- 6.83 # last year it was $7.48
crn_lo <- 4.74 # last year it was $5.70

#--note last year range, $0.60 - $1.20
dat %>% 
  arrange(price_recieved_corn) %>%
  distinct() %>% 
  mutate(n = 1:n(),
         trial_label = paste(trial_label, n),
         trial_label = fct_inorder(trial_label)) %>% 
  ggplot(aes(trial_label, price_recieved_corn)) + 
  geom_point() + 
  geom_hline(yintercept = 4.74, linetype = "dashed") +
  geom_hline(yintercept = 6.83, linetype = "dashed") +
  coord_flip() + 
  scale_y_continuous(labels = label_dollar()) + 
  labs(title = "2023 NASS corn prices ranged from $4.74-$6.83")

ggsave("figs_scratch/scratch_cornprices.png")
