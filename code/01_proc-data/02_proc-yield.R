#--process stefan's data
#--created 1/17/2024

library(tidyverse)
library(janitor)

rm(list = ls())

# who should we have data for? --------------------------------------------

d <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  mutate(trial_label = str_remove_all(trial_label, " ")) #--remove space from Van Horn

#--Stefan's file names use the 'trial label' capitalization/notation
coops <- 
  d |> 
  arrange(trial_label) %>% 
  select(trial_key, trial_label) 

# functions ---------------------------------------------------------------

CleanData <- function(i = 1){

  tmp.key <- coops %>% slice(i)
    
  f.data <- read_excel(paste0("data_stefan/trt_mgmt.", tmp.key$trial_label, ".ReduceN.xlsx"), 
                              sheet = "yld") 
  
  tmp.data <- 
    f.data |> 
    clean_names() |> 
    mutate_if(is.character, str_to_lower) |> 
    mutate(trial_key = tmp.key$trial_key) 
  return(tmp.data)
  
}

# let's try a loop, -------------------------------------------------------

#--column names seem standardized
dat <- NULL

for (j in 1:nrow(coops)){
  d.tmp <- CleanData(i = j)
  dat <- dat %>% bind_rows(d.tmp)
}

dat


# check -------------------------------------------------------------------

#--this should be empty (no duplicates)
dat %>%
  dplyr::group_by(trial_key, rep, trt) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

dat2 <- 
  dat |> 
  mutate(trt = ifelse(trt == "typical", "typ", "red")) %>%
  arrange(trial_key, rep, trt) %>% 
  rename(nrate_lbac = nrate, yield_buac = yield) %>% 
  select(trial_key, rep, trt, nrate_lbac, yield_buac)

dat2 |> 
  write_csv("data_tidy/td_cornyields.csv")



# make df with diffs ------------------------------------------------------

d_diff <- 
  dat2 %>% 
  pivot_longer(nrate_lbac:yield_buac) %>% 
  pivot_wider(names_from = trt, values_from = value) %>% 
  mutate(dif = typ - red,
         name = paste0("dif_", name)) %>% 
  select(trial_key, rep, name, dif) %>% 
  pivot_wider(names_from = name, values_from = dif) %>% 
  filter(!is.na(dif_yield_buac)) 

d_diff %>% 
  write_csv("data_tidy/td_trtdiffs.csv")
