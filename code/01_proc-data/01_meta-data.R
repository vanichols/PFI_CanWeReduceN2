#--make a clean meta-data file
#--read in 'meta' tab from each file in data_stefan
#--created 1/16/2024
#--just getting lat/long for now, will need to add other info

rm(list = ls())

library(tidyverse)
library(readxl)
library(tidygeocoder)


# list of files -----------------------------------------------------------

my_files <- list.files("data_stefan/")


# loop --------------------------------------------------------------------

#--test loop
dat <- NULL

for (i in 1:length(my_files)) {

  t.file <- my_files[i]
  
  t.d <- read_excel(paste0("data_stefan/", t.file), sheet = "meta")

  dat <- 
    dat %>% 
    bind_rows(t.d)
    
}
library(tm)

#--fix some things
md <- 
  dat %>% 
  mutate_if(is.character, str_to_lower) %>% 
  mutate(trial_label = str_to_title(last_name),
         last_name = removeNumbers(last_name)) %>% 
  rename(city = town) %>% 
  #--fix some issues
  mutate(trial_label = case_when(
      trial_key == "amun1_23" ~ "Amundson1",
      trial_key == "amun2_23" ~ "Amundson2",
      trial_key == "mcca1_23" ~ "McCaw1",
      trial_key == "mcca2_23" ~ "McCaw2",
      TRUE ~ trial_label
    ))

# add lat and lon ---------------------------------------------------------

#--takes awhile to run
md2 <- 
  md |> 
  geocode(city = city, state = state, method = 'osm', lat = latitude , long = longitude)



#--add field size for now, later should go into mgmt file


# field sizes --------------------------------------------------------------------

#--made a sheet by hand (field size)
# fz <- 
#   read_excel("data_raw/byhand_cooperator-field-size.xlsx", skip = 5) %>% 
#   mutate(trial_label = ifelse(grepl("veenstra", last_name), "veenstra", last_name)) 
# 
# #--average length of field? 1437
# 
# m_length <- 
#   fz %>% 
#   summarise(length_ft = mean(length_ft, na.rm = T)) %>% 
#   pull(length_ft)
# 
# #--average width? 30
# 
# m_width <- 
#   fz %>% 
#   summarise(width_ft = mean(width_ft, na.rm = T)) %>% 
#   pull(width_ft)
# 
# 
# #--already have acres
# fz_acres <- 
#   fz %>% 
#   filter(!is.na(acres))
# 
# #--fill in NAs
# fz_new <- 
#   fz %>% 
#   filter(is.na(acres)) %>% 
#   select(-acres) %>% 
#   mutate(width_ft = ifelse(is.na(width_ft), m_width, width_ft),
#          length_ft = ifelse(is.na(length_ft), m_length, length_ft),
#          ft2 = width_ft * length_ft,
#          acres = ft2 * 2.29568e-5) %>% 
#   bind_rows(fz_acres) %>% 
#   select(last_name = trial_label, strip_size_acres = acres) 
# 
# fz_new %>% 
#   summarise(ac = mean(strip_size_acres))
# 
# #--combind
# md4 <- 
#   md3 %>% 
#   left_join(fz_new, by = "last_name") %>% 
#   distinct()


md2 %>% 
  write_csv("data_tidy/td_trialkey.csv")
