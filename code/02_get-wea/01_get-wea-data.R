#--purpose: use lat/long (gotten in 01_meta-data) to ping nasa power database
#--get previous 30 years of data as historical
#--get temperature at 2 m height
#--get corrected precipitation
#--note it takes a while to run

library(nasapower)
library(tidyverse)

rm(list = ls())


#--keep just lat/lon, use city as identifier to avoid getting some places twice
d <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(city, latitude, longitude) %>% 
  distinct()

# loop --------------------------------------------------------------------

uni_cities <- 
  d |> 
  pull(city) |> 
  unique()

for (i in 1:length(uni_cities)) {
  
    c.tmp <- uni_cities[i]
  
  d.tmp <- 
    d |>
    select(city, latitude, longitude) |> 
    filter(city == c.tmp) |> 
    distinct()
  
  t.tmp <- 
    get_power(
      community = "ag",
      lonlat = c(d.tmp$longitude, d.tmp$latitude),
      pars = c("T2M", "PRECTOTCORR"),
      dates = c("1983-01-01", "2023-12-31"),
      temporal_api = "daily"
    ) |> 
    mutate(city = paste(c.tmp))
  
  t.tmp |> 
    write_csv(paste0("data_wea/raw/", c.tmp, ".csv"))
  
}
