#-notes:
# 4/24 - reran using data-supported prices for corn
# 5/23 - changed to be based on statistical yields
#--recreated/rerun on 1/18/2024
#--1/25/2024 updated prices
#--2/8/2024 - updated prices again (only using 'adjusted' n sources)

library(tidyverse)
library(lubridate)

rm(list = ls())


# data --------------------------------------------------------------------

nrates <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(nrate_lbac)) %>% 
  select(trial_key, trt, nrate_lbac) %>% 
  distinct()

#--yields
y_raw <- 
  read_csv("data_stats/stat_cornyield.csv")

#--use statistical means (either grand mean or trt mean if trt is sig)
y <- 
  y_raw %>% 
  group_by(trial_key) %>% 
  mutate(grand_mean = mean(estimate),
         yield_buac = ifelse(pval < 0.05, estimate, grand_mean)
         #yield_buac = estimate
         ) %>% 
  select(trial_key, trt, yield_buac) %>% 
  left_join(nrates)

#--range in n prices
price <- read_csv("data_tidy/td_prices.csv")

#--N high price = $1.40/lb (was $1.20/lb last year)
#--N low price = $0.31/lb (as $0.60/lb last year)
n_hi <- max(price$cost_unit_n)
n_lo <- min(price$cost_unit_n)
n_av <- mean(c(n_hi, n_lo))

#--use producer corn prices
crn_hi <- max(price$corn_price_bu, na.rm = T) # last year it was $7.48
crn_lo <- min(price$corn_price_bu, na.rm = T) # last year it was $5.70
crn_av <- mean(c(crn_hi, crn_lo))

#--make table to write/copy to report

tibble(n_cost = c(n_hi, n_av, n_lo),
       corn_rev = c(crn_lo, crn_av, crn_hi)) %>% 
  write_csv("data_tidy/td_price-summary.csv")
  
  

m <- 
  y |> 
  mutate(ncost_hi = n_hi * nrate_lbac,
         ncost_lo = n_lo * nrate_lbac,
         ncost_av = n_av * nrate_lbac,
         
         crev_hi = crn_hi * yield_buac,
         crev_lo = crn_lo * yield_buac,
         crev_av = crn_av * yield_buac) 


#--Get ranges of high and low

d_money <- 
  m |> 
  mutate(bestsav_dolac = crev_lo - ncost_hi,
         worstsav_dolac = crev_hi - ncost_lo,
         midsav_dolac = crev_av - ncost_av) |> 
  select(trial_key, trt, contains("sav")) %>%
  pivot_longer(bestsav_dolac:ncol(.)) |> 
  pivot_wider(names_from = trt, values_from = value) |> 
  mutate(savings = red - typ) |> 
  select(trial_key, name, savings) |> 
  pivot_wider(names_from = name, values_from = savings) 

d_money |> 
  write_csv("data_tidy/td_money.csv")

