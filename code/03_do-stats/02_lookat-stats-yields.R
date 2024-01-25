#--look at fixed versus random models
#--1/25/2024

library(tidyverse)

rm(list = ls())


# data --------------------------------------------------------------------

res_f <- read_csv("code/03_do-stats/stats_fixed-cornyield.csv")
res_r <- read_csv("code/03_do-stats/stats_random-cornyield.csv")


# compare -----------------------------------------------------------------

res_comp <-
  res_f |>
  mutate(sig_fixed = ifelse(pval < 0.05, "y", "n")) |>
  select(-pval, -lsd) |>
  left_join(
    res_r |>
      mutate(sig_rand = ifelse(pval < 0.05, "y", "n")) |>
      select(trt, trial_key, sig_rand)
  ) |>
  mutate(dif = ifelse(sig_fixed == sig_rand, "n", "y"))

#--sifnificances are the same except for abel, random makes them not sig
res_comp %>% 
  filter(dif == "y")

res_f %>% 
  filter(trial_key == "abel_23")
res_r %>% 
  filter(trial_key == "abel_23")

#--random effects gives lsd for anderson...
res_f %>% 
  filter(is.na(lsd))

res_r %>% 
  filter(is.na(lsd))

# write random effect results ----------------------------------------------

res_r %>% 
  write_csv("data_stats/stat_cornyield.csv")

