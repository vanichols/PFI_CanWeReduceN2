#--do stats on yields
#--did mixed and fixed effect for rep (block), results are the same
#--created 1/17/2024

library(tidyverse)
library(lubridate)
library(agricolae)
library(broom)
library(emmeans)

rm(list = ls())


# yields ------------------------------------------------------------------

d_raw <- read_csv("data_tidy/td_cornyields.csv")

d_raw %>% 
  filter(is.na(yield_buac))

#--process if needed
y <-
  d_raw

#--get the trial keys to loop through 
tk <- 
  y |> 
  pull(trial_key) |> 
  unique()



#--test individual
tst <- y |> filter(trial_key== "abel_23")


# 1. fixed effect stats -------------------------------------------------------------------

mf <- lm(yield_buac ~ trt, data = tst)

summary(mf) %>% 
  tidy()  %>%
  filter(term == "trttyp")

emmeans(mf, "trt") |> 
  tidy()

res_f <- NULL

i <- 4


for (i in 1:length(tk)) {

  tmp.tk <- tk[i]
  d.tmp <- y |> filter(trial_key== tmp.tk) 
  
  tmp.mod <- lm(yield_buac ~ trt, data = d.tmp)
  
  tmp.em <- 
    emmeans(tmp.mod, "trt") |> 
    tidy() |> 
    select(-std.error, -df, -p.value)
  
  tmp.smy <- 
    summary(tmp.mod) %>% 
    tidy()  %>%
    filter(term == "trttyp")
  
  #--ande_23 has only 3 reps, one w/a missing value. Won't produec an LSD for some reason
  
  if ("LSD" %in% names(
    (LSD.test(tmp.mod, "trt"))$statistics |> 
    as_tibble()) == 1) {
    tmp.lsd <- 
      (LSD.test(tmp.mod, "trt"))$statistics |> 
      as_tibble() |> 
      pull(LSD)} else {
        tmp.lsd <- NA
      }

  tmp.lsd
  
  tmp.res <-
    tmp.em |>
    mutate(
      trial_key= tmp.tk,
      diff_est = tmp.smy |> pull(estimate),
      pval = tmp.smy |> pull(p.value),
      lsd = tmp.lsd
    )
  
  res_f <- bind_rows(res_f, tmp.res)
    
}



res_f %>% 
  write_csv("code/03_do-stats/stats_fixed-cornyield.csv")
