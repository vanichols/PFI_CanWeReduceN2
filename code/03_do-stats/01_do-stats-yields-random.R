#--do stats on yields
#--did mixed and fixed effect for rep (block)
#--created 1/17/2024
#--had issues with Matrix/lme4, did this and it works now 
# https://stackoverflow.com/questions/77481539/error-in-initializeptr-function-cholmod-factor-ldeta-not-provided-by-pack

library(tidyverse)
library(lubridate)
library(agricolae)
library(broom)
library(emmeans)
library(LSDer)
library(lme4)
library(lmerTest)

#tools::package_dependencies("Matrix", which = "LinkingTo", reverse = TRUE)[[1L]]
#install.packages("lme4", type = "source")

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



#--test individual, will fail for ande_23
tst <- y |> filter(trial_key== "abel_23")

tst2 <- y |> filter(trial_key== "ande_23")


# 1. random effect stats -------------------------------------------------------------------


mr <- lmer(yield_buac ~ trt + (1|rep), data = tst)

pval2 <- 
  anova(mr) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  pull(pr_f)

lsd2 <- 
  LSDer(mr, "trt", comps = NULL, level = 0.95) |> 
  as.numeric() 


res_r <- NULL

i <- 1

for (i in 1:length(tk)) {
  
  tmp.tk <- tk[i]
  d.tmp <- y |> filter(trial_key== tmp.tk) 
  
  tmp.mod <- lmer(yield_buac ~ trt + (1|rep), data = d.tmp)
  
  tmp.em <- 
    emmeans(tmp.mod, "trt") |> 
    tidy() |> 
    select(-std.error, -df, -p.value)
  
  tmp.smy <- 
    pairs(emmeans(tmp.mod, "trt")) %>% 
    tidy() %>% 
    pull(estimate)
  
  tmp.pval <- 
    anova(tmp.mod) |> 
    as_tibble() |> 
    janitor::clean_names() |> 
    pull(pr_f)
  
  tmp.lsd <- 
    LSDer(tmp.mod, "trt", comps = NULL, level = 0.95) |> 
    as.numeric() 
  
  tmp.res <- 
    tmp.em %>% 
    mutate(trial_key= tmp.tk,
           diff_est = -tmp.smy,
           pval = tmp.pval,
           lds = tmp.lsd)
  
  
  
  res_r <- bind_rows(res_r, tmp.res)
  
}

res_r %>% 
  rename(lsd = lds) %>% 
  write_csv("code/03_do-stats/stats_random-cornyield.csv")
