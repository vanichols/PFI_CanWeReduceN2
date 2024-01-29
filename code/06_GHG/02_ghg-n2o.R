#--figure out ghg emissions avoided
#--started 1/26/2024

library(tidyverse)
library(lubridate)
library(readxl)

rm(list = ls())

source("code/06_GHG/fxn_conversions.R")

navoided <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  select(trial_key, "avoided_lbnac" = dif_nrate_lbac) %>% 
  distinct()

# references --------------------------------------------------------------

#--co2 released per unit fuel used
r_fuelghg <- 
  read_csv("code/06_GHG/ref_fuel_ghg.csv") |> 
  mutate_if(is.character, str_to_lower) 

#--energy contents
r_fuele <- 
  read_csv("code/06_GHG/ref_fuel-energy.csv") 

#--ghg gwp for each time horizon
r_ghg <- 
  read_excel("code/06_GHG/refbyhand_gwp.xlsx", skip = 5) 

#--fert categories (urea, ammonium, etc)
r_fertcat <- 
  read_csv("code/06_GHG/refbyhand_fert-category.csv", skip = 5) |> 
  select(-notes)

#--amount of n in each type of fertilizer
r_fertn <- 
  read_csv("code/06_GHG/ref_fert-n-contents.csv") |> 
  rename(desc = fert_type) |> 
  mutate(kgn_kgfert = value) |> 
  select(desc, kgn_kgfert)


# assumptions -------------------------------------------------------------

r_ass <- read_excel("code/06_GHG/refbyhand_assumptions.xlsx", skip = 5)

#--timespan for ghg emissions
a_timeframe <- r_ass %>% filter(desc == "timespan") %>% pull(value)

# fert n2o avoided --------------------------------------------------------

#--direct + indirect

# n2o assumptions ---------------------------------------------------------

#--direct emission assumptions
a_dir <- 
  r_ass |> 
  filter(category == "n2o direct",
         desc == "kg n-n2o emitted per kg applied/residue n") |> 
  mutate(value = as.numeric(value)) %>% 
  pull(value)

#--indir asssumps, amount volatilized/leached etc.
a_indir_temp <- 
  r_ass |> 
  filter(category == "n2o indirect") |> 
  #--get rid of fertilizer types assum, deal with separately
  filter(!grepl("synthetic n,|organic n,", desc)) |>
  mutate(value = as.numeric(value)) |> 
  pivot_wider(names_from = desc, values_from = value) |> 
  janitor::clean_names() |> 
  select(-category, -unit)

#--kg n20-n released per kg volatilized ()
a_indn2o_vol_temp <- 
  a_indir_temp %>% 
  pull(kg_n_n2o_per_kg_n_volatalized)

#--kg n20-n released via leaching per kg n applied
#--assumes 24% of applied N is leached, then 1.1% of that leached N is released as N2O
a_indn20_leach <- 
  a_indir_temp %>%
  mutate(value = kg_n_leached_per_kg_n * kg_n_n2o_per_kg_n_leached) %>% 
  pull(value)

#--frac volatilized for each type of fertilizer
a_volat_f <- 
  r_ass |> 
  filter(category == "n2o indirect") |> 
  filter(grepl("synthetic n,|organic n,", desc)) |> 
  separate(desc, into = c("x", "fert_cat"), sep = ",") |> 
  select(-x) |> 
  mutate_if(is.character, str_trim) |> 
  mutate(value = as.numeric(value) * a_indn2o_vol_temp,
         category = "n20-n released per unit n applied via volatilization")

#--co2e of n2o
a_n2oe <- 
  r_ghg %>% 
  filter(time_horizon == a_timeframe,
         molecule == "n2o") %>% 
  pull(global_warming_potential)


# direct emissions calcs --------------------------------------------------

#--assumed amount of fertilizer avoided, assume all possibilities for fert type
f_avoid <- 
  crossing(navoided, fert_type = a_volat_f %>% pull(fert_cat)) %>% 
#--change units
  mutate(value = avoided_lbnac * kg_per_lb * ac_per_ha,
         unit = "kg n/ha",
         desc = "fert n",
         cat = "avoided emissions") |> 
  select(trial_key, value, unit, fert_type)

#--direct n2o emissions
f_direct <- 
  f_avoid |>  
  mutate(n2oN_kg = a_dir * value,
         n2o_kg = n2oN_kg * n_to_n2o,
         co2e_kgha = n2o_kg * a_n2oe) %>% 
  mutate(desc = "avoided n2o emissions, direct") %>% 
  select(trial_key, desc, fert_type, co2e_kgha)


# indirect emissions via volatilization/leaching --------------------------

#--volatilization
f_indvol <- 
  f_avoid |> 
  left_join(a_volat_f %>% 
              select(fert_type = fert_cat, kg_n2on_per_kg_applied_n = value)) %>% 
  mutate(n2oN_kg = value * kg_n2on_per_kg_applied_n,
         n2o_kg = n2oN_kg * n_to_n2o,
         co2e_kgha = n2o_kg * a_n2oe,
         desc = "avoided n20 emissions, ind-vol") %>% 
  select(trial_key, desc, fert_type, co2e_kgha)

#--leaching

f_indleach <- 
  f_avoid %>% 
  mutate(
    n2oN_kg = value * a_indn20_leach,
    n2o_kg = n2oN_kg * n_to_n2o,
    co2e_kgha = n2o_kg * a_n2oe,
    desc = "avoided n2o emissions, ind-leach") %>% 
  select(trial_key, desc, fert_type, co2e_kgha)
    

# combine -----------------------------------------------------------------

f_all <- 
  f_direct %>% 
  bind_rows(f_indvol) %>% 
  bind_rows(f_indleach) %>% 
  select(trial_key, desc, fert_type, co2e_kgha)

f_all %>% 
  write_csv("data_tidy/td_ghg-n2o.csv")
