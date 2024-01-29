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

# energy avoided ----------------------------------------------------------

e <- read_csv("data_tidy/td_fert-energy-avoided.csv")


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

# energy ghg calcs -------------------------------------------------------------------

#--change energy to ghg emissions
#--requires assumptions about source of energy

#--what source to use for ghg emissions? EPA
a_dsghg <- r_ass %>% filter(desc == "ghg from fuel") %>% pull(value)

#--fuel energy content
a_fuelec <- r_ass %>% filter(desc == "fuel energy content") %>% pull(value)


#--kg co2 per unit fuel 
e_ghg <- 
  r_fuelghg |> 
  filter(grepl(a_dsghg, source)) |> #--assumed energy content info
  filter(time_horizon == a_timeframe) #--assumed timeframe

#--energy content
e_fuele <- 
  r_fuele |> 
  filter(source == a_fuelec) |> 
  mutate(energy_mj_per_l = value) |> 
  select(fuel_type, energy_mj_per_l)

#--get kg co2e released per mj of a fuel used
e_fuelghg <- 
  e_ghg |> 
  left_join(e_fuele) |> 
  mutate(value2 = case_when(
    unit == "kg/l" ~ value * (1/energy_mj_per_l), #--diesel and gasoline
    unit == "kg/kwh" ~ value * kwh_per_btu * btu_per_mj), #--electicity
    unit2 = "kg co2e/mj") |> 
  select(fuel_type, unit2, value2)


#--cross with all the fuel types
e1 <- 
  crossing(e, fuel_type = e_fuele %>% pull(fuel_type)) %>% 
  rename(value = mj_avoided_ha)
  

#--co2 released by fuel type, not sure this is the right way to do this. GREET probably has a mix of fuel sources it assumes
#--energy required isn't the same as energy used, bc different fuels produce energy w/different efficiencies
#--all the values are very close, just average them

e2 <- 
  e1 %>% 
  left_join(e_fuelghg) %>% 
  group_by(trial_key, unit2) %>% 
  summarise(value  = mean(value),
            value2 = mean(value2)) %>% 
  mutate(value4 = value * value2,
         unit4 = "kg co2e/ha") %>%
  select(trial_key, value4, unit4) %>% 
  rename(unit = unit4,
         value = value4)

e2 %>% 
  ungroup() %>% 
  arrange(value) %>% 
  mutate(n = 1:n(),
         trial_key = paste(trial_key, n),
         trial_key = fct_inorder(trial_key),
         cum_value = cumsum(value)) %>% 
  ggplot(aes(trial_key, cum_value)) + 
  geom_point() +
  labs(y = "Cumulative fertilizer manufacturing kg CO2e avoided per hectare")

e2 %>%
  mutate(desc = "avoided fert manu ghg emissions") %>% 
  select(trial_key, desc, co2e_kgha = value) %>% 
  write_csv("data_tidy/td_ghg-energy.csv")
