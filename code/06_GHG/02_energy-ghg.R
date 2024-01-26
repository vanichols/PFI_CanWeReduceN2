#--figure out ghg emissions avoided
#--started 1/26/2024

library(tidyverse)
library(lubridate)
library(readxl)

rm(list = ls())

source("code/06_GHG/fxn_conversions.R")


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

#--what source to use for ghg emissions? EPA
a_dsghg <- r_ass %>% filter(desc == "ghg from fuel") %>% pull(value)

#--fuel energy content
a_fuelec <- r_ass %>% filter(desc == "fuel energy content") %>% pull(value)

#--timespan for ghg emissions
a_timeframe <- r_ass %>% filter(desc == "timespan") %>% pull(value)

# calcs -------------------------------------------------------------------

#--change energy to ghg emissions
#--requires assumptions about source of energy


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


# 3.  fert n2o (f)---------------------------------------------------------------------

#--calculate n2o emissions
#--use fertilizer n and plant n inputs
#--direct + indirect


#--direct emission assumptions
f_dir <- 
  d_o |> 
  filter(cat == "n2o direct") |> 
  mutate(value = as.numeric(value)) |> 
  pivot_wider(names_from = desc, values_from = value) |> 
  janitor::clean_names() |> 
  select(-cat, -unit)

#--indir asssumps, amount volatilized/leached etc.
f_indir <- 
  d_o |> 
  filter(cat == "n2o indirect") |> 
  #--get rid of fertilizer types assum, deal with separately
  filter(!grepl("synthetic n,|organic n,", desc)) |>
  mutate(value = as.numeric(value)) |> 
  pivot_wider(names_from = desc, values_from = value) |> 
  janitor::clean_names() |> 
  select(-cat, -unit)

#--frac emitted for each type of fertilizer
f_indir_f <- 
  d_o |> 
  filter(cat == "n2o indirect") |> 
  filter(grepl("synthetic n,|organic n,", desc)) |> 
  separate(desc, into = c("x", "fert_cat"), sep = ",") |> 
  select(-x) |> 
  mutate_if(is.character, str_trim) |> 
  mutate(value = as.numeric(value))

#--this is a hack
f_id <- 
  d_o |> 
  pull(scenario_id) |> 
  unique()

#--assumed crop following alfalfa

f_crop <- 
  d_o |> 
  filter(cat == "fertilizer avoidance",
         desc == "subsequent crop") |> 
  pull(value)

#--assumed amount of fertilizer avoided
f_amount <- 
  d_o |> 
  filter(cat %in% c("fertilizer avoidance")) |> 
  filter(grepl(f_crop, desc)) |> 
  mutate(avoided_lbnac = as.numeric(value)) |> 
  select(scenario_id, avoided_lbnac)

#--assumed type of fert avoided (they have different volatilities)
f_type <- 
  d_o |> 
  filter(cat %in% c("fertilizer avoidance")) |> 
  filter(desc == "type of fertilizer avoided") |> 
  mutate(fert_type = value) |> 
  select(scenario_id, fert_type)

f_avoid <- 
  f_amount |> 
  left_join(f_type)

#--actual n2o emissions

# note it is assumed all the n2o is released when the stand is terminated
# therefore the total is amortized over the standlife. Longer standlife = less n2o
# from IPCC 2019 refinement: the nitrogen residue from forages is only accounted for during pasture renewal


#--calculate kg of N applied per ha via fertilizers

#--get n per unit fertilizer
f1 <- 
  d_p |> 
  filter(cat == "fertility") |> 
  mutate(value = as.numeric(value))

#--calc applied n via fertilization
f2 <- 
  f1 |> 
  rename("fert_type" = desc) |> 
  left_join(r_fertcat) |> 
  left_join(r_fertn |> rename("fert_type" = desc)) |> 
  mutate(f_ntot = value * kgn_kgfert) |> 
  group_by(scenario_id, fert_cat) |> 
  summarise(value = sum(f_ntot, na.rm = T),
            unit = "kg n/stand",
            desc = "fert n")

#--get n from plants

#--amount of harvested dry matter each year
f3 <- 
  d_p |> 
  filter(cat == "yield") |> 
  filter(unit == "kg/stand") |> 
  mutate(value = as.numeric(value)) |> 
  group_by(scenario_id) |> 
  summarise(value = sum(value)) |> 
  left_join(p_sl) |> 
  mutate(kgdm_per_year = value/stand_life_yrs) |>
  select(scenario_id, stand_life_yrs, kgdm_per_year) 


#---use IPCC assumptions to get root N and residue N
#--note this is a hack - need to think about combining them more cleanly
f4 <- 
  f3 |> 
  mutate(scenario_id = f_id) |>#--they don't have a common column 
  left_join(f_dir)  |>  
  mutate(
    dm_n = kgdm_per_year * fraction_of_dm_not_harvested * kg_of_n_per_kg_dm,
    root_n = kgdm_per_year * kg_roots_per_kg_dm_harvested * kg_n_per_kg_root_dm,
    plant_n_kg = (dm_n + root_n)/stand_life_yrs 
  )

#--clean up to merge with fert n
f5 <- 
  f4 |> 
  mutate(value = plant_n_kg,
         unit = "kg n/stand",
         desc = "plant n") |> 
  select(scenario_id, scenario_id, desc, value, unit)

#--fert n and plant n 'applied'    
f6 <- 
  f2 |> 
  bind_rows(f5) |> 
  fill(scenario_id, .direction = "updown") |> 
  mutate(cat = "emissions")

#--now do avoided fert amount

f_avoid1 <-
  f_avoid |> 
  left_join(r_fertcat) |> 
  mutate(value = avoided_lbnac * kg_per_lb * ac_per_ha,
         unit = "kg n/stand",
         desc = "fert n",
         cat = "avoided emissions") |> 
  select(scenario_id, cat, desc, unit, value, fert_cat)


#--combine the n sources
f7_all <- 
  f6 |> 
  bind_rows(f_avoid1) |> 
  ungroup() |> 
  fill(scenario_id, .direction = "downup")


#--direct n2o emissions

f8 <- 
  f7_all |>  
  left_join(f_dir) |> 
  mutate(n2oN_kg = kg_n_n2o_emitted_per_kg_applied_residue_n * value,
         n2o_kg = n2oN_kg * n_to_n2o) |> 
  select(-value, -n2oN_kg) 

#---just look at it
# f8 |> 
#   mutate(co2_eq_kg = n2o_kg * o_gwpn2o) |> 
#   ggplot(aes(desc, co2_eq_kg)) + 
#   geom_col(aes(fill = desc), color = "black") + 
#   facet_grid(.~cat) +
#   labs(y = "kg co2-eq per ha",
#        x = NULL,
#        title = "N2O emissions, IPCC method") 


f9_dir <- 
  f8 |> 
  mutate(value = n2o_kg * o_gwpn2o,
         unit = "kg co2e/stand") |> 
  mutate(desc = paste0("direct, ", desc)) |> 
  select(scenario_id, scenario_id, cat, desc, unit, value)

#--indirect n2o emissions

#--assign the fertilizer to the correct category for volatilization
# (urea, ammonium, nitrate, ammonium-nitrate)
#--add in volatilization value
f10 <- 
  r_fertcat |> 
  rename("desc2" = fert_type) |> 
  left_join(f_indir_f |> 
              select(fert_cat, value)) |> 
  rename("kg_n_volatized_per_kg_applied_n" = value)


#--get all the constants lined up
#--note if you apply many types of fertilizer, each should be in a row here

f11 <- 
  f7_all |> 
  # #--get the type of fertilizer it is
  # left_join(f1 |> 
  #             select(scenario_id, desc) |> 
  #             rename("desc2" = desc) |> 
  #             mutate(desc = "fert n")) |> 
  #--add the category and assumed %N volatilization
  left_join(f10) |> 
  left_join(f_indir) 

#--do the calcs for volatization - plant n is not included here
f12_vol <- 
  f11 |> 
  filter(desc != "plant n") |> 
  mutate(value2 = 
           value * kg_n_volatized_per_kg_applied_n * kg_n_n2o_per_kg_n_volatalized,
         unit = "kg n2o-n vol/stand", 
         desc = "indirect, volatilize") |> 
  group_by(scenario_id, cat, unit, desc) |> 
  #--sum together in case there are multiple fertilizers
  summarise(value = sum(value2))

#--do the calcs for leaching
f13_leach <- 
  f11 |> 
  mutate(value2 = 
           value * kg_n_leached_per_kg_n * kg_n_n2o_per_kg_n_leached,
         unit = "kg n2o-n leach/stand",
         #--change desc from plant n/fert n to just indirect, leach
         desc = "indirect, leach") |> 
  group_by(scenario_id, cat, unit, desc) |> 
  #--sum them together (plant + all fertilizers)
  summarise(value = sum(value2))

#--comnbine volat and leach values, change to co2e
f14_ind <- 
  f12_vol |> 
  bind_rows(f13_leach) |> 
  mutate(value = value * n_to_n2o * o_gwpn2o,
         unit = "kg co2e/stand") 

#--combine direct and indirect emissions

f15 <- 
  f9_dir |> 
  bind_rows(f14_ind) |> 
  mutate(cat = str_replace(cat, "emissions", "n2o")) 

#--if it is avoided, it is negative
f16 <- 
  f15 |> 
  mutate(value = ifelse(cat == "n2o", value, -value))

g3 <- f16
