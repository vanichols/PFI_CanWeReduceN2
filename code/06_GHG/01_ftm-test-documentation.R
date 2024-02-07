#--see effect of crop and location AND rates on N2O emissions in FPP
#--created 6/24/2022

library(tidyverse)
library(readxl)


# data --------------------------------------------------------------------

# NOTE: I manually added new column names, that's why it's skip = 2
# This is the more up-to-date spreadsheet, although it wasn't in the truth folder

dat <- 
  read_excel("code/06_GHG/Alfalfa N2O update 05 25 2021.xlsx",
             sheet = "N2O Calcs",
             skip = 2) %>% 
  janitor::remove_empty() %>% 
  mutate_if(is.character, str_to_lower) %>% 
  mutate_if(is.character, str_remove_all, "_") %>% 
  mutate_if(is.character, str_remove_all, "-")



dat_convfac <- 
  dat %>% 
  select(1:6)


# do own calcs ------------------------------------------------------------

#--assume 0-200 rates, by 25

res <- NULL

for (i in seq(from = 0, to = 200, by = 25)){
  
  res.tmp <- 
    dat_convfac %>% 
    mutate(Nrate_kgNha = i,
           scaled_kgN2ONha = 
             (Nrate_kgNha/Nrate_typical_fixed) * (flux_typical_fixed - bkgdflux_fixed),
           direct_N2ONha = bkgdflux_fixed + scaled_kgN2ONha,
           indirect_N2ONha = Nrate_kgNha * 0.0035,
           total_kgN2ONha = direct_N2ONha + indirect_N2ONha,
           total_lbsN2ONac = total_kgN2ONha * 2.2/2.471,
           total_CO2eq = total_lbsN2ONac * 1.57 * 296,
           default1pct_CO2eq = 0.0135*Nrate_kgNha *2.2/2.471 * 1.57 * 296)
  
  res <- bind_rows(res, res.tmp)
  
}



# viz ---------------------------------------------------------------------

#--all crops
res %>% 
  #filter(crop == "alfalfa") %>% 
  ggplot(aes(
    Nrate_kgNha,
    total_CO2eq)) + 
  geom_line(aes(color = loc_code,
                linetype = soiltexture_class))  +
  facet_wrap(~crop) + 
  theme_bw()

#--just corn
res %>% 
  filter(crop == "corn") %>% 
  ggplot(aes(
    Nrate_kgNha,
    total_CO2eq)) + 
  geom_line(aes(color = loc_code,
                linetype = soiltexture_class))  +
  facet_wrap(~crop) + 
  theme_bw()



# regress to get slopes for each unit -------------------------------------
library(broom)


nflux_model <- function(df) {
  lm(scaled_kgN2ONha ~ Nrate_kgNha, data = df)
}

tst1 <- res %>% filter(soiltexture_class == "coarse", loc_code == "c", crop == "corn")
broom::tidy(nflux_model(tst1))


gr_res <- 
  res %>% 
  filter(crop == "corn") %>% 
  group_by(loc_code, soiltexture_class) %>% 
  nest() %>% 
  mutate(model = map(data, nflux_model)) %>% 
  mutate(res = map(model, broom::tidy)) %>% 
  unnest(res)

gr_res %>% 
  filter(grepl("Nrate", term)) %>% 
  mutate(lbco2eqac)
