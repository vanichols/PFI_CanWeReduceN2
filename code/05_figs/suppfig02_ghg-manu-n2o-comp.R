library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/05_figs/00_fig-colors.R")

# theme -------------------------------------------------------------------

my_ghg_theme <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                 vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.3)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
   text = element_text(family = "Times New Roman"),
   plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3)
  ) 


# data --------------------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label)

ghg_raw <- 
  read_csv("data_tidy/td_co2e.csv") %>% 
  left_join(tk) 

ghg <- 
  ghg_raw %>% 
  mutate(desc = case_when(
    grepl("manu", desc) ~ "Avoided manufacture-related GHG",
    grepl("ind-vol", desc) ~ "Avoided application-related N2O",
    grepl("direct", desc) ~ "Avoided application-related N2O",
    grepl("ind-leach", desc) ~ "Avoided leaching-related N2O",
    TRUE ~ "XX"
  )) %>% 
  mutate(desc = as.factor(desc),
         desc = fct_rev(desc))

# fig ---------------------------------------------------------------------

#--what amount is the n20?


my_ord <- 
  ghg %>% 
  group_by(trial_key) %>% 
  mutate(co2e_tot = sum(co2e_lbac)) %>% 
  arrange(co2e_tot) %>% 
  pull(trial_label) %>% 
  unique()

ghg_tot <- 
  ghg %>% 
  group_by(trial_label) %>% 
  summarise(co2e_lbac = sum(co2e_lbac)) 

ghg_min <- round(min(ghg_tot$co2e_lbac), 0)
ghg_max <- round(max(ghg_tot$co2e_lbac),0)

ghg %>% 
  group_by(trial_key) %>% 
  mutate(co2e_tot = sum(co2e_lbac)) %>% 
  arrange(co2e_tot) %>% 
  mutate(trial_label = factor(trial_label, levels = my_ord)) %>% 
  arrange(trial_label) %>% 
  ggplot(aes(trial_label, co2e_lbac, fill = desc)) + 
  geom_col() +
  coord_flip() + 
  scale_fill_manual(values = c(pfi_blue, "red4", "red3")) + 
  my_ghg_theme + 
  labs(x = NULL,
       y = "Pounds of CO2e per avoided per acre",
       fill = NULL,
       title = "Reduced N applications avoids GHG emissions from manufacturing and soil-derived N2O production",
       subtitle = paste0("Avoided emissions ranged from ", ghg_min, " to ", ghg_max,
                         " pounds of CO2 equivalents per acre"),
       caption = "*As estimated using GREET and IPCC methodologies") +
  my_ghg_theme + 
  theme(legend.position = c(0.85, 0.2))


ggsave("figs/fig06_ghg.jpg", width = 9, height = 5)
