library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)
library(ggrepel)

rm(list = ls())

source("code/05_figs/00_fig-colors.R")

one_car_lbsco2 <- 10141.3
one_lbN_lbsco2 <- 11.1 #--from IPCC/GREET

# theme -------------------------------------------------------------------

my_ghg_theme <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                 vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    #axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
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

nrates <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  select(trial_key, dif_nrate_lbac) %>% 
  distinct() %>% 
  left_join(tk)

ghg <- 
  nrates %>% 
  mutate(red_co2_ac = dif_nrate_lbac * one_lbN_lbsco2,
         acres_needed_per_car = one_car_lbsco2/red_co2_ac)

# fig ---------------------------------------------------------------------

cars_min <- min(ghg$acres_needed_per_car) %>% round()
cars_max <- max(ghg$acres_needed_per_car) %>% round()
cars_mean <- mean(ghg$acres_needed_per_car) %>% round()

ghg %>% 
  summarise(mn = mean(dif_nrate_lbac))


ghg %>% 
  mutate(things = "2023 trials") %>% 
  ggplot(aes(things, acres_needed_per_car)) + 
  geom_jitter(width = 0.2, size = 4, aes(fill = dif_nrate_lbac), pch = 21) +
  # geom_segment(aes(x = 0.75, xend = 1.25,
  #                  y = 26, yend = 26),
  #              linetype = "dashed", linewidth = 1.5) +
  geom_hline(yintercept = 26, linetype = "dashed", size = 1.5) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_fill_viridis_c(option = "magma") +
  my_ghg_theme + 
  labs(
    title = paste0("On average*, reducing N on ", cars_mean, " acres would offset one vehicle's GHG emissions"),
    subtitle = "Reduced N on 26 million acres would offset 1 million of the USA's 300 million cars",
      x = NULL,
       y = "Acres of\nreduced N\nneeded to offset\none car",
    fill = "N rate reduction",
    caption = paste0("*Actual acres needed depends on the actual amount of N reduced, and ranges from ",
                     cars_min, "-", cars_max, " acres\nThe value presented assumes the average reduction from this set of trials (39 lb N/ac)")) 

ggsave("figs/fig07_ghg-cars.jpg", width = 6.5, height = 5)

