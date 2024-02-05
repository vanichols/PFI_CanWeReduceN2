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

ghg <- 
  read_csv("data_tidy/td_co2e.csv") %>% 
  left_join(tk) %>% 
  arrange(FTMco2e_lbac)



# fig ---------------------------------------------------------------------

ghg_tot_lab <- 
  ghg %>% 
  mutate(year = 2023,
         co2e_lb = co2e_lbac * 4, #--assumed strips of 1 acre (close to average)
         cumco2e_lb = cumsum(co2e_lb)) %>% 
  filter(cumco2e_lb == max(cumco2e_lb)) %>%
  mutate(co2e_tons = cumco2e_lb/2000) %>% 
  pull(co2e_tons) %>% 
  round(., 1)

big_dot <- 
  ghg %>% 
  mutate(year = 2023,
         co2e_lb = co2e_lbac * 4, #--assumed strips of 1 acre (close to average)
         cumco2e_lb = cumsum(co2e_lb)) %>% 
  filter(cumco2e_lb == max(cumco2e_lb))
  

ghg %>% 
  mutate(year = 2023,
         co2e_lb = co2e_lbac * 4, #--assumed strips of 1 acre (close to average)
         cumco2e_lb = cumsum(co2e_lb),
         trial_label = fct_inorder(trial_label),
         trial_labelN = as.numeric(trial_label)) %>% 
  ggplot(aes(trial_label, cumco2e_lb, group = 1)) + 
  geom_line() +
  geom_point(size = 3, color = "black") +
  geom_point(data = big_dot, size = 5, color = pfi_red) +
  geom_label(aes(x = 14, y = 27000), 
            label = paste0(ghg_tot_lab, " tons of CO2e avoided")) +
  scale_y_continuous(labels = label_comma(), limits = c(0, 30000)) +
  scale_x_discrete(expand = expansion(mult = c(0.05, .2))) +
  labs(y = "Cumulative\npounds\nof CO2e\navoided",
       x = NULL,
       title = "PFI cooperators avoided GHG emissions* through N-rate trials",
       subtitle = str_wrap("N fertilizer emits GHGs during manufacturing and through nitrous oxide released from the soil after application", 90),
       caption = "*As estimated using the Fieldprint Calculator\nGHG = Greenhouse gas\nN = nitrogen") +
  my_ghg_theme


ggsave("figs/fig06_ghg.jpg", width = 6, height = 5)
