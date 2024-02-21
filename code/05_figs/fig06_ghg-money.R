library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)
library(ggrepel)

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

nrates <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(nrate_lbac)) %>% 
  select(trial_key, trt, nrate_lbac) %>% 
  distinct()

#--yields
y_raw <- 
  read_csv("data_stats/stat_cornyield.csv")

#--use statistical means (either grand mean or trt mean if trt is sig)
y <- 
  y_raw %>% 
  group_by(trial_key) %>% 
  mutate(grand_mean = mean(estimate),
         yield_buac = ifelse(pval < 0.05, estimate, grand_mean)
         #yield_buac = estimate
  ) %>% 
  select(trial_key, trt, yield_buac) %>% 
  left_join(nrates)


m <- read_csv("data_tidy/td_money.csv") %>% select(trial_key, midsav_dolac)

ghg <- 
  read_csv("data_tidy/td_co2e.csv") %>%
  group_by(trial_key) %>% 
  summarise(co2e_lbac = sum(co2e_lbac)) %>% 
  left_join(tk) %>% 
  left_join(y) %>% 
  mutate(co2e_lbac2 = nrate_lbac * 7.82,
         co2e_lbbu = co2e_lbac2 / yield_buac) %>% 
  left_join(m)


# fig ---------------------------------------------------------------------

my_ord <- 
  ghg %>%
  group_by(trial_key) %>% 
  mutate(maxco2 = max(co2e_lbbu)) %>% 
  arrange(-maxco2) %>% 
  pull(trial_label) %>% 
  unique()
  
#--what percentage was this reduced?
ghg %>%
  select(trial_label, trt, co2e_lbbu) %>% 
  pivot_wider(names_from = trt, values_from = co2e_lbbu) %>% 
  mutate(red_pct = (typ - red)/typ) %>% 
  summarise(red_pct = mean(red_pct))


ghg %>%
  group_by(trial_key) %>% 
  mutate(maxco2 = max(co2e_lbbu)) %>% 
  arrange(maxco2) %>% 
  mutate(trial_label = factor(trial_label, levels = my_ord)) %>% 
  ggplot(aes(trial_label, co2e_lbbu, color = trt)) +
  geom_point() +
#  coord_flip() + 
  labs(title = "Reducing N lowered CO2e released per bushel of corn by an average of 22%")
  

#--money saved and ghg/ac reduced?

ghg %>% 
  select(trial_label, co2e_lbac, midsav_dolac) %>% 
  distinct() %>% 
  mutate(mon = ifelse(midsav_dolac < 0, "lost", "saved")) %>% 
  group_by(mon) %>% 
  summarise(n = n())
  
ghg %>% 
  select(trial_label, co2e_lbac, midsav_dolac) %>% 
  distinct() %>% 
  mutate(mon = ifelse(midsav_dolac < 0, "lost", "saved")) %>% 
  ggplot(aes(co2e_lbac, midsav_dolac)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 4) + 
  geom_label_repel(aes(label = trial_label, fill = mon),
                   max.overlaps = 22, show.legend = F,
                   box.padding = 0.5) + 
  labs(title = "Sixteen out of 22 trials saved money and reduced GHG emissions with reduced N rates",
       x = "Pounds of CO2e avoided per acre",
       y = "Savings\n($/ac)") + 
  scale_fill_manual(values = c(pfi_red, pfi_green)) + 
  scale_y_continuous(labels = label_dollar()) +
  my_ghg_theme

ggsave("figs/fig06_ghg-money.jpg", width = 8, height = 5)

ghg %>%
  select(trial_label, trt, co2e_lbbu) %>% 
  pivot_wider(names_from = trt, values_from = co2e_lbbu) %>% 
  mutate(red_pct = (typ - red)/typ) %>% 
  summarise(red_pct = mean(red_pct))
