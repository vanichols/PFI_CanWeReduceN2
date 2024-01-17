#--note font is times new roman in theme and geom_text

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/05_figs/00_fig-colors.R")

# theme -------------------------------------------------------------------

my_nitapp_theme <- 
  theme_bw() +
  theme(
    legend.position = c(0.2, 0.9),
    legend.text = element_text(size = rel(1.2)),
    legend.background = element_blank(),
    axis.title.y = element_text(angle = 0,
                                vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.3)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
    text = element_text(family = "Times New Roman")
  ) 


# data --------------------------------------------------------------------

#--tk
tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label, trial_label)

#--yields/nrates
y <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(yield_buac)) %>%
  left_join(tk)

# N application rates -----------------------------------------------------

#--how much did they reduce it?
d_nredp <-
  y |> 
  select(-yield_buac, -rep)  |> 
  distinct() |> 
  pivot_wider(names_from = trt, values_from = nrate_lbac) |> 
  mutate(red_pct = (typ - red)/typ * 100,
         red_abs = typ - red)

red_pct <- round(mean(d_nredp$red_pct), 0)
red_abs_min <- round(min(d_nredp$red_abs), 0)
red_abs_max <- round(max(d_nredp$red_abs), 0)
abs_min <- round(min(d_nredp$typ), 0)
abs_max <- round(max(d_nredp$typ), 0)

#--get order I want them in
my_order <- 
  y %>% 
  select(trial_label, trt, nrate_lbac) %>% 
  distinct() %>% 
  filter(trt == "typ") %>% 
  arrange(trt, nrate_lbac) %>% 
  pull(trial_label)

#--find half of difference to place percent labels
pct_lab <- 
  y %>% 
  select(trial_label, trt, nrate_lbac) %>% 
  distinct() %>% 
  pivot_wider(names_from = trt, values_from = nrate_lbac) %>% 
  mutate(xdif = typ - red,
         halfxdif = xdif/2,
         pct_lab_y = red + halfxdif) %>% 
  select(trial_label, pct_lab_y)

fig_dat <- 
  y %>% 
  select(trial_label, trt, nrate_lbac) %>% 
  distinct() %>% 
  pivot_wider(names_from = trt, values_from = nrate_lbac) %>% 
  mutate(xdif = typ - red) %>% 
  arrange(xdif) %>% 
  select(-typ) %>% 
  pivot_longer(red:xdif) %>% 
  rename(trt = name, nrate_lbac = value) %>% 
  mutate(trt = ifelse(trt == "xdif", "Typical N rate", "Reduced N rate")) %>% 
  left_join(d_nredp %>% select(-red)) %>% 
  left_join(pct_lab) %>% 
  mutate(trt_fct = factor(trt, levels = c("Typical N rate", "Reduced N rate")),
         trial_label = factor(trial_label, levels = my_order))




# fig ---------------------------------------------------------------------

fig_dat %>% 
  ggplot(aes(trial_label, nrate_lbac)) +
  geom_col(aes(fill = trt_fct), color = "black") +
  geom_text(aes(x = trial_label, y = pct_lab_y,
                label = paste0(round(red_pct, 0), "%")),
            fontface = "italic",
            color = "white") +
  scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) + 
  scale_y_continuous(breaks = seq(0, 300, 50),
                     limits = c(0, 300)) +
  labs(x = "Farmer",
       y = "Nitrogen\napplied\n(lb/ac)",
       fill = NULL,
       title = paste0("Typical N rates ranged from ", abs_min, "-", abs_max, " lb N/ac"),
       subtitle = paste0("On average, rates were reduced by ", red_pct, "% (", red_abs_min, "-",
                         red_abs_max, " lb N/ac lower than typical rate)")) + 
  my_nitapp_theme 

ggsave("figs/fig02_nrates.jpg", width = 8, height = 5.5)


