#--note font is times new roman in theme and geom_text

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())

source("code/07_combined-figs/00_fig-colors22.R")

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
    plot.title = element_text(size = rel(2), hjust = 0.5),
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

#--tk22
tk22 <- 
  read_csv("2022/data_tidy/td_trialkey.csv") %>% 
  mutate(trial_label = paste0(trial_label, ",", " ", year_of_trial_harvest),
         year = 2022) %>% 
  select(trial_key, year, trial_label) 

#--tk23
tk23 <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  mutate(trial_label = paste0(trial_label, ",", " 2023"),
         year = 2023) %>% 
  select(trial_key, year, trial_label)

tk <- 
  bind_rows(tk22, tk23)
         
#--yields/nrates

y22 <- 
  read_csv("2022/data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(yield_buac)) 

y23 <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(yield_buac)) 

y <- 
  bind_rows(y22, y23) %>% 
  left_join(tk)


# N application rates -----------------------------------------------------

#--how much did they reduce it?
d_nredp <-
  y %>% 
  select(-yield_buac, -rep) %>% 
  distinct() %>% 
  pivot_wider(names_from = trt, values_from = nrate_lbac) |> 
  mutate(red_pct = (typ - red)/typ * 100)


# just make 2 separate figs and patchwork them tog ------------------------


# 2022 --------------------------------------------------------------------

#--get order I want them in
my_order22 <- 
  y %>% 
  filter(year == 2022) %>% 
  select(trial_label, trt, nrate_lbac) %>% 
  distinct() %>% 
  filter(trt == "typ") %>% 
  arrange(trt, nrate_lbac) %>% 
  pull(trial_label)

#--find half of difference to place percent labels
pct_lab22 <- 
  y %>% 
  filter(year == 2022) %>% 
  select(trial_label, trt, nrate_lbac) %>% 
  distinct() %>% 
  pivot_wider(names_from = trt, values_from = nrate_lbac) %>% 
  mutate(xdif = typ - red,
         halfxdif = xdif/2,
         pct_lab_y = red + halfxdif) %>% 
  select(trial_label, pct_lab_y)

fig_dat22 <- 
  y %>% 
  filter(year == 2022) %>% 
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
  left_join(pct_lab22) %>% 
  mutate(trt_fct = factor(trt, levels = c("Typical N rate", "Reduced N rate")),
         trial_label = factor(trial_label, levels = my_order22))

f22 <- 
  fig_dat22 %>% 
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
       title = "2022 Harvest Season") + 
  my_nitapp_theme 



# 2023 --------------------------------------------------------------------

#--get order I want them in
my_order23 <- 
  y %>% 
  filter(year == 2023) %>% 
  select(trial_label, trt, nrate_lbac) %>% 
  distinct() %>% 
  filter(trt == "typ") %>% 
  arrange(trt, nrate_lbac) %>% 
  pull(trial_label)

#--find half of difference to place percent labels
pct_lab23 <- 
  y %>% 
  filter(year == 2023) %>% 
  select(trial_label, trt, nrate_lbac) %>% 
  distinct() %>% 
  pivot_wider(names_from = trt, values_from = nrate_lbac) %>% 
  mutate(xdif = typ - red,
         halfxdif = xdif/2,
         pct_lab_y = red + halfxdif) %>% 
  select(trial_label, pct_lab_y)

fig_dat23 <- 
  y %>% 
  filter(year == 2023) %>% 
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
  left_join(pct_lab23) %>% 
  mutate(trt_fct = factor(trt, levels = c("Typical N rate", "Reduced N rate")),
         trial_label = factor(trial_label, levels = my_order23))

f23 <- 
  fig_dat23 %>% 
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
       title = "2023 Harvest Season") + 
  my_nitapp_theme 



# put together ------------------------------------------------------------

f22 + f23

ggsave("figs_combo22and23/napp.png", width = 15, height = 7)


# no white percent labels -------------------------------------------------

f22_alt <- 
  fig_dat22 %>% 
  ggplot(aes(trial_label, nrate_lbac)) +
  geom_col(aes(fill = trt_fct), color = "black") +
  # geom_text(aes(x = trial_label, y = pct_lab_y,
  #               label = paste0(round(red_pct, 0), "%")),
  #           fontface = "italic",
  #           color = "white") +
  scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) + 
  scale_y_continuous(breaks = seq(0, 300, 50),
                     limits = c(0, 300)) +
  labs(x = "Farmer",
       y = "Nitrogen\napplied\n(lb/ac)",
       fill = NULL,
       title = "2022 Harvest Season") + 
  my_nitapp_theme 

f23_alt <- 
  fig_dat23 %>% 
  ggplot(aes(trial_label, nrate_lbac)) +
  geom_col(aes(fill = trt_fct), color = "black") +
  # geom_text(aes(x = trial_label, y = pct_lab_y,
  #               label = paste0(round(red_pct, 0), "%")),
  #           fontface = "italic",
  #           color = "white") +
  scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) + 
  scale_y_continuous(breaks = seq(0, 300, 50),
                     limits = c(0, 300)) +
  labs(x = "Farmer",
       y = "Nitrogen\napplied\n(lb/ac)",
       fill = NULL,
       title = "2023 Harvest Season") + 
  my_nitapp_theme 



# together ----------------------------------------------------------------


f22_alt + f23_alt

ggsave("figs_combo22and23/napp-clean.png", width = 15, height = 7)
