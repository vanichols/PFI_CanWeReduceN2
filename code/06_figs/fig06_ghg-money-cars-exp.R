library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)
library(ggrepel)


#--3/6/24 - change colors to be three buckets (same as financial figure)

rm(list = ls())

source("code/06_figs/00_fig-colors.R")

# theme -------------------------------------------------------------------

my_ghg_theme1 <- 
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
   #plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3)
  ) 


# data --------------------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label)

n1 <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(nrate_lbac)) %>% 
  select(trial_key, trt, nrate_lbac) %>% 
  distinct()

n2 <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  select(trial_key, dif_nrate_lbac) %>% 
  distinct() %>% 
  left_join(tk) %>% 
  mutate(dif_nrate_lbac = round(dif_nrate_lbac, 0))

st2 <- 
  read_csv("data_stats/stat_cornyield.csv") %>% 
  mutate(sig = ifelse(pval < 0.05, "sig", "ns")) %>% 
  select(trial_key, sig)



#--yields
y_raw1 <- 
  read_csv("data_stats/stat_cornyield.csv")

#--use statistical means (either grand mean or trt mean if trt is sig)
y1 <- 
  y_raw1 %>% 
  group_by(trial_key) %>% 
  mutate(grand_mean = mean(estimate),
         yield_buac = ifelse(pval < 0.05, estimate, grand_mean)
         #yield_buac = estimate
  ) %>% 
  select(trial_key, trt, yield_buac) %>% 
  left_join(n1)


m1 <- read_csv("data_tidy/td_money.csv") 

ghg1 <- 
  read_csv("data_tidy/td_co2e.csv") %>%
  group_by(trial_key) %>% 
  summarise(co2e_lbac = sum(co2e_lbac)) %>% 
  left_join(tk) %>% 
  #left_join(y1) %>% 
  left_join(m1)


# fig 1 prep---------------------------------------------------------------------

my_ord1 <- 
  ghg1 %>%
  group_by(trial_key) %>% 
  mutate(maxco2 = max(co2e_lbac)) %>% 
  arrange(-maxco2) %>% 
  pull(trial_label) %>% 
  unique()

co2_smy <- 
  ghg1 %>% 
  summarise(min_co2 = min(co2e_lbac),
            max_co2 = max(co2e_lbac)) %>% 
  mutate_if(is.numeric, round, -1)

# GHG FIG --------------------------------------------------------

fig1 <- 
  ghg1 %>% 
  rename(value_max = bestsav_dolac,
         value_mid = midsav_dolac,
         value_min = worstsav_dolac) %>%
  mutate(mon = ifelse(value_mid > 0, "Savings", "Loss")) %>% 
  ggplot(aes(co2e_lbac, value_mid)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 4) + 
  geom_label_repel(aes(label = trial_label, fill = mon),
                   color = "gray",
                   max.overlaps = 22,
                   show.legend = F,
                   box.padding = 0.5) + 
  labs(
    #subtitle = "Financial outcomes while doing so varied",
       title = paste0("Reduced N treatments avoided ", 
                     co2_smy$min_co2, "-", co2_smy$max_co2, " lb CO2e/ac"),
       x = "Avoided GHG emissions\n(lb CO2e/ac)",
       y = "Midpoint\nfinancial\noutcome\n($/ac)",
    fill = "Midpoint financial outcome (see Figure 4)") + 
  scale_fill_manual(values = c("Loss" = pfi_orange, 
                               #"Inconclusive" = pfi_tan, 
                               "Savings" = pfi_blue)) + 
  scale_y_continuous(labels = label_dollar()) +
  my_ghg_theme1 

fig1

# bar chart 2 -------------------------------------------------------------

one_car_lbsco2 <- 10141.3
one_lbN_lbsco2 <- 7.82 #--from IPCC/GREET

# theme2 -------------------------------------------------------------------

my_ghg_theme2 <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.3)),
    panel.grid.minor = element_blank(),
    # panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    text = element_text(family = "Times New Roman"),
    #plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3)
  ) 


# data --------------------------------------------------------------------


ghg2 <- 
  n2 %>% 
  mutate(red_co2_ac = dif_nrate_lbac * one_lbN_lbsco2,
         acres_needed_per_car = one_car_lbsco2/red_co2_ac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb N/ac")) %>% 
  distinct() %>% 
  left_join(ghg1 %>% select(trial_key, 
                            bestsav_dolac, 
                            worstsav_dolac, 
                            midsav_dolac)) %>%
  distinct() %>% 
  rename(value_max = bestsav_dolac,
         value_mid = midsav_dolac,
         value_min = worstsav_dolac) %>%
  mutate(mon = ifelse(value_mid > 0, "Savings", "Loss"))


#--should be 22
tot_num <- 
  ghg2 %>% 
  nrow() %>% 
  as.numeric()

saved_mon <- 
  ghg2 %>% 
  filter(mon != "Loss") %>% 
  nrow() %>% 
  as.numeric()

saved_pct <- 
  round(saved_mon/tot_num * 100)

cars_min <- min(ghg2$acres_needed_per_car) %>% round()
cars_max <- max(ghg2$acres_needed_per_car) %>% round()
cars_mean <- mean(ghg2$acres_needed_per_car) %>% round()

ghg2 %>% 
  summarise(mn = mean(dif_nrate_lbac))

ghg2 %>% 
  filter(trial_key == "pete_23")

# ACRES OF CARS ---------------------------------------------------------------------

fig2 <- 
  ghg2 %>%
  arrange(acres_needed_per_car) %>% 
  mutate(trial_label = fct_inorder(trial_label),
         mon = factor(mon, levels = c("Savings", "Inconclusive", "Loss"))) %>% 
  ggplot(aes(trial_label, acres_needed_per_car)) + 
  geom_col(color = "black", aes(fill = mon), width = 0.75) +
  scale_fill_manual(values = c("Loss" = pfi_orange, 
                               "Savings" = pfi_blue,
                               "Inconclusive" = pfi_tan)) +
  coord_flip() +
  labs(
    title = paste0("Reduced N treatments could offset one vehicle* with ", cars_min, "-", cars_max, " acres"),
    #subtitle = "Bigger N reductions require less acreage to offset one vehicle*",
    x = NULL,
    y = "Acres of reduced N needed to offset one vehicle",
    caption = "*Assuming EPA estimates for average vehicle emissions",
    fill = "Midpoint financial outcome (see Figure 4)") +
  my_ghg_theme2 +
  theme(legend.position = c(0.75, 0.2),
        legend.background = element_rect(fill = 'transparent', color = 'transparent'))


fig2


# PATCHWORK ---------------------------------------------------------------

set.seed(1234)
fig1 + fig2 +
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom') &
  plot_annotation(
    
    title = str_wrap("All N reductions avoided GHG emissions, but with varying financial outcomes",
                     width = 80),
    
    theme = theme(
      plot.title = element_text(size = rel(1.75)),
      text = element_text(family = "Times New Roman"),
      plot.background = element_rect(
    fill = NA,
    colour = 'black',
    linewidth = 3)
    ))


ggsave("figs/fig05_ghg-money-cars.jpg", width = 10.5, height = 6)
