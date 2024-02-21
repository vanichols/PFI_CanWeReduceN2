library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)
library(ggrepel)

rm(list = ls())

source("code/05_figs/00_fig-colors.R")

# theme -------------------------------------------------------------------

my_ghg_theme1 <- 
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

n1 <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(nrate_lbac)) %>% 
  select(trial_key, trt, nrate_lbac) %>% 
  distinct()

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


m1 <- read_csv("data_tidy/td_money.csv") %>% select(trial_key, midsav_dolac)

ghg1 <- 
  read_csv("data_tidy/td_co2e.csv") %>%
  group_by(trial_key) %>% 
  summarise(co2e_lbac = sum(co2e_lbac)) %>% 
  left_join(tk) %>% 
  left_join(y1) %>% 
  mutate(co2e_lbac2 = nrate_lbac * 7.82,
         co2e_lbbu = co2e_lbac2 / yield_buac) %>% 
  left_join(m1)


# fig 1---------------------------------------------------------------------

my_ord1 <- 
  ghg1 %>%
  group_by(trial_key) %>% 
  mutate(maxco2 = max(co2e_lbbu)) %>% 
  arrange(-maxco2) %>% 
  pull(trial_label) %>% 
  unique()
  
#--what percentage was this reduced?
ghg1 %>%
  select(trial_label, trt, co2e_lbbu) %>% 
  pivot_wider(names_from = trt, values_from = co2e_lbbu) %>% 
  mutate(red_pct = (typ - red)/typ) %>% 
  summarise(red_pct = mean(red_pct))


# christmas plot 1 --------------------------------------------------------


ghg1 %>% 
  select(trial_label, co2e_lbac, midsav_dolac) %>% 
  distinct() %>% 
  mutate(mon = ifelse(midsav_dolac < 0, "lost", "saved")) %>% 
  group_by(mon) %>% 
  summarise(n = n())
  

fig1 <- 
  ghg1 %>% 
  select(trial_label, co2e_lbac, midsav_dolac) %>% 
  distinct() %>% 
  mutate(mon = ifelse(midsav_dolac < 0, "lost", "saved")) %>% 
  ggplot(aes(co2e_lbac, midsav_dolac)) + 
  geom_hline(yintercept = 0) + 
  geom_point(size = 4) + 
  geom_label_repel(aes(label = trial_label, fill = mon),
                   color = "gray",
                   max.overlaps = 22, show.legend = F,
                   box.padding = 0.5) + 
  labs(title = str_wrap("Sixteen out of 22 trials saved money and reduced GHG emissions with reduced N rates", width = 50),
       x = "Pounds of CO2e avoided per acre",
       y = "Savings\n($/ac)") + 
  scale_fill_manual(values = c(pfi_orange, pfi_blue)) + 
  scale_color_manual(values = c("black", "white")) +
  scale_y_continuous(labels = label_dollar()) +
  my_ghg_theme1

fig1

#ggsave("figs/fig06_ghg-money.jpg", width = 8, height = 5)

ghg %>%
  select(trial_label, trt, co2e_lbbu) %>% 
  pivot_wider(names_from = trt, values_from = co2e_lbbu) %>% 
  mutate(red_pct = (typ - red)/typ) %>% 
  summarise(red_pct = mean(red_pct))


# bar chart 2 -------------------------------------------------------------

one_car_lbsco2 <- 10141.3
one_lbN_lbsco2 <- 11.1 #--from IPCC/GREET

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
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3)
  ) 


# data --------------------------------------------------------------------

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


ghg2 <- 
  n2 %>% 
  mutate(red_co2_ac = dif_nrate_lbac * one_lbN_lbsco2,
         acres_needed_per_car = one_car_lbsco2/red_co2_ac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb N/ac")) %>% 
  left_join(st2) %>% 
  distinct()



# fig ---------------------------------------------------------------------

cars_min <- min(ghg2$acres_needed_per_car) %>% round()
cars_max <- max(ghg2$acres_needed_per_car) %>% round()
cars_mean <- mean(ghg2$acres_needed_per_car) %>% round()

ghg2 %>% 
  summarise(mn = mean(dif_nrate_lbac))

fig2 <- 
  ghg2 %>%
  arrange(acres_needed_per_car) %>% 
  mutate(trial_label = fct_inorder(trial_label)) %>% 
  ggplot(aes(trial_label, acres_needed_per_car)) + 
  geom_col(color = "black", aes(fill = sig)) +
  scale_fill_manual(values = c("sig" = pfi_orange, "ns" = pfi_blue)) +
  coord_flip() +
  labs(
    title = paste0("On average*, reducing N on ", cars_mean, " acres would offset one vehicle's GHG emissions"),
    subtitle = "Reduced N on 26 million acres would offset 1 million of the USA's 300 million cars",
    x = NULL,
    y = "Acres of reduced N needed to offset one car",
    fill = "N rate reduction",
    caption = paste0("*Assumes the average reduction from this set of trials (39 lb N/ac)"))  +
  my_ghg_theme2

#ggsave("figs/fig07_ghg-cars.jpg", width = 6.5, height = 5)


# patchwork ---------------------------------------------------------------

fig1 + fig2
