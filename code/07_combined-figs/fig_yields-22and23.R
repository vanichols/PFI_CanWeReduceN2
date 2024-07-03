library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggarchery)

rm(list = ls())


source("code/07_combined-figs/00_fig-colors22.R")

# theme -------------------------------------------------------------------

my_yield_theme <- 
  theme_bw() +
  theme(
    axis.title.y = element_text(angle = 0,
                                 vjust = 0.5),
    axis.title = element_text(size = rel(1.1)),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(2), hjust = 0.5),
    panel.grid.minor = element_blank(),
   # panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
   text = element_text(family = "Times New Roman"),
   plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3)
  ) 


# 2022 data --------------------------------------------------------------------

tk22 <- 
  read_csv("2022/data_tidy/td_trialkey.csv") %>% 
  mutate(trial_label = paste0(trial_label, ",", " ", year_of_trial_harvest),
         year = 2022) %>% 
  select(trial_key, year, trial_label) 


#--yields-----------------

#--raw yields
y22 <- 
  read_csv("2022/data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(yield_buac)) %>% 
  left_join(tk22)

# #--nrate and yield diffs by rep
d_diffs22 <- 
  read_csv("2022/data_tidy/td_trtdiffs.csv") %>% 
  left_join(tk22)

#--raw stats on yields
st_buac22 <- 
  read_csv("2022/data_stats/stat_cornyield.csv") %>% 
  left_join(tk22) 

#--rename to make joins easier
st_yld22 <- 
   st_buac22 %>% 
   select(trt, estimate, trial_label, diff_est, pval) |>
   rename(yld_pval = pval,
          yld_mn = estimate,
          yld_dif_buac = diff_est) 
   
#--yields and stats
y_dst22 <- 
   y22 %>% 
   left_join(st_yld22) %>% 
   select(-rep, -yield_buac) |> 
   distinct()

#--money data------------------

d_money_raw22 <- 
  read_csv("2022/data_tidy/td_money.csv") %>% 
  left_join(tk22) %>% 
  select(trial_key, trial_label, everything())

#--money data and stats
d_money_fig22 <- 
  d_money_raw22 %>%
  rename(value_max = bestsav_dolac,
         value_mid = midsav_dolac,
         value_min = worstsav_dolac) %>%
  mutate(clr = case_when(
    (value_max < 0 & value_min < 0) ~ "bad",
    (value_max > 0 & value_min < 0) ~ "neutral",
    (value_max > 0 & value_min > 0) ~ "good"
  )) %>% 
  select(trial_label, clr, value_mid) %>% 
  #--create nice label
  mutate(value_mid_lab = ifelse(value_mid < 0, 
                                  paste0("-$", abs(round(value_mid)), "/ac"),
                                  paste0("$", round(value_mid), "/ac")))


#--combine money and yield info
d_fig22 <- 
  y_dst22 %>% 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " ")) %>% 
  select(trial_label, yld_dif_buac, yld_sig) |> 
  distinct() |> 
  left_join(d_money_fig22)


# yield diffs, money-------------------------------------------------------------

#--make them colored by financial losses
f22 <- 
  d_fig22 %>% 
  left_join(d_diffs22 %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  #--make it so it is red-typ and nice labels
  mutate(yld_dif_buac = -yld_dif_buac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb/ac")) %>% 
  #--fig
  ggplot(aes(reorder(trial_label, -yld_dif_buac), yld_dif_buac)) +
  geom_col(aes(fill = yld_sig),
               #size = yld_sig),
           color = "black",
           show.legend = F) +
  geom_text(aes(reorder(trial_label, -yld_dif_buac), 
                yld_dif_buac - 2, 
                label = yld_sig,
                hjust = 0,
                vjust = 0.75)) +
  # #--savings values
  # geom_text(aes(reorder(trial_label, -yld_dif_buac), 
  #               y = 28, 
  #               hjust = 1,
  #               label = value_mid_lab,
  #               color = clr),
  #           angle = 0,
  #           show.legend = F,
  #           fontface = "bold") +
  coord_flip() +
  my_yield_theme +
  scale_size_manual(values = c(0, 2)) +
  scale_y_continuous(limits = c(-60, 40),
                     breaks = c(-60, -40, -20, 0,
                                20, 40)) +
  scale_fill_manual(values = c(pfi_tan, pfi_green)) +
  scale_color_manual(values = c("neutral" = pfi_tan, 
                                "good" = pfi_blue,
                                "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Impact of reduced N rate on corn yield (bu/ac)\nrelative to typical N treatment",
       title = "2022 Harvest Season",
       caption = "* = Significant change in yield at 95% confidence level")




# 2023 data --------------------------------------------------------------------

tk23 <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  mutate(trial_label = paste0(trial_label, ",", " ", 2023),
         year = 2023) %>% 
  select(trial_key, year, trial_label) 


#--yields-----------------

#--raw yields
y23 <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(yield_buac)) %>% 
  left_join(tk23)

# #--nrate and yield diffs by rep
d_diffs23 <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  left_join(tk23)

#--raw stats on yields
st_buac23 <- 
  read_csv("data_stats/stat_cornyield.csv") %>% 
  left_join(tk23) 

#--rename to make joins easier
st_yld23 <- 
  st_buac23 %>% 
  select(trt, estimate, trial_label, diff_est, pval) |>
  rename(yld_pval = pval,
         yld_mn = estimate,
         yld_dif_buac = diff_est) 

#--yields and stats
y_dst23 <- 
  y23 %>% 
  left_join(st_yld23) %>% 
  select(-rep, -yield_buac) |> 
  distinct()

#--money data------------------

d_money_raw23 <- 
  read_csv("data_tidy/td_money.csv") %>% 
  left_join(tk23) %>% 
  select(trial_key, trial_label, everything())

#--money data and stats
d_money_fig23 <- 
  d_money_raw23 %>%
  rename(value_max = bestsav_dolac,
         value_mid = midsav_dolac,
         value_min = worstsav_dolac) %>%
  mutate(clr = case_when(
    (value_max < 0 & value_min < 0) ~ "bad",
    (value_max > 0 & value_min < 0) ~ "neutral",
    (value_max > 0 & value_min > 0) ~ "good"
  )) %>% 
  select(trial_label, clr, value_mid) %>% 
  #--create nice label
  mutate(value_mid_lab = ifelse(value_mid < 0, 
                                paste0("-$", abs(round(value_mid)), "/ac"),
                                paste0("$", round(value_mid), "/ac")))


#--combine money and yield info
d_fig23 <- 
  y_dst23 %>% 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " ")) %>% 
  select(trial_label, yld_dif_buac, yld_sig) |> 
  distinct() |> 
  left_join(d_money_fig23)


# yield diffs, money-------------------------------------------------------------

#--make them colored by financial losses

f23 <- 
  d_fig23 %>% 
  left_join(d_diffs23 %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  #--make it so it is red-typ and nice labels
  mutate(yld_dif_buac = -yld_dif_buac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb/ac")) %>% 
  #--fig
  ggplot(aes(reorder(trial_label, -yld_dif_buac), yld_dif_buac)) +
  geom_col(aes(fill = yld_sig),
           #size = yld_sig),
           color = "black",
           show.legend = F) +
  geom_text(aes(reorder(trial_label, -yld_dif_buac), 
                yld_dif_buac - 2, 
                label = yld_sig,
                hjust = 0,
                vjust = 0.75)) +
  # #--savings values
  # geom_text(aes(reorder(trial_label, -yld_dif_buac), 
  #               y = 28, 
  #               hjust = 1,
  #               label = value_mid_lab,
  #               color = clr),
  #           angle = 0,
  #           show.legend = F,
  #           fontface = "bold") +
  coord_flip() +
  my_yield_theme +
  scale_size_manual(values = c(0, 2)) +
  scale_y_continuous(limits = c(-60, 40),
                     breaks = c(-60, -40, -20, 0,
                                20, 40)) +
  scale_fill_manual(values = c(pfi_tan, pfi_green)) +
  scale_color_manual(values = c("neutral" = pfi_tan, 
                                "good" = pfi_blue,
                                "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Impact of reduced N rate on corn yield (bu/ac)\nrelative to typical N treatment",
       title = "2023 Harvest Season",
       caption = "* = Significant change in yield at 95% confidence level")


# together ----------------------------------------------------------------


f22 + f23

ggsave("figs_combo22and23/yields.png", width = 15, height = 7)
