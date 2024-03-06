library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(english)


rm(list = ls())

source("code/06_figs/00_fig-colors.R")


my_money_theme <- 
  theme_bw() +
  theme(
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
    text = element_text(family = "Times New Roman")) 



# DATA --------------------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label)

#--yields-----------------

#--raw yields
y <- 
  read_csv("data_tidy/td_cornyields.csv") %>% 
  filter(!is.na(yield_buac)) %>% 
  left_join(tk)

# #--nrate and yield diffs by rep
d_diffs <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  left_join(tk)

#--raw stats on yields
st_buac <- 
  read_csv("data_stats/stat_cornyield.csv") %>% 
  left_join(tk) 

#--rename to make joins easier
st_yld <- 
  st_buac %>% 
  select(trt, estimate, trial_label, diff_est, pval) |>
  rename(yld_pval = pval,
         yld_mn = estimate,
         yld_dif_buac = diff_est) 

#--yields and stats
y_dst <- 
  y |> 
  left_join(st_yld) |> 
  select(-rep, -yield_buac) |> 
  distinct()

#--money
d_money_raw <- 
  read_csv("data_tidy/td_money.csv") %>% 
  left_join(tk) %>% 
  select(trial_key, trial_label, everything())




# YIELDS ------------------------------------------------------------------

d_fig <- 
  y_dst |> 
  mutate(yld_sig = ifelse(yld_pval < 0.05, "*", " ")) %>% 
  select(trial_label, yld_dif_buac, yld_sig) |> 
  distinct() 


nu_trials <- nrow(d_fig)

nu_redy <- nrow(d_fig %>% filter(yld_sig == "*"))

pct_redy <- round(nu_redy/nu_trials, 2) * 100

nu_redy_ch <- 
  as.character(english(nu_redy)) %>% 
  str_to_sentence()


# fig 1 -------------------------------------------------------------------

#--colors w/green and gray

fig1 <- 
  #--prepare data
  d_fig %>% 
  left_join(d_diffs %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  #--make it so it is red-typ and nice labels
  mutate(yld_dif_buac = -yld_dif_buac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb N/ac")) %>%
  mutate(yld_sig = ifelse(yld_sig == "*", "Significant change in yield", 
                          "No significant change in yield"),
         yld_sig = as.factor(yld_sig),
         yld_sig = fct_rev(yld_sig)) %>% 
  #--make fig
  ggplot(aes(reorder(trial_label, yld_dif_buac), yld_dif_buac)) +
  geom_segment(aes(x = trial_label, xend = trial_label, y = 0, yend = yld_dif_buac,
                   color = yld_sig), linewidth = 1.2, show.legend = F) +
  geom_point(aes(color = yld_sig), size = 4) +
  my_yield_theme +
  scale_y_continuous(limits = c(-25, 20),
                     breaks = c(-20, -10, 0, 10,
                                20)) +
  scale_fill_manual(values = c(pfi_green, "gray70")) +
  scale_color_manual(values = c(pfi_green, "gray70")) +
  labs(x = NULL,
       y = "Change in\ncorn yield\n(bu/ac)",
       title = str_wrap(paste0(nu_redy_ch, " (", pct_redy, "%) trials saw significant* yield declines when reducing N application"), 
                        width = 56),
       caption = "* = At 95% confidence level",
       color = NULL) + 
  my_money_theme +
  theme(legend.position = c(0.25, 0.85),
        legend.background = element_rect(fill = 'transparent', color = 'transparent')) 


fig1

#---alph test

fig1 <- 
  #--prepare data
  d_fig %>% 
  left_join(d_diffs %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  #--make it so it is red-typ and nice labels
  mutate(yld_dif_buac = -yld_dif_buac,
         trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb N/ac")) %>%
  mutate(yld_sig = ifelse(yld_sig == "*", "Significant change in yield", 
                          "No significant change in yield"),
         yld_sig = as.factor(yld_sig),
         yld_sig = fct_rev(yld_sig)) %>% 
  #--make fig
  ggplot(aes(reorder(trial_label, yld_dif_buac), yld_dif_buac)) +
  geom_segment(aes(x = trial_label, xend = trial_label, y = 0, yend = yld_dif_buac, alpha = yld_sig, color = yld_sig),
               #    color = pfi_green, 
               linewidth = 1.2, show.legend = F) +
  geom_point(aes(alpha = yld_sig, color = yld_sig), 
             #color = pfi_green, 
             size = 4) +
  scale_y_continuous(limits = c(-25, 20),
                     breaks = c(-20, -10, 0, 10,
                                20)) +
  scale_fill_manual(values = c(pfi_green, "gray")) +
  scale_color_manual(values = c(pfi_green, "gray")) +
  scale_alpha_manual(values = c(1, 0.35)) +
  labs(x = NULL,
       y = "Change in\ncorn yield\n(bu/ac)",
       title = str_wrap(paste0(nu_redy_ch, " (", pct_redy, "%) trials saw significant* yield declines when reducing N application"), 
                        width = 60),
       caption = "* = At 95% confidence level",
       alpha = NULL, 
       color= NULL) + 
  my_money_theme +
  theme(legend.position = c(0.25, 0.85),
        legend.background = element_rect(fill = 'transparent', color = 'transparent')) 


fig1

# MONEY -------------------------------------------------------------------


d_money <-
  d_money_raw %>%
  rename(value_max = bestsav_dolac,
         value_mid = midsav_dolac,
         value_min = worstsav_dolac) %>%
  mutate(clr = case_when(
    (value_max < 0 & value_min < 0) ~ "bad",
    (value_max > 0 & value_min < 0) ~ "neutral",
    (value_max > 0 & value_min > 0) ~ "good"
  ))


# ordered by max value? No order by triangle value (mean) -------------------------------------------------------------

#--create nice labels for fig
#--don't normalize
d_money_fig <-
  d_money %>% 
  left_join(d_diffs %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  mutate(#value_min = value_min / dif_nrate_lbac,
    #value_max = value_max/dif_nrate_lbac,
    #value_mid = value_mid/dif_nrate_lbac,
    trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb N/ac")) 


#--NOTE: you will need to change the x scale to match the best for the figure

nu_sav <- nrow(d_money_fig %>% filter(clr != "bad"))

pct_sav <- round(nu_sav/nu_trials, 2) * 100

nu_sav_ch <- 
  as.character(english(nu_sav)) %>% 
  str_to_sentence()


nu_loss <- 
  nrow(d_money_fig %>% 
         filter(clr == "bad"))

nu_loss_ch <- as.character(english(nu_loss)) %>% str_to_sentence()


# fig2 --------------------------------------------------------------------


fig2 <- 
  d_money_fig %>% 
  ggplot() + 
  geom_hline(yintercept = 0) +
  #--creating white background for alpha
  geom_segment(aes(
    x = reorder(trial_label, value_mid),
    xend = reorder(trial_label, value_mid),
    y = value_min,
    yend = value_max), 
    color = "white",
    linewidth = 4,
    show.legend = F
  )  +   
  #--alpha
  geom_segment(aes(
    x = reorder(trial_label, value_mid),
    xend = reorder(trial_label, value_mid),
    y = value_min,
    yend = value_max, 
    color = clr),
    alpha = 0.6,
    linewidth = 4,
    show.legend = F
  )  + 
  #--best case
  geom_segment(aes(
    x = reorder(trial_label, value_mid),
    xend = reorder(trial_label, value_mid),
    y = value_max - 5,
    yend = value_max, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  #--wrost case
  geom_segment(aes(
    x = reorder(trial_label, value_mid),
    xend = reorder(trial_label, value_mid),
    y = value_min + 5,
    yend = value_min, 
    color = clr),
    linewidth = 4,
    show.legend = F
  )  + 
  #--midpoint
  geom_point(aes(
    x = reorder(trial_label, value_mid),
    y = value_mid, 
    color = clr),
    #color = "white",
    show.legend = F,
    pch = 17,
    size = 2) +
  geom_text(aes(x = 15, y = 110), label = "Financial savings", check_overlap = T,
            #hjust = 0,
            fontface = "italic", color = pfi_blue) +
  geom_text(aes(x = 4, y = -140), label = "Financial loss", check_overlap = T,
            #hjust = 0,
            fontface = "italic", color = pfi_orange) +
  scale_y_continuous(labels = label_dollar(),
                     #                     limits = c(-15, 5),
                     #breaks = c(-400, -300, -200, -100, 0, 100, 200, 300)
  ) +
  scale_color_manual(values = c("good" = pfi_blue, "neutral" = pfi_tan, "bad" = pfi_orange)) +
  labs(x = NULL,
       y = "Dollars\nper acre\n($/ac)",
       title = str_wrap(paste0(nu_sav_ch, " (", pct_sav, "%) trials potentially saw savings when reducing N application"),
                        width = 55),
       #subtitle = paste0(nu_loss_ch, " trials saw financial losses in all price scenarios")
       ) + 
  my_money_theme



# PATCHWORK ---------------------------------------------------------------


fig1 + fig2 +
  plot_annotation(
    title = str_wrap("Yield reductions are not indicative of financial outcomes", width = 80),
    theme = theme(plot.title = element_text(size = rel(1.75)),
                  text = element_text(family = "Times New Roman")) ) 

ggsave("figs/fig04_yield-and-money.jpg", width = 10, height = 6)    




