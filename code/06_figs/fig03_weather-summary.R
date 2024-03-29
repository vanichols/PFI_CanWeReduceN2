#--summary of all weathers at all trials
#--these functions seem to be different compared to the 'wea fxn file'
#--theme and weather files have Times New Roman
#--modified from 2022 data on 1/16/2024
#--3/8/24 - updated titles

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

rm(list = ls())
source("code/06_figs/00_weather-figs-fxns.R")
source("code/06_figs/00_fig-colors.R")


# border for patchwork plot -----------------------------------------------

theme_border <- 
  theme(
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
    text = element_text(family = "Times New Roman"))

# data --------------------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label, city)

w <- 
  read_csv("data_tidy/td_wea.csv") %>%
  mutate_if(is.character, str_to_lower) %>% 
  arrange(city) %>% 
  full_join(tk, by = "city")

t <- 
  w %>% 
  filter(grepl("temperature", wea_type))

cp <- 
  w %>% 
  filter(grepl("precip", wea_type))


# figs --------------------------------------------------------------------

fig_cp <-
  CumPrecipFigSummary(f.data = cp)

fig_cp

#--add an arrow to temperature plot?No.
fig_t <- 
  TempFigSummary(f.data = t) #+ 
  #   geom_text(aes(x = as.Date("2023-06-28"), y = -2.5, label = "Warm Springs"),
  #             family = "Times New Roman",
  #             check_overlap = T) + 
  #   geom_segment(aes(xend = as.Date("2023-05-15"),
  #                    yend = 1, 
  #                    x = as.Date("2023-06-05"),
  #                    y = -1.7),
  #                arrow = arrow())
  
#library(ggplotlyExtra)
#library(plotly)
#ggplotly(fig_t)


# temp + precip fig -------------------------------------------------------

nu_trials <- 
  nrow(w %>% 
  select(trial_label) %>% 
  distinct())

fig_t + fig_cp + 
  plot_annotation(
    theme = theme_border,
    title = str_wrap("2023 weather deviations from 30-year historical averages", width = 80), 
    subtitle = str_wrap(paste0("Warm springs, dry finish to growing seasons at all ", 
                               nu_trials, " trials"), width = 80))

ggsave("figs/fig03_wea.jpg", width = 7, height = 5)


