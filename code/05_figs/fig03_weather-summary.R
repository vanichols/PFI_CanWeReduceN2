#--summary of all weathers at all trials
#--these functions seem to be different compared to the 'wea fxn file'
#--theme and weather files have Times New Roman
#--modified from 2022 data on 1/16/2024
#--boyer and amundson don't seem to be complete

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

rm(list = ls())
source("code/05_figs/00_weather-figs-fxns.R")
source("code/05_figs/00_fig-colors.R")


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

#--boyer should be working
w %>% 
  filter(trial_label == "Boyer") %>% 
  group_by(trial_label, city, mm, name) |> 
  summarise(value = mean(value, na.rm = T)) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  mutate(dev_t = t_23 - t_lt,
         date = paste("2023", mm, "01", sep = "/"),
         date_mm = as_date(date)) %>% 
  ggplot(aes(date_mm, dev_t)) + 
  geom_point()

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

#--add an arrow to temperature plot
fig_t <- 
  TempFigSummary(f.data = t) + 
    geom_text(aes(x = as.Date("2023-06-28"), y = -2.5, label = "Warm Springs"),
              family = "Times New Roman",
              check_overlap = T) + 
    geom_segment(aes(xend = as.Date("2023-05-15"),
                     yend = 1, 
                     x = as.Date("2023-06-05"),
                     y = -1.7),
                 arrow = arrow())
  
library(ggplotlyExtra)
library(plotly)
ggplotly(fig_t)

fig_t



fig_t + fig_cp + 
  plot_annotation(
    theme = theme_border,
    title = str_wrap("Weather deviations from historical averages", width = 80), 
    subtitle = str_wrap("Cool Aprils, dry growing seasons at all 17 trials", width = 80))

ggsave("figs/fig03_wea.jpg", width = 7, height = 5)


