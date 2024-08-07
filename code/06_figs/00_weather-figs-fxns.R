#--note: fonts need to be updated when chaparral becomes usable, right now they are times new roman
#--upated 1/16/2024 to be 2023 data

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)

rm(list = ls())


# theme -------------------------------------------------------------------

#--note the chaparral font is not working...using times new roman for now

my_wea_theme.fxn <- 
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = "gray50"),
          plot.title = element_text(family = "Times New Roman"),
          plot.subtitle = element_text(family = "Times New Roman"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          text = element_text(family = "Times New Roman")) 
  

# individual plots --------------------------------------------------------

#--deviation from long term average, temp

TempFigInd <- function(f.data = t, f.trial_label = "Boyer") {
  
  tmp.data <- 
    f.data |>
    group_by(trial_label, city, mm, name) |> 
    summarise(value = mean(value, na.rm = T)) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    mutate(dev_t = t_23 - t_lt,
           date = paste("2023", mm, "01", sep = "/"),
           date_mm = as_date(date),
           f_order = ifelse(trial_label == f.trial_label, 2, 1)) |> 
    arrange(f_order) |> 
    mutate(f_order = as_factor(f_order),
           f_order = fct_rev(f_order))
  
  ggplot() + 
    geom_hline(yintercept = 0) +
    geom_line(data = tmp.data, 
              aes(date_mm, dev_t, group = trial_label),
              color = pfi_tan,
              linewidth = 1,
              show.legend = F) + 
    geom_line(data = tmp.data %>% filter(trial_label == f.trial_label),
              aes(date_mm, dev_t, group = trial_label),
              color = pfi_red, 
              linewidth = 3,
              show.legend = F) + 
    geom_text(aes(x = as_date("2023/07/01"),
                  y = 8,
                  #hjust = 0,
                  label = "Warmer than average"),
              check_overlap = T,
              fontface = "italic",
              family = "Times New Roman",
              color = "gray50") +
    geom_text(aes(x = as_date("2023/07/01"),
                  y = -4,
                  #hjust = 0,
                  label = "Cooler than average"),
              check_overlap = T,
              fontface = "italic",
              family = "Times New Roman",
              color = "gray50") +
    scale_y_continuous(limits = c(-6, 13)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    labs(x = NULL,
         y = "Monthly average temperature,\ndeviation from average (deg F)") + 
    my_wea_theme.fxn
  
  
}  


CumPrecipFigInd <- function(f.data = cp, f.trial_label = "Fredericks") {
  
  tmp.data <- 
    f.data |>
    mutate(dd_date = as.Date(doy - 1, 
                             origin = "2023/01/01")) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    mutate(dev_cp = cp_23 - cp_lt)
  
  ggplot() + 
    geom_hline(yintercept = 0) +
    geom_line(data = tmp.data, 
              aes(dd_date, dev_cp, group = trial_label),
              color = pfi_tan,
              linewidth = 1,
              show.legend = F) + 
    geom_line(data = tmp.data %>% filter(trial_label == f.trial_label),
              aes(dd_date, dev_cp, group = trial_label),
              color = pfi_blue, 
              linewidth = 3,
              show.legend = F) + 
    geom_text(aes(x = as.Date("2023/07/01"), 
                  y = 13, 
                  #hjust = 0,
                  label = "Wetter than average"),
              check_overlap = T,
              color = "gray50",
              family = "Times New Roman",
              fontface = "italic") +
    geom_text(aes(x = as.Date("2023/07/01"), 
                  y = -13, 
                  #hjust = 0,
                  label = "Drier than average"),
              check_overlap = T,
              color = "gray50",
              family = "Times New Roman",
              fontface = "italic") +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    scale_y_continuous(limits = c(-15, 15), breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
    labs(x = NULL,
         y = "Cumulative precipitation,\ndeviation from average (inches)") + 
    my_wea_theme.fxn
  
}



# summary plots -----------------------------------------------------------


TempFigSummary <- function(f.data = t) {
  
  tmp.data <- 
    f.data |>
    group_by(trial_label, city, mm, name) |> 
    summarise(value = mean(value, na.rm = T)) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    mutate(dev_t = t_23 - t_lt,
           date = paste("2023", mm, "01", sep = "/"),
           date_mm = as_date(date))
  
  ggplot() + 
    geom_hline(yintercept = 0) +
    geom_line(data = tmp.data, 
              aes(date_mm, dev_t, group = trial_label),
              color = pfi_red,
              linewidth = 1,
              show.legend = F) + 
    geom_text(aes(x = as_date("2023/07/01"),
                  y = 12,
                  #hjust = 0,
                  label = "Warmer than average"),
              check_overlap = T,
              family = "Times New Roman",
              fontface = "italic",
              color = "gray50") +
    geom_text(aes(x = as_date("2023/07/01"),
                  y = -4,
                  #hjust = 0,
                  label = "Cooler than average"),
              check_overlap = T,
              family = "Times New Roman",
              fontface = "italic",
              color = "gray50") +
    scale_y_continuous(limits = c(-6, 15)) +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b") +
    labs(x = NULL,
         y = "Monthly average temperature,\ndeviation from average (deg F)") + 
    my_wea_theme.fxn
  
  
}  


CumPrecipFigSummary <- function(f.data = cp) {

  tmp.data <- 
    f.data |>
    mutate(dd_date = as.Date(doy - 1, 
                             origin = "2023/01/01")) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    mutate(dev_cp = cp_23 - cp_lt)
  
  ggplot() + 
    geom_hline(yintercept = 0) +
    geom_line(data = tmp.data, 
              aes(dd_date, dev_cp, group = trial_label),
              color = pfi_blue,
              linewidth = 1,
              show.legend = F) + 
    geom_text(aes(x = as.Date("2023/07/01"), 
                  y = 13, 
                  #hjust = 0,
                  label = "Wetter than average"),
              check_overlap = T,
              color = "gray50",
              family = "Times New Roman",
              fontface = "italic") +
    geom_text(aes(x = as.Date("2023/07/01"), 
                  y = -13, 
                  #hjust = 0,
                  label = "Drier than average"),
              check_overlap = T,
              color = "gray50",
              family = "Times New Roman",
              fontface = "italic") +
    scale_x_date(date_breaks = "1 month", 
                 date_labels = "%b",
                 limits = as.Date(c('2023-01-01','2023-12-01'))) +
    scale_y_continuous(limits = c(-15, 15), breaks = c(-15, -10, -5, 0, 5, 10, 15)) +
    labs(x = NULL,
         y = "Cumulative precipitation,\ndeviation from average (inches)") + 
    my_wea_theme.fxn
  
}

