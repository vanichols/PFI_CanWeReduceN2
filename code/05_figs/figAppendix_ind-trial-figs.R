#--make summary figure w/weather, money, yields
#--note the fonts aren't working, cufrently times new roman
#--want to add GHG emissions somehow

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)


rm(list = ls())

source("code/05_figs/00_weather-figs-fxns.R")
source("code/05_figs/00_fig-colors.R")


# theme -------------------------------------------------------------------

my_combo_theme <-
  theme_bw() +
  theme(
    axis.title = element_text(size = rel(1.1)),
    strip.text = element_text(size = rel(1.2)),
    strip.background = element_rect(fill = pfi_tan),
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    plot.title = element_text(size = rel(1.1)),
    panel.grid.minor = element_blank(),
    # panel.grid.major.y = element_blank(),
    plot.caption = element_text(hjust = 1),
    #panel.border = element_blank(),
    #plot.title.position = "plot",
    plot.caption.position =  "plot",
    text = element_text(family = "Times New Roman")
  )

#--for overal patchworked fig
theme_border <- 
  theme(
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
    text = element_text(family = "Times New Roman"))

# general data --------------------------------------------------------------------
 
tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label) 

d_diffs <- 
  read_csv("data_tidy/td_trtdiffs.csv") %>% 
  left_join(tk)

d_ghg <- 
  read_csv("data_tidy/td_co2e.csv") %>% 
  select(-co2e_kgha) %>% 
  mutate(co2e_lbac = round(co2e_lbac, 0),
         FTMco2e_lbac = round(FTMco2e_lbac, 0))

# yields ------------------------------------------------------------------

y <- read_csv("data_tidy/td_cornyields.csv") %>% 
  left_join(tk) %>% 
  mutate(yield_buac = round(yield_buac, 0))

#--yield stats for LSD
y_st <- 
  read_csv("data_stats/stat_cornyield.csv") %>% 
  left_join(tk)

#--need estimated reduction
#--make it say 'No statistical difference in yields'
#--get rid of 'rounding' and add Least Signifant Difference (LSD)
y2 <-
  y |>  
  left_join(y_st) |> 
  #--get rid of bakehouse's NA rep
  filter(!is.na(yield_buac)) %>%  
  mutate(trt = ifelse(trt == "typ", 
                      paste0("Typical"), #,\n", round(nrate_lbac, 0), " lb N/ac"),
                      paste0("Reduced")),#\n", round(nrate_lbac, 0), " lb N/ac")),
         trt = as.factor(trt),
         trt = fct_rev(trt),
         dir = ifelse(diff_est < 0, "increase", "reduction")) %>% 
  mutate(lsd_lab = paste("Least significant difference (LSD)\nat 95% confidence level of", round(lsd, 0), "bu/ac"),
         yld_lab = paste(round(estimate, 0), "bu/ac")) %>% 
  group_by(trial_label) %>% 
  mutate(yield_max = max(yield_buac, na.rm = T)) 

#--fix the rounding issue
#--new diff_est
y_diff <- 
  y2 %>% 
  select(trial_key, trt, estimate, diff_est) %>% 
  distinct() %>% 
  pivot_wider(names_from = trt, values_from = estimate) %>% 
  mutate(
    diff_calc = round(Typical, 0) - round(Reduced, 0)) %>% 
  select(trial_key, diff_calc)

#--using FTM values 
y3 <- 
  y2 %>% 
  select(-diff_est) %>% 
  left_join(y_diff) %>% 
  rename(diff_est = diff_calc) %>% 
  #--put amount of N in trt label
  mutate(trt = paste0(trt, ",\n", nrate_lbac, "lb N/ac"),
         sig_lab = 
           case_when(
             (pval < 0.05) ~ paste("Significant", dir, "of", abs(round(diff_est, 0)), "bu/ac"),
             (pval > 0.05) ~ "No statistical difference in yields")) %>% 
  left_join(d_ghg) %>% 
  mutate(co2lab = paste("GHG emissions reduced by", FTMco2e_lbac, "lb CO2e per acre"))

# money -------------------------------------------------------------------

d_money_raw <- 
  read_csv("data_tidy/td_money.csv") %>% 
  left_join(tk) %>% 
  select(trial_key, trial_label, everything())

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

#--farmer plus reduction label
m <-
  d_money %>% 
  left_join(d_diffs %>% select(trial_label, dif_nrate_lbac) %>% distinct()) %>% 
  #mutate(trial_label = paste0(trial_label, ", -", round(dif_nrate_lbac), " lb/ac")) %>% 
  mutate(
    value_max_lab = ifelse(value_max < 0, 
                              paste0("-$", abs(round(value_max, 0)), "/ac"), 
                              paste0("+$", abs(round(value_max, 0)), "/ac")),
    value_min_lab = ifelse(value_min < 0, 
                               paste0("-$", abs(round(value_min, 0)), "/ac"), 
                               paste0("+$", abs(round(value_min, 0)), "/ac")),
    value_mid_lab = ifelse(value_mid < 0, 
                             paste0("-$", abs(round(value_mid, 0)), "/ac"), 
                             paste0("+$", abs(round(value_mid, 0)), "/ac"))
  ) 



# names to loop through ---------------------------------------------------


my_names_raw <- 
  m |> 
  pull(trial_label) |> 
  unique()


# weather -----------------------------------------------------------------

tkcity <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label, city, state)

w <- 
  read_csv("data_tidy/td_wea.csv") %>% 
  full_join(tkcity, by = "city", relationship = "many-to-many")

t <- 
  w %>% 
  filter(grepl("temperature", wea_type))

cp <- 
  w %>% 
  filter(grepl("precip", wea_type))




# functions ---------------------------------------------------------------

#--test
y.tst <- y3 %>% filter(trial_key == "abel_23")

YieldFig <- function(y.data = y.tst) {
  
  caption.lsd <- y.data %>% pull(lsd_lab) %>% unique()
  
  fig <- 
    y.data %>% 
    arrange(trt) %>% 
    mutate(trt = fct_rev(trt)) %>% 
    mutate(rep_lab = paste0("Rep ", rep)) %>% 
    ggplot(aes(trt, yield_buac)) +
    geom_col(aes(group = rep, fill = trt),
             color = "black",
             position = position_dodge(width = .9),
             show.legend = F) +
    #--yields
    geom_text(aes(x = trt,
                  y = 1.15 * yield_max,
                  label = yld_lab),
              check_overlap = T,
              fontface = "italic", 
              family = "Times New Roman",
              color = "gray50") +
    #-rep labels
    geom_text(aes(x = trt, y = 5, label = rep_lab, group = rep),
              position = position_dodge(width = .9), 
              angle = 90,
              hjust = 0, 
              family = "Times New Roman",
              color = "gray") +
    #--diff
    geom_text(aes(x = 1.5,
                  y = 1.75 * yield_max,
                  label = sig_lab),
              check_overlap = T,
              family = "Times New Roman",
              color = "gray50") +
    #--ghg avoided
    geom_text(aes(x = 1.5,
                  y = 1.5 * yield_max,
                  label = co2lab),
              check_overlap = T,
              family = "Times New Roman",
              color = "gray50") +
    scale_y_continuous(expand = expansion(0.15)) +
    scale_fill_manual(values = c(pfi_dkgreen, pfi_green)) +
    my_combo_theme +
    theme(axis.text.x = element_text(size = rel(1.1))) +
    labs(x = NULL,
         y = "Bushels per ac",
         title = "Corn yield response",
        caption = caption.lsd)
  
  return(fig)
  
}

# tst <- y |>
#    filter(trial_label == my_names[7])
# 
# fig1 <- YieldFig(y.data = tst)
# 
# fig1

m.tst <- m %>% filter(trial_label == "Aukes")

MoneyFig <- function(m.data = m.tst) {
  
  m.data %>%
    ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    #--range, white
    geom_segment(
      aes(
        x = trial_label,
        xend = trial_label,
        y = value_min,
        yend = value_max,
      ),
      color = "white",
      linewidth = 8,
      show.legend = F
    )  +
    #--range alpha
    geom_segment(
      aes(
        x = trial_label,
        xend = trial_label,
        y = value_min,
        yend = value_max,
        color = clr
      ),
      alpha = 0.5,
      linewidth = 8,
      show.legend = F
    )  +
    #--top of range
    geom_segment(
      aes(
        x = trial_label,
        xend = trial_label,
        y = value_max - 5,
        yend = value_max,
        color = clr
      ),
      linewidth = 8,
      show.legend = F
    ) +
    #--bottom of range
    geom_segment(
      aes(
        x = trial_label,
        xend = trial_label,
        y = value_min,
        yend = value_min + 5,
        color = clr
      ),
      linewidth = 8,
      show.legend = F
    ) +
  #--midpoint
    geom_point(
      aes(x = trial_label,
          y = value_mid,
          color = clr),
      pch = 17,
      show.legend = F,
      size = 4
    ) +
    #--arrows
    geom_segment(aes(
      xend = 1,
      x = 1.3,
      yend = value_max + 1,
      y = value_max + 12
    ),
    color= "gray50",
    arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(
      xend = 1,
      x = 1.2,
      yend = value_min - 1,
      y = value_min - 15
    ),
    color= "gray50",
    arrow = arrow(length = unit(0.2, "cm"))) +
    geom_segment(aes(
      xend = 1.05,
      x = 1.35,
      yend = value_mid,
      y = value_mid + 2
    ),
    color= "gray50",
    arrow = arrow(length = unit(0.2, "cm"))) +
    #--text
    geom_text(
      aes(
        x = 1.25,
        y = value_max + 12,
        label = paste0(
          "Best-case, ",
          value_max_lab,
          "\n  (expensive N, low corn revenue)"
        )
      ),
      check_overlap = T,
      hjust = 0,
      family = "Times New Roman",
      fontface = "italic",
      color = "gray50"
    ) +
    geom_text(
      aes(
        x = 1.4,
        y = value_min - 15,
        label = paste0(
          "Worst-case, ",
          value_min_lab,
          "\n  (cheap N,  high corn revenue)"
        )
      ),
      check_overlap = T,
      hjust = 0,
      family = "Times New Roman",
      fontface = "italic",
      color = "gray50"
    ) +
    geom_text(
      aes(
        x = 1.4,
        y = value_mid + 2,
        label = paste0("Midpoint, ", value_mid_lab)
      ),
      check_overlap = T,
      hjust = 0,
      family = "Times New Roman",
      fontface = "italic",
      color = "gray50"
    ) +
    scale_y_continuous(labels = label_dollar(), expand = expansion(mult = 0.2)) +
    expand_limits(x = 3.5) +
    my_combo_theme +
    theme(axis.text.x = element_text(size = rel(1.1))) +
    scale_color_manual(values = c(
      "good" = pfi_blue,
      "neutral" = pfi_tan,
      "bad" = pfi_orange
    )) +
    labs(
      x = NULL,
      y = "Dollars per acre",
      title = str_wrap(
        "Financial outcome",
        width = 40
      ),
      caption = "N prices ranged from $0.60-$1.20/lb N\n Corn revenue ranged from $5.70-$7.48/bu"
    )
  
  
  
}

MoneyFig()

# loop it -----------------------------------------------------------------

#--to label the appendix
my_letters <- letters %>% str_to_upper()

#--to loop through
my_names <- y %>% select(trial_label) %>% distinct() %>% pull(trial_label)

#--for reference
name_letter_guide <- 
  tibble(trial_label = my_names) %>% 
  mutate(trial_label = str_remove_all(trial_label, pattern = "[:digit:]")) %>% 
  distinct() %>% 
  mutate(let = LETTERS[1:n()],
         num = 1:n()) %>% 
  #--note doubles
  mutate(dbl_yn = case_when(
    trial_label == "Amundson" ~ "y",
    trial_label == "Bakehouse" ~ "y",
    trial_label == "McCaw" ~ "y",
    TRUE ~ "n"
  ))

name_letter_guide_no_doubles <- 
  name_letter_guide %>% 
  filter(dbl_yn == "n")


# non-double trials -------------------------------------------------------

i <- 1
for (i in 1:nrow(name_letter_guide_no_doubles)){

  ref.tmp <- 
    name_letter_guide_no_doubles %>% 
    slice(i)
  
  #--fig name
  tmp.figname <- paste0(ref.tmp$let, "1")
  
  #--yield/money
  tmp.ydat <- 
    y3 |> 
    filter(trial_label == ref.tmp$trial_label) 
  
  fig1 <- YieldFig(y.data = tmp.ydat)
  
 
  tmp.mdat <- 
    m %>% 
    filter(trial_label == ref.tmp$trial_label)
  
  fig3 <- MoneyFig(m.data = tmp.mdat)
  
  fig_res <- 
    fig1 + fig3 + plot_layout(widths = c(0.5, 0.5)) & 
    plot_annotation(theme = theme_border) 
  
  #--weather
  tmp.name <- ref.tmp$trial_label

  suppressMessages(
     wfig1 <- 
      TempFigInd(f.data = t, f.trial_label = tmp.name) +
      labs(title = "Temperature")
  )
  
  suppressMessages(
    wfig2 <- 
      CumPrecipFigInd(f.data = cp, f.trial_label = tmp.name) + 
      labs(title = "Precipitation")
    
  )
  
  
  fig_wea <- 
    wfig1 + wfig2  
  
  #--overall plot title
  tst.nlo <- 
    tmp.ydat %>% filter(grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tst.nhi <- 
    tmp.ydat %>% filter(!grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tmp.loc <- t %>% filter(trial_label == tmp.name) %>% pull(city) %>% unique() %>% str_to_title()
  
  tmp.state <- t %>% filter(trial_label == tmp.name) %>% pull(state) %>% unique() %>% str_to_upper()
  
  tmp.plot.title = paste0("Impact of reducing N from ", 
                          tst.nhi, 
                          " lb/ac to ",
                          tst.nlo, 
                          " lb/ac in ",
                          tmp.loc, " ", tmp.state, ", 2023")
  
  fig_wea / fig_res + 
    plot_annotation(theme = theme_border,
                    title = tmp.plot.title) &
    theme(plot.title = element_text(size = rel(1.4)),
          text = element_text(family = "Times New Roman"))
  
  ggsave(paste0("figs/ind-figs/", tmp.figname, "_", ref.tmp$trial_label, ".jpg"), 
         height = 7, width = 8)

  print(paste0("Wrote ", tmp.figname, "_", ref.tmp$trial_label))  
  
}


# two-trialed sheets ---------------------------------------------

#--yield, patchworked

name_letter_guide_doubles <- 
  name_letter_guide %>% 
  filter(dbl_yn == "y")

i <- 1

for (i in 1:nrow(name_letter_guide_doubles)){
  
  
  ref.tmp <- name_letter_guide_doubles %>% slice(i)
  
  n1.tmp <- paste0(ref.tmp$trial_label, "1")
  n2.tmp <- paste0(ref.tmp$trial_label, "2")
  
  v.yield1 <- y3 %>% filter(trial_label == n1.tmp)
  v.yield2 <- y3 %>% filter(trial_label == n2.tmp)
  
  yfig1 <- YieldFig(y.data = v.yield1) + facet_grid(.~trial_label) #+ labs(title = NULL)
  yfig2 <- YieldFig(y.data = v.yield2) + facet_grid(.~trial_label) #+ labs(title = NULL)
  
  yfig <- yfig1 / yfig2 #  + plot_annotation("Corn yield response")
  
  ##---money, patchworked
  
  v.money1 <- 
    m %>% filter(trial_label == n1.tmp)
  
  v.money2 <- 
    m %>% filter(trial_label == n2.tmp)
  
  mfig1 <- MoneyFig(m.data = v.money1) + facet_grid(.~trial_label) #+ labs(title = NULL)
  mfig2 <- MoneyFig(m.data = v.money2) + facet_grid(.~trial_label) #+ labs(title = NULL)
  
  mfig <- mfig1/mfig2 #+ plot_annotation("Financial outcome")
  
  vfig_res <- 
    yfig | mfig & # + plot_layout(widths = c(0.5, 0.5)) & 
    plot_layout(guides = "collect") &
    plot_annotation(theme = theme_border) 
  
  #--weather, only use one of the names
  v.name <- n1.tmp
  
  suppressMessages(
    vfig3 <- 
      TempFigInd(f.data = t, f.trial_label = v.name) +
      labs(title = "Temperature")
  )
    
  vfig4 <- 
    CumPrecipFigInd(f.data = cp, f.trial_label = v.name) + 
    labs(title = "Precipitation")
  
  vfig_wea <- 
    vfig3 + vfig4
  
  #--overall plot title - assumes the same reduction was used in both trials
  tst.nlo <- 
    v.yield1 %>% filter(grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tst.nhi <- 
    v.yield1 %>% filter(!grepl("Reduced", trt)) %>% pull(nrate_lbac) %>% 
    unique() %>% round()
  
  tmp.loc <- t %>% filter(trial_label == v.name) %>% pull(city) %>% unique() %>% str_to_title()
  
  tmp.plot.title = paste0("Impact of reducing N from ", 
                          tst.nhi, 
                          " lb/ac to ",
                          tst.nlo, 
                          " lb/ac in ",
                          tmp.loc, " IA, 2023")
  
  vfig_wea / vfig_res + 
    plot_layout(heights = c(0.3, 0.7)) +
    plot_annotation(theme = theme_border,
                    title = tmp.plot.title) &
    theme(plot.title = element_text(size = rel(1.4)),
          text = element_text(family = "Times New Roman"))
  
  ggsave(paste0("figs/ind-figs/", ref.tmp$let, "1_", ref.tmp$trial_label, ".jpg"), 
       height = 12, width = 8)

  print(paste0("Wrote ", ref.tmp$let, "_", ref.tmp$trial_label))  
  
}

  