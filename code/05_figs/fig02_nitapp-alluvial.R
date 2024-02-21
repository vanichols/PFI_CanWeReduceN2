#--note font is times new roman in theme and geom_text

library(tidyverse)
library(scales)
library(lubridate)
library(patchwork)
library(ggalluvial)

rm(list = ls())

source("code/05_figs/00_fig-colors.R")


# data --------------------------------------------------------------------

tk <- 
  read_csv("data_tidy/td_trialkey.csv") %>% 
  select(trial_key, trial_label, trial_label)

d <- 
  read_csv("data_tidy/td_napplications.csv") %>% 
  mutate(adjusted_n_type = paste("Red.", str_to_lower(adjusted_n_type)))

# theme -------------------------------------------------------------------

my_alluv_theme <- 
  theme_bw() +
  theme(
    # legend.position = c(0.2, 0.9),
    # legend.text = element_text(size = rel(1.2)),
    # legend.background = element_blank(),
    # axis.title.y = element_text(angle = 0,
    #                             vjust = 0.5),
    # axis.title = element_text(size = rel(1.1)),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = rel(1.5)),
    axis.ticks = element_blank(),
    plot.title = element_text(size = rel(1.5)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(hjust = 1),
    panel.border = element_blank(),
    plot.title.position = "plot",
    plot.caption.position =  "plot",
    plot.background = element_rect(fill = NA, colour = 'black', linewidth = 3),
    text = element_text(family = "Times New Roman")
  ) 





# alluvial plot -----------------------------------------------------------

d_all <- 
  d %>% 
  group_by(n_type, adjusted_n_type) %>% 
  summarise(freq = n()) %>% 
  ungroup()



ggplot(d_all,
       aes(y = freq,
           axis1 = n_type, axis2 = adjusted_n_type)) +
  geom_alluvium(aes(fill = n_type), 
                width = 1/4, knot.pos = 0, reverse = FALSE) +
  scale_fill_manual(values = c(pfi_yellow, pfi_orange, pfi_dkgreen, pfi_green,
                               pfi_blue, pfi_red)) +
  guides(fill = "none") +
  geom_stratum(alpha = 0.25, width = 1/4, reverse = FALSE, color = "black", size = 2) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  #scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_x_continuous(breaks = 1:2, labels = c("N timings utilized", "N reduction timing")) +
  #coord_flip() +
  labs(title = "Sidedress was the most popular timing for N reductions",
       y = NULL) + 
  my_alluv_theme



# try three axes ----------------------------------------------------------

#--this didn't work, trying to make all cooperator heights the same
# d_all2 <- 
#   d %>%  
#   group_by(trial_key) %>% 
#   mutate(freq = n()) %>% 
#   left_join(tk) %>% 
#   distinct() %>% 
#   mutate(rep = 12/freq) #%>% 
#   #uncount(rep)


d_all2 <- 
  d %>% 
  group_by(trial_key, n_type, adjusted_n_type) %>% 
  summarise(freq = n()) %>% 
  left_join(tk) %>% 
  distinct()


#--lowest common denom is 12
d_all2 %>% 
  pull(freq) %>% 
  unique()

#--61 total applications
d %>% 
  summarise(n = n())


ggplot(d_all2,
       aes(y = freq,
           axis1 = trial_label, axis2 = n_type, axis3 = adjusted_n_type)) +
  geom_alluvium(aes(fill = n_type),
                width = 1/2, knot.pos = 0, reverse = FALSE) +#, color = "black") +
  guides(fill = "none") +
  geom_stratum(alpha = .25, width = 1/2, reverse = FALSE, size = 2) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_fill_manual(values = c(pfi_yellow, pfi_orange, pfi_dkgreen, pfi_green,
                               pfi_blue, pfi_red)) +
  scale_x_continuous(breaks = 1:3, labels = c("Cooperator", "N timings", "Reduced N timings")) +
  labs(title = "Side-dress was the most popular timing for N reductions",
       y = NULL) + 
  my_alluv_theme

ggsave("figs/fig02_ntimings.jpg", width = 7, height = 10)

# ugh ---------------------------------------------------------------------

data("HairEyeColor")
ec <- as.data.frame(HairEyeColor)

ggplot(ugh,
       aes(y = freq,
           axis1 = trial_key, axis2 = n_type, axis3 = adjusted_n_type)) +
  geom_alluvium(aes(fill = n_type),
                width = 1/2, knot.pos = 0, reverse = FALSE) +#, color = "black") +
  guides(fill = "none") +
  geom_stratum(alpha = .25, width = 1/2, reverse = FALSE, size = 2) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_fill_manual(values = c(pfi_yellow, pfi_orange, pfi_dkgreen, pfi_green,
                               pfi_blue, pfi_red)) +
  scale_x_continuous(breaks = 1:3, labels = c("Cooperator", "N timings", "Reduced N timings")) +
  labs(title = "Side-dress was the most popular timing for N reductions",
       y = NULL) + 
  my_alluv_theme


#--transposed...


d_all2 <- 
  d %>% 
  pivot_longer(n_type:adjusted_n_type) %>% 
  rename("n_cat" = 2, "timing" = 3) %>% 
  mutate(n_cat = as.factor(n_cat),
         trial_key = as.factor(trial_key),
         trial_keyN = as.numeric(trial_key)) 

ggplot(d_all2,
       aes(x = n_cat, stratum = timing, alluvium = trial_keyN,
           fill = timing, label = timing)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("student curricula across several semesters") + 
  theme(strip.background = element_rect(colour="white", linewidth = 4))



data(vaccinations)
vaccinations <- transform(vaccinations,
                          response = factor(response, rev(levels(response))))
ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq,
           fill = response, label = response)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("vaccination survey responses at three points in time")

# example -----------------------------------------------------------------

data(majors)
majors$curriculum <- as.factor(majors$curriculum)


maj <- as_tibble(majors)


is_lodes_form(d_trans)


ggplot(majors,
       aes(x = semester, stratum = curriculum, alluvium = student,
           fill = curriculum, label = curriculum)) +
  scale_fill_brewer(type = "qual", palette = "Set2") +
  geom_flow(stat = "alluvium", lode.guidance = "frontback",
            color = "darkgray") +
  geom_stratum() +
  theme(legend.position = "bottom") +
  ggtitle("student curricula across several semesters")


titanic_alluvia <- as.data.frame(Titanic)
head(titanic_alluvia)
is_alluvia_form(titanic_alluvia,
                weight = "Freq")
# Titanic data in lodes format
titanic_lodes <- to_lodes_form(titanic_alluvia,
                               key = "x", value = "stratum", id = "alluvium",
                               axes = 1:4)

data(vaccinations)
vT <- transform(vaccinations,
                          response = factor(response, rev(levels(response))))
as_tibble(vaccinations)
