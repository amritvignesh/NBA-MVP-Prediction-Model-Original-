library(tidyverse)
library(nbastatR)
library(nbaTools)
library(ggimage)
library(gt)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(png)
library(paletteer)

basic_stats <- read.csv("basic_stats.csv")
advanced_stats <- read.csv("advanced_stats.csv")
basicadvanced_stats <- inner_join(advanced_stats, basic_stats, by=c('season'='yearSeason', 'player'='namePlayer')) # combining basic stats and advanced stats

basicadvanced_stats <- basicadvanced_stats %>%
  mutate(mvp = mvp.y) %>%
  group_by(season) %>%
  select(player, team, season, ppg, apg, rpg, spg, bpg, win, per, ws48, bpm, vorp, mvp) %>%
  ungroup()

basicadvanced_stats_train <- basicadvanced_stats %>%
  filter(season != 2023) # train data from 2000 - 2022

basicadvanced_stats_test <- basicadvanced_stats %>%
  filter(season == 2023) # test data for 2023

basicadvanced_reg <- glm(mvp ~ ppg + apg + rpg + spg + bpg + win + per + ws48 + bpm + vorp, data = basicadvanced_stats_train, family = binomial)
summary(basicadvanced_reg)

basicadvanced_stats_train <- basicadvanced_stats_train %>%
  mutate(prediction = predict(basicadvanced_reg, basicadvanced_stats_train, type = "response")) %>%  
  group_by(season) %>%
  mutate(mvpprob = prediction / sum(prediction)) %>%
  ungroup()

basicadvanced_stats_test <- basicadvanced_stats_test %>%
  mutate(prediction = predict(basicadvanced_reg, basicadvanced_stats_test, type = "response")) %>%
  mutate(mvpprob = prediction / sum(prediction)) %>%
  ungroup()

mvp_probability_train <- basicadvanced_stats_train %>%
  mutate(mvp_prob = round(mvpprob, 3), mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  select(player, team, season, mvp_prob, mvp_won) %>%
  arrange(season, -mvp_prob) %>%
  filter(mvp_prob != 0.000) 

mvp_probability_test <- basicadvanced_stats_test %>%
  mutate(mvp_prob = round(mvpprob, 3), mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  select(player, team, season, mvp_prob, mvp_won) %>%
  arrange(-mvp_prob) %>%
  filter(mvp_prob != 0.000) 

t2000 = mvp_probability_train %>% filter(season == 2000) %>% filter(row_number() <= 6)
t2001 = mvp_probability_train %>% filter(season == 2001) %>% filter(row_number() <= 6)
t2002 = mvp_probability_train %>% filter(season == 2002) %>% filter(row_number() <= 6)
t2003 = mvp_probability_train %>% filter(season == 2003) %>% filter(row_number() <= 6)
t2004 = mvp_probability_train %>% filter(season == 2004) %>% filter(row_number() <= 6)
t2005 = mvp_probability_train %>% filter(season == 2005) %>% filter(row_number() <= 6)
t2006 = mvp_probability_train %>% filter(season == 2006) %>% filter(row_number() <= 6)
t2007 = mvp_probability_train %>% filter(season == 2007) %>% filter(row_number() <= 6)
t2008 = mvp_probability_train %>% filter(season == 2008) %>% filter(row_number() <= 6)
t2009 = mvp_probability_train %>% filter(season == 2009) %>% filter(row_number() <= 6)
t2010 = mvp_probability_train %>% filter(season == 2010) %>% filter(row_number() <= 6)
t2011 = mvp_probability_train %>% filter(season == 2011) %>% filter(row_number() <= 6)
t2012 = mvp_probability_train %>% filter(season == 2012) %>% filter(row_number() <= 6)
t2013 = mvp_probability_train %>% filter(season == 2013) %>% filter(row_number() <= 6)
t2014 = mvp_probability_train %>% filter(season == 2014) %>% filter(row_number() <= 6)
t2015 = mvp_probability_train %>% filter(season == 2015) %>% filter(row_number() <= 6)
t2016 = mvp_probability_train %>% filter(season == 2016) %>% filter(row_number() <= 6)
t2017 = mvp_probability_train %>% filter(season == 2017) %>% filter(row_number() <= 6)
t2018 = mvp_probability_train %>% filter(season == 2018) %>% filter(row_number() <= 6)
t2019 = mvp_probability_train %>% filter(season == 2019) %>% filter(row_number() <= 6)
t2020 = mvp_probability_train %>% filter(season == 2020) %>% filter(row_number() <= 6)
t2021 = mvp_probability_train %>% filter(season == 2021) %>% filter(row_number() <= 6)
t2022 = mvp_probability_train %>% filter(season == 2022) %>% filter(row_number() <= 6)
t2023 = mvp_probability_test %>% filter(season == 2023) %>% filter(row_number() <= 6)


t2023 %>% gt() %>% # change the t value to the corresponding table above to output gt table and for 2023, add the title and subtitle
  cols_align(
    align = "center",
    columns = c(player, team, season, mvp_prob, mvp_won)
  ) %>%
  data_color(
    columns = mvp_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    player = md("**Player**"),
    team = md("**Team**"),
    season = md("**Season**"),
    mvp_prob = md("**MVP Probability**"),
    mvp_won = md("**MVP Result**")
  ) %>%
  tab_header(
    title = md("**2023 NBA MVP Probability**"),
    subtitle = "Based on NBA MVP Data from 2000 - 2022 Involving Basic And Advanced Statistics"
  )
