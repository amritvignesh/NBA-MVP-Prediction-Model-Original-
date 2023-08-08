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

Sys.setenv("VROOM_CONNECTION_SIZE"= 131072 * 10)
bref_players_stats(seasons = c(2000:2023), tables = "advanced", widen = TRUE, assign_to_environment = TRUE)

advanced_stats <- dataBREFPlayerAdvanced %>%
  mutate(player = namePlayer, season = yearSeason + 1) %>%
  filter(minutes >= 820) %>%
  group_by(season, player) %>%
  summarize(team = slugTeamsBREF, per = ratioPER, ws48 = ratioWSPer48, bpm = ratioBPM, vorp = ratioVORP)

advanced_stats <- advanced_stats %>%
  group_by(season) %>%
  mutate(per_rank = order(order(per, season, decreasing=TRUE)), ws48_rank = order(order(ws48, season, decreasing=TRUE)), bpm_rank = order(order(bpm, season, decreasing=TRUE)), vorp_rank = order(order(vorp, season, decreasing=TRUE)))

write_csv(advanced_stats, "advanced_stats.csv")

advanced_stats <- read_csv("advanced_stats.csv")

advanced_stats_train <- advanced_stats %>%
  filter(season != 2023)

advanced_stats_test <- advanced_stats %>%
  filter(season == 2023)

advanced_reg <- glm(mvp ~ per + ws48 + bpm + vorp, data = advanced_stats_train, family = binomial)
summary(advanced_reg)

advanced_stats_train <- advanced_stats_train %>%
  mutate(prediction = predict(advanced_reg, advanced_stats_train, type = "response")) %>%
  group_by(season) %>%
  mutate(mvpprob = prediction / sum(prediction)) %>%
  ungroup()

advanced_stats_test <- advanced_stats_test %>%
  mutate(prediction = predict(advanced_reg, advanced_stats_test, type = "response")) %>%
  mutate(mvpprob = prediction / sum(prediction)) %>%
  ungroup()

mvp_probability_train <- advanced_stats_train %>%
  mutate(mvp_prob = round(mvpprob, 3), mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  select(player, team, season, mvp_prob, mvp_won) %>%
  arrange(season, -mvp_prob) %>%
  filter(mvp_prob != 0.000) 

mvp_probability_test <- advanced_stats_test %>%
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


t2001 %>% gt() %>%
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
  ) 
