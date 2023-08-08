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
pbp <- game_logs(seasons = c(2000:2023))
pbp %>% head()

basic_stats <- pbp %>%
  filter(minutes >= 20) %>%
  mutate(outcome = ifelse(outcomeGame == "W", 1, 0)) %>%
  group_by(yearSeason, namePlayer, nameTeam) %>%
  filter(n() >= 41) %>%
  summarize(games = n(), ppg = mean(pts), apg = mean(ast), rpg = mean(treb), spg = mean(stl), bpg = mean(blk), win = sum(outcome))

basic_stats <- basic_stats %>%
  group_by(yearSeason) %>%
  mutate(ppg_rank = order(order(ppg, yearSeason, decreasing=TRUE)), apg_rank = order(order(apg, yearSeason, decreasing=TRUE)), rpg_rank = order(order(rpg, yearSeason, decreasing=TRUE)), spg_rank = order(order(spg, yearSeason, decreasing=TRUE)), bpg_rank = order(order(bpg, yearSeason, decreasing=TRUE)), win_rank = order(order(win, yearSeason, decreasing=TRUE)))

write_csv(basic_stats, "basic_stats.csv")

basic_stats <- read_csv("basic_stats.csv")

basic_stats_train <- basic_stats %>%
  filter(yearSeason != 2023)

basic_stats_test <- basic_stats %>%
  filter(yearSeason == 2023)

basic_reg <- glm(mvp ~ ppg + apg + rpg + spg + bpg + win, data = basic_stats_train, family = binomial)
summary(basic_reg)

basic_stats_train <- basic_stats_train %>%
  mutate(prediction = predict(basic_reg, basic_stats_train, type = "response")) %>%
  group_by(yearSeason) %>%
  mutate(mvpprob = prediction / sum(prediction)) %>%
  ungroup()

basic_stats_test <- basic_stats_test %>%
  mutate(prediction = predict(basic_reg, basic_stats_test, type = "response")) %>%
  mutate(mvpprob = prediction / sum(prediction)) %>%
  ungroup()

mvp_probability_train <- basic_stats_train %>%
  mutate(player = namePlayer, team = nameTeam, season = yearSeason, mvp_prob = round(mvpprob, 3), mvp_won = ifelse(mvp == 1, "WON", "")) %>%
  select(player, team, season, mvp_prob, mvp_won) %>%
  arrange(season, -mvp_prob) %>%
  filter(mvp_prob != 0.000) 

mvp_probability_test <- basic_stats_test %>%
  mutate(player = namePlayer, team = nameTeam, season = yearSeason, mvp_prob = round(mvpprob, 3), mvp_won = ifelse(mvp == 1, "WON", "")) %>%
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


t2015 %>% gt() %>%
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


