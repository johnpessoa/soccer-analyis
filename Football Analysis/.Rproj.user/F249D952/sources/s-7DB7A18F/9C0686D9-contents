library(tidyverse)
library(ggrepel)
library(devtools)
source("~/Desktop/token.R")
library(expappr)

#Below is all the data I will be using for this project. It includes data from
#FiveThirtyEight website, it's called "Club Soccer Preditions". 

spi_matches <- read.csv("~/Downloads/soccer-spi 2/spi_matches.csv")
spi_global_rankings <-  read.csv("~/Downloads/soccer-spi 2/spi_global_rankings.csv")
spi_global_rankings_int <- read.csv("~/Downloads/soccer-spi 2/spi_global_rankings_intl.csv")

head(spi_matches)

spi_matches %>% 
  mutate(season = lubridate::year(date)) %>% 
  View()

spi_matches %>% 
  mutate(goal_difference_1 = score1 - score2
         , goal_difference_2 = score2 - score1
         , points_1 = case_when(score1 > score2 ~ 3
                                ,score1 == score2 ~ 1
                                ,score1 < score2 ~ 0
                                ,TRUE ~ 0)) %>% 
  head()

spi_matches_2 <- spi_matches %>% 
  mutate(match_id = row_number())

spi_matches_3 <- spi_matches_2[, c(1, 2, 3, 23, 4:22)]

spi_matches_3 %>%
  filter(match_id == 1) %>% 
  gather(key, val, team1:adj_score2) %>% 
  mutate(team_num = str_remove_all(key, "[^0-9]"),
         key_adj = str_remove_all(key, "[0-9]")) %>% 
  filter(team_num == 1) %>% 
  select(- team_num, - key) %>% 
  spread(key_adj, val) %>% 
  head(n = 20)

home_teams <- spi_matches_3 %>% 
  select(-contains("2")) %>% 
  mutate(home = 1)

names(home_teams) <- names(home_teams) %>%  str_remove("[0-9]")


away_teams <- spi_matches_3 %>% 
  select(-contains("1")) %>% 
  mutate(home = 0)

names(away_teams) <- names(away_teams) %>% str_remove("[0-9]")

away_teams_for_join <- away_teams %>% 
  rename(opp = team
         ,opp_spi = spi
         ,opp_prob = prob
         ,opp_probtie = probtie
         ,opp_proj_score = proj_score
         ,opp_importance = importance
         ,opp_score = score
         ,opp_xg = xg
         ,opp_nsxg = nsxg
         ,opp_adj_score = adj_score
         ,opp_home = home)

home_teams_updated <- home_teams %>% 
  inner_join(away_teams_for_join, by = "match_id") %>% 
  select(- date.y, - league_id.y, - league.y) %>% 
  rename(date = date.x,
         league = league.x,
         league_id = league_id.x) %>% 
  select(match_id, league_id, league, date, team, home, opp, opp_home, spi, opp_spi, prob, opp_prob, probtie, 
         score, opp_score, proj_score, opp_proj_score, importance, opp_importance, xg, opp_xg, nsxg, opp_nsxg, adj_score, opp_adj_score) %>% 
  arrange(match_id)

home_teams_for_join <- home_teams %>% 
  rename(opp = team
         ,opp_spi = spi
         ,opp_prob = prob
         ,opp_probtie = probtie
         ,opp_proj_score = proj_score
         ,opp_importance = importance
         ,opp_score = score
         ,opp_xg = xg
         ,opp_nsxg = nsxg
         ,opp_adj_score = adj_score
         ,opp_home = home)

away_teams_updated <- away_teams %>% 
  inner_join(home_teams_for_join, by = "match_id") %>% 
  select(- date.y, - league_id.y, - league.y) %>% 
  rename(date = date.x,
         league = league.x,
         league_id = league_id.x) %>% 
  select(match_id, league_id, league, date, team, home, opp, opp_home, spi, opp_spi, prob, opp_prob, probtie, 
         score, opp_score, proj_score, opp_proj_score, importance, opp_importance, xg, opp_xg, nsxg, opp_nsxg, adj_score, opp_adj_score) %>% 
  arrange(match_id)

results_combined <- rbind(home_teams_updated, away_teams_updated) %>% arrange(match_id)

results_combined_2 <- results_combined %>% 
  mutate(result = case_when(score > opp_score ~ "win",
                            score == opp_score ~ "tie",
                            score < opp_score ~ "loss",
                            TRUE ~ "other")) %>% 
  mutate(points = case_when(result == "win" ~ 3,
                            result == "loss"~ 0,
                            result == "tie" ~ 1,
                            TRUE ~ 0)) %>% 
  mutate(goal_differential = score - opp_score)

class(results_combined_2$date)

results_combined_3 <- results_combined_2 %>% 
  mutate(date_actual = as.Date(date)) %>% 
  select(- date)
  #filter(date > "2016-08-01" & date < "2017-06-01") %>% 
  #filter(between(date, as.Date("2016-08-01"), as.Date("2017-06-01"))) %>% 

results_combined_3 <- results_combined_3[, c(1, 2, 3, 28, 4:27)]


results_combined_4 <-  results_combined_3 %>% 
  mutate(season = case_when(between(date_actual, as.Date("2016-06-01"), as.Date("2017-05-31")) ~ "2016-17",
                            between(date_actual, as.Date("2015-06-01"), as.Date("2016-05-31")) ~ "2015-16",
                            between(date_actual, as.Date("2017-06-01"), as.Date("2018-05-31")) ~ "2017-18",
                            between(date_actual, as.Date("2018-06-01"), as.Date("2019-06-09")) ~ "2018-19",
                            TRUE ~ "other"))

results_combined_5 <- results_combined_4[, c(1,2,3,29,4:28)]

results_combined_5 %>% filter(league == "Barclays Premier League") %>% filter(season == "2018-19") %>% View()

  mutate(goal_differential = ifelse(is.na(goal_differential), 0, goal_differential)) %>% 
  #filter(season == "2016-17") %>% 
  group_by(season, team) %>% 
  summarise(total_points = sum(points),
            total_goal_differential = sum(goal_differential)) %>% 
  arrange(season, desc(total_points)) %>%
  group_by(season) %>% 
  mutate(rank = row_number()) %>% 
  select(season, rank, team, total_points, total_goal_differential) %>% 
  View()

premier_league_results <- results_combined_5 %>% 
  filter(league == "Barclays Premier League")

premier_league_table <- premier_league_results %>% 
  mutate(goal_differential = ifelse(is.na(goal_differential), 0, goal_differential)) %>% 
  #filter(season == "2016-17") %>% 
  group_by(season, team) %>% 
  summarise(total_points = sum(points),
            total_goal_differential = sum(goal_differential)) %>% 
  arrange(season, desc(total_points)) %>%
  group_by(season) %>% 
  mutate(rank = row_number()) %>% 
  select(season, rank, team, total_points, total_goal_differential)

premier_league_results %>% 
  filter(team == "Arsenal", date_actual ) %>% 
  ggplot(aes(date_actual, spi)) +
  geom_point()

results_combined_5 %>% 
  filter(league != "UEFA Champions League") %>% 
  #distinct(league) %>% 
  ggplot(aes(date_actual, spi)) +
  geom_jitter()

grey_results <- results_combined_5[, -6] %>% filter(date_actual < "2018-09-20")

results_combined_5 %>% 
  filter(league == "Barclays Premier League", date_actual < "2018-09-20") %>% 
  ggplot(aes(date_actual, spi, color = league)) + 
  geom_jitter(data = grey_results, colour = "grey") +
  geom_jitter(alpha = .8, size = 3, color = "cyan4") +
  theme_expapp() +
  scale_color_discrete(guide = FALSE) +
  xlab("Date") +
  ylab("SPI") +
  ggtitle("Barclays Premier League Comparison","SPI of Premier League teams compared to the rest")
  
