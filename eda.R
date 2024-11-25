library(nflfastR)
library(tidyverse)

FIRST_SEASON = 2020
LAST_SEASON = 2024

POINTS_REC = 1
POINTS_YARD = 0.1
POINTS_TD = 6

POSITIONS = c("RB", "WR", "TE")

stats <- nflfastR::load_player_stats(seasons = FIRST_SEASON:LAST_SEASON, stat_type = "offense")

clean_stats <- stats %>%
  filter(position_group %in% POSITIONS) %>%
  select(id = player_id, player_name, position_group, season, week, tgt_share = target_share, targets, catches = receptions, yards = receiving_yards, tds = receiving_tds) %>%
  mutate(fpts = POINTS_REC * catches + POINTS_YARD * yards + POINTS_TD * tds) %>%
  arrange(season, week) %>%
  mutate(tgt_share = ifelse(is.na(tgt_share) & targets == 0, 0, tgt_share))

player_season <- clean_stats %>%
  group_by(id, player_name, position_group, season) %>%
  summarise(
    games = n(),
    avg_share = mean(tgt_share),
    targets = mean(targets),
    fpts = mean(fpts),
    .groups = "drop"
    ) %>%
  arrange(-fpts) %>%
  filter(games >= 5)

ggplot(player_season, aes(x = targets)) + 
  geom_histogram(binwidth = 1, color = "black", fill = 'lightblue') + 
  theme_classic() +
  xlim(0, 15) +
  scale_x_continuous(breaks = seq(0, 15))

ggplot(player_season, aes(x = targets, y = fpts)) +
  geom_point(alpha = 0.25) +
  theme_classic() +
  geom_smooth(method = 'lm', fill = 'blue')

target_model <- lm(fpts ~ 0 + position_group * targets, data = player_season)

player_season_preds <- player_season %>%
  mutate(xfpts = predict(target_model, player_season)) %>%
  mutate(resid = fpts - xfpts)

residual_analysis <- player_season_preds %>%
  arrange(season) %>%
  group_by(id, player_name) %>%
  mutate(prev_xfpts = lag(xfpts),
         prev_fpts = lag(fpts), 
         prev_resid = lag(resid)) 

ggplot(residual_analysis, aes(x = prev_xfpts, y = fpts)) +
  geom_point()

cor(residual_analysis$xfpts, residual_analysis$fpts)

cor(residual_analysis$prev_xfpts, residual_analysis$fpts, use = 'complete.obs')
cor(residual_analysis$prev_xfpts, residual_analysis$xfpts, use = 'complete.obs')
cor(residual_analysis$prev_fpts, residual_analysis$fpts, use = 'complete.obs')

ggplot(residual_analysis, aes(x = prev_resid, y = resid)) +
  geom_point() +
  geom_smooth(method = 'lm')
