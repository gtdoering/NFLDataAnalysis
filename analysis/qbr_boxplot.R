library(nflfastR)
library(dplyr)
library(ggplot2)
source("functions/qbr.R")
options(scipen = 9999)


# QBR OVER TIME BOXPLOT -----------
qbr_all <- load_pbp(1999:2020)


qbr_all_filter <- qbr_all %>%
  # Selec the variables that ares used in calculcating QBR
  select(
    season, week, home_team, away_team, posteam, play_type, result, season_type,
    third_down_converted, third_down_failed, incomplete_pass, interception,
    pass_attempt, pass_touchdown, complete_pass, passer_player_name,
    passing_yards, pass, result, sack
  ) %>%
  # Adjust the result for home and away teams
  mutate(result = case_when(
    away_team == posteam ~ -result,
    home_team == posteam ~ result
  )) %>%
  # Only select the passing plays
  filter(play_type %in% c("pass"), season_type == "REG") %>%
  group_by(season, week, posteam) %>%
  # Calculate all the variables including QBR with a custom function
  summarise(
    pass_plays = n(),
    sacks = sum(sack, na.rm = TRUE),
    passes_attempted = sum(pass_attempt, na.rm = TRUE) - sacks,
    incompletions = sum(incomplete_pass, na.rm = TRUE),
    completions = passes_attempted - incompletions,
    pass_touchdowns = sum(pass_touchdown, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    third_down_converted = sum(third_down_converted, na.rm = TRUE),
    third_down_failed = sum(third_down_failed, na.rm = TRUE),
    third_down_conversion_rate =
      signif(
        third_down_converted / (third_down_failed + third_down_converted),
        3
      ),
    result = unique(result),
    ypa = passing_yards / passes_attempted,
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ .5
      ),
    qbr = qbr(
      completions = completions,
      passes_attempted = passes_attempted,
      passing_yards = passing_yards,
      interceptions = interceptions,
      pass_touchdowns = pass_touchdowns
    ),
    win = as.logical(win),
    season = as.factor(season)
  ) %>%
  filter(!is.na(win))

# Create boxplot for QBR over all 22 seasons
ggplot(qbr_all_filter, aes(x = season, y = qbr, fill = win)) +
  geom_boxplot(position = position_dodge(0.9)) +
  labs(
    title = "QBR For Winning and Losing",
    x = "Season",
    y = "QBR"
  ) +
  scale_fill_discrete(name = "Result", labels = c("Lose", "Win")) +
  theme_minimal() +
  geom_hline(
    yintercept = mean(qbr_all_filter$qbr),
    color = "black",
    linetype = "dashed",
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = mean(qbr_all_filter$qbr[qbr_all_filter$win == TRUE]),
    color = "#00BFC4",
    size = 1,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = mean(qbr_all_filter$qbr[qbr_all_filter$win == FALSE]),
    color = "#F8766D",
    size = 1,
    linetype = "dashed"
  )
