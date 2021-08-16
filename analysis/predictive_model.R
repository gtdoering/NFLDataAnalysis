library(nflfastR)
library(zoo)
library(dplyr)
library(faraway)
library(gains)
source("functions/qbr.R")
options(scipen = 9999)


# PREDICTIVE MODEL ------------------

# Train Dataset ---------------------
model_fav <- load_pbp(2017:2019)

# Filter function for the train data
train <- model_fav %>%
  select(
    home_team, away_team, season, week, div_game, result, spread_line,
    passing_yards, pass_attempt, pass_touchdown, incomplete_pass, posteam,
    interception, sack
  ) %>%
  filter(
    week %in% c(1:17),
    !is.na(posteam),
    season %in% c(2017, 2018)
  ) %>%
  group_by(season, posteam, week) %>%
  summarise(
    home_team = unique(home_team),
    away_team = unique(away_team),
    div_game = unique(div_game),
    result = unique(result),
    spread = unique(spread_line),
    sacks = sum(sack, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    incompletions = sum(incomplete_pass, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passes_attempted = sum(pass_attempt, na.rm = TRUE) - sacks,
    pass_touchdowns = sum(pass_touchdown, na.rm = TRUE),
    completions = passes_attempted - incompletions
  ) %>%
  mutate(
    fav_team = case_when(
      spread < 0 ~ away_team,
      spread >= 0 ~ home_team
    ),
    spread = case_when(
      fav_team == away_team ~ -spread,
      fav_team == home_team ~ spread
    ),
    fav_win = case_when(
      fav_team == away_team & result <= 0 ~ 1,
      fav_team == away_team & result > 0 ~ 0,
      fav_team == home_team & result >= 0 ~ 1,
      fav_team == home_team & result < 0 ~ 0
    ),
    win = case_when(
      fav_team == posteam ~ fav_win,
      fav_team != posteam & fav_win == 0 ~ 1,
      fav_team != posteam & fav_win == 1 ~ 0
    ),
    cum_pass_yards = lag(cumsum(passing_yards), k = 1, default = 0),
    cum_pass_attempts = lag(cumsum(passes_attempted), k = 1, default = 0),
    qbr = qbr(
      completions = completions,
      passing_yards = passing_yards,
      pass_touchdowns = pass_touchdowns,
      interceptions = interceptions,
      passes_attempted = passes_attempted
    )
  ) %>%
  ungroup(season) %>%
  mutate(
    previous = season - 1,
    winning_form_5 = lag(
      rollsum(win, 5, align = "right", all.x = TRUE, fill = NA),
      k = 1
    ),
    winning_form_10 = lag(
      rollsum(win, 10, align = "right", all.x = TRUE, fill = NA),
      k = 1
    ),
    ypa = case_when(
      week %in% c(2:17) ~ cum_pass_yards / cum_pass_attempts,
      week == 1 ~ 
        mean(cum_pass_yards[season %in% previous]) /
        mean(cum_pass_attempts[season %in% previous])
    ),
    avg_pass_yards = case_when(
      week %in% c(2:17) ~ cum_pass_yards / (as.numeric(week) - 1),
      week == 1 ~ max(cum_pass_yards[season %in% previous]) / 16
    ),
    avg_pass_attempts = case_when(
      week %in% c(2:17) ~ cum_pass_attempts / (as.numeric(week) - 1),
      week == 1 ~ max(cum_pass_attempts[season %in% previous] / 16)
    ),
    qbr_form_5 = case_when(
      week %in% c(2:17) ~ lag(
        rollsum(qbr, 5, align = "right", all.x = TRUE, fill = NA),
        k = 1
      ) / 5,
      week == 1 ~ mean(qbr[season %in% previous])
    )
  ) %>%
  filter(!(season == 2017 & week %in% c(1:11))) %>%
  select(
    season, week, fav_team, posteam, home_team, away_team, result, spread,
    fav_win, win, everything()
  ) %>%
  select(-previous) %>%
  arrange(season, week)

train <- model_fav %>%
mtrain_under <- train %>%
  filter(fav_team != posteam)

train <- train %>%
  filter(fav_team == posteam)

train$under_form_5 <- train_under$winning_form_5
train$under_form_10 <- train_under$winning_form_10
train$under_qbr_form_5 <- train_under$qbr_form_5


# Test Dataset --------

log.model <- glm(fav_win ~ spread + qbr_form_5 + under_qbr_form_5 + div_game,
  family = binomial, data = train
)

summary(log.model)

test <- model_fav %>%
  select(
    home_team, away_team, season, week, div_game, result, spread_line,
    passing_yards, pass_attempt, pass_touchdown, incomplete_pass, posteam,
    interception, sack
  ) %>%
  filter(week %in% c(1:17), !is.na(posteam)) %>%
  group_by(season, posteam, week) %>%
  summarise(
    home_team = unique(home_team),
    away_team = unique(away_team),
    div_game = unique(div_game),
    result = unique(result),
    spread = unique(spread_line),
    sacks = sum(sack, na.rm = TRUE),
    interceptions = sum(interception, na.rm = TRUE),
    incompletions = sum(incomplete_pass, na.rm = TRUE),
    passing_yards = sum(passing_yards, na.rm = TRUE),
    passes_attempted = sum(pass_attempt, na.rm = TRUE) - sacks,
    pass_touchdowns = sum(pass_touchdown, na.rm = TRUE),
    completions = passes_attempted - incompletions
  ) %>%
  mutate(
    fav_team = case_when(
      spread < 0 ~ away_team,
      spread >= 0 ~ home_team
    ),
    spread = case_when(
      fav_team == away_team ~ -spread,
      fav_team == home_team ~ spread
    ),
    fav_win = case_when(
      fav_team == away_team & result <= 0 ~ 1,
      fav_team == away_team & result > 0 ~ 0,
      fav_team == home_team & result >= 0 ~ 1,
      fav_team == home_team & result < 0 ~ 0
    ),
    win = case_when(
      fav_team == posteam ~ fav_win,
      fav_team != posteam & fav_win == 0 ~ 1,
      fav_team != posteam & fav_win == 1 ~ 0
    ),
    cum_pass_yards = lag(cumsum(passing_yards), k = 1, default = 0),
    avg_pass_yards = cum_pass_yards / (as.numeric(week) - 1),
    cum_pass_attempts = lag(cumsum(passes_attempted), k = 1, default = 0),
    avg_pass_attempts = cum_pass_attempts / (as.numeric(week) - 1),
    qbr = qbr(
      completions = completions,
      passing_yards = passing_yards,
      pass_touchdowns = pass_touchdowns,
      interceptions = interceptions,
      passes_attempted = passes_attempted
    )
  ) %>%
  ungroup(season) %>%
  mutate(
    previous = season - 1,
    winning_form_5 = lag(
      rollsum(win, 
              5, 
              align = "right", 
              all.x = TRUE, 
              fill = NA), 
      k = 1
    ),
    winning_form_10 = lag(
      rollsum(
        x = win, 
        k = 10, 
        align = "right", 
        all.x = TRUE, 
        fill = NA), 
      k = 1
    ),
    ypa = case_when(
      week %in% c(2:17) ~ cum_pass_yards / cum_pass_attempts,
      week == 1 ~ mean(cum_pass_yards[season %in% previous]) /
        mean(cum_pass_attempts[season %in% previous])
    ),
    avg_pass_yards = case_when(
      week %in% c(2:17) ~ cum_pass_yards / (as.numeric(week) - 1),
      week == 1 ~ max(cum_pass_yards[season %in% previous]) / 16
    ),
    avg_pass_attempts = case_when(
      week %in% c(2:17) ~ cum_pass_attempts / (as.numeric(week) - 1),
      week == 1 ~ max(cum_pass_attempts[season %in% previous] / 16)
    ),
    qbr_form_5 = case_when(
      week %in% c(2:17) ~ lag(
        rollmean(
          x = qbr, 
          k = 5, 
          align = "right", 
          all.x = TRUE, 
          fill = NA), 
        k = 1),
      week == 1 ~ mean(qbr[season %in% previous])
    )
  ) %>%
  filter(season == 2019) %>%
  select(
    season, week, fav_team, posteam, home_team, away_team, result, spread,
    fav_win, win, everything()
  ) %>%
  select(-previous) %>%
  arrange(season, week)

test_under <- test %>%
  filter(fav_team != posteam)


test <- test %>%
  filter(fav_team == posteam)

test$under_form <- test_under$winning_form_10
test$under_qbr_form_5 <- test_under$qbr_form_5



# Logistic Model and Performance ---------
test$logprobs <- predict(log.model, test, type = "response")

test <- test %>%
  ungroup() %>%
  mutate(
    spread_performance = cumsum(fav_win) / as.numeric(row.names(test)),
    mod_pred = case_when(
      logprobs > .5 ~ 1,
      logprobs <= .5 ~ 0
    ),
    model_performance = 
      cumsum(mod_pred == fav_win) / as.numeric(row.names(test))
  )

test <- test %>%
  arrange(week, desc(logprobs))

# Gains Plot ---------
test$fav_win <- as.numeric(test$fav_win)
log.gains <- gains(test$fav_win, test$logprobs, groups = 6)
log.gains
plot(log.gains, ylim = c(0, 1))

# Lift Plot ----------
plot(
  log.gains$depth, 
  log.gains$lift, 
  col = "red", 
  xlab = "Depth", 
  ylab = "Lift", 
  xlim = c(0, 105), 
  ylim = c(0, 180)
)
lines(log.gains$depth, log.gains$lift, col = "red")
points(log.gains$depth, log.gains$cume.lift, col = "blue")
lines(log.gains$depth, log.gains$cume.lift, col = "blue")
title("Logisitic Lift Plot")
abline(h = 100)
