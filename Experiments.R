install.packages("nflfastR")

library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)

options(scipen = 9999)
data <- load_pbp(2019)
View(data)

data %>% 
  filter(posteam == 'JAX' & pass == 1) %>%
  select(posteam, desc, rush, pass, name, passer, rusher, receiver) %>% 
  head(10)

down4 <- data %>% 
  filter(down == 4 & special == 0) %>%
  filter(fourth_down_converted != 0 | fourth_down_failed != 0) %>% 
  select(down, ydstogo, desc, fourth_down_converted, fourth_down_failed) 
head(down4)
down4$fourth_down_converted <- as.logical(down4$fourth_down_converted)
ggplot(Down4, aes(x=fourth_down_converted, y=ydstogo, group = fourth_down_converted,
                  fill = fourth_down_converted)) + 
  geom_boxplot()



pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa))
pbp_rp %>%
  filter(posteam == "DAL", rush == 1) %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), ypc = mean(yards_gained), plays = n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20)

pbp <- load_pbp(2016:2020)
pbp %>%
  group_by(season) %>%
  summarize(n = n())

pbp %>%
  group_by(play_type) %>%
  summarize(n = n())

qbs <- pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  group_by(id, name) %>%
  summarize(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    team = last(posteam)
  ) %>%
  ungroup() %>%
  filter(n_dropbacks > 100 & n_plays > 1000)
qbs

qbs <- qbs %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept =  mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = qbs$team_color, cex=qbs$n_plays / 350, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2015 - 2019",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


###### EXPERIMENTS #####
pbp20 <- load_pbp(2020)
#names(pbp20) 

pbp20home <- pbp20 %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team) %>% 
  distinct(week, team, .keep_all = TRUE)

pbp20away <- pbp20 %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>% 
  mutate(result = -result) %>% 
  distinct(week, team,.keep_all = TRUE)

pbp20wins <- rbind(pbp20away,pbp20home) %>% 
  mutate(win = case_when(
    result > 0  ~ 1,
    result < 0  ~ 0,
    result == 0 ~ .5
  )) %>% 
  group_by(team) %>% 
  summarise(wins = sum(win))
  

stats20 <- pbp20 %>% 
  filter(!is.na(posteam)) %>% 
  group_by(posteam) %>% 
  rename(team = posteam) %>% 
  summarise(passing_yards = sum(passing_yards, na.rm = TRUE),
            rushing_yards = sum(rushing_yards, na.rm = TRUE)) 

offense <- left_join(pbp20wins, stats20 )
offense <- offense %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

offense %>%
  ggplot(aes(x = rushing_yards, y = passing_yards)) +
  #horizontal line with mean passing yards
  geom_hline(yintercept = mean(offense$passing_yards), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean rushing yards
  geom_vline(xintercept =  mean(offense$rushing_yards), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the teams with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = offense$team_color, cex= offense$wins, alpha = .6)+
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=team)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Offensive Rushing Yards for 2020",
       y = "Offensive Passing Yards for 2020",
       title = "Offense Impact on Wins for 2020",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))


####### STATS AND WINS TOTAL #######
pbp <- load_pbp(1999:2020)
#names(pbp20) 

pbphome <- pbp %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team) %>% 
  distinct(week, team, season, .keep_all = TRUE)

pbpaway <- pbp %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>% 
  mutate(result = -result) %>% 
  distinct(week, team, season, .keep_all = TRUE)

pbpwins <- rbind(pbpaway,pbphome) %>% 
  mutate(win = case_when(
    result > 0  ~ 1,
    result < 0  ~ 0,
    result == 0 ~ .5
  )) %>% 
  group_by(team, season) %>% 
  summarise(wins = sum(win))


stats <- pbp %>% 
  filter(!is.na(posteam)) %>% 
  group_by(posteam, season) %>% 
  rename(team = posteam) %>% 
  summarise(passing_yards = sum(passing_yards, na.rm = TRUE),
            rushing_yards = sum(rushing_yards, na.rm = TRUE)) 

offense <- left_join(pbpwins, stats )
offense <- offense %>%
  left_join(teams_colors_logos, by = c('team' = 'team_abbr'))

jaxoffense <- offense %>% 
  filter(team == 'JAX') %>% 
  mutate(misery = misery(wins, rushing_yards + passing_yards))


jaxoffense %>%
  ggplot(aes(x = rushing_yards, y = passing_yards)) +
  #horizontal line with mean passing yards
  geom_hline(yintercept = mean(jaxoffense$passing_yards), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean rushing yards
  geom_vline(xintercept =  mean(jaxoffense$rushing_yards), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the teams with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = "#008080",cex= jaxoffense$wins, alpha = .6)+
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=paste0(season," Misery: ",misery))) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Offensive Rushing Yards",
       y = "Offensive Passing Yards",
       title = "JAX Offense Impact on Wins 1999:2020",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

misery <- function(wins, total_yards){
  misery <- vector()
  for (i in 1:length(wins)) {
    
    if(total_yards[i] > 6653){
      misery[i] = 0
    }else{
      misery[i] = 3 - (total_yards[i]-4400)/2653 * 3
    }
    
    if(wins[i] > 16){
      misery[i] = misery[i] 
    }else{
      misery[i] = misery[i] + (7 - (wins[i] / 16)*7)
    }
  }

  signif(misery, 3)
}

misery(wins = jaxoffense$wins, total_yards = jaxoffense$passing_yards + jaxoffense$rushing_yards)

####### GAME BY GAME QBR AND WIN CHANCE ########
qbr_2020 <- load_pbp(2020)

qbr_2020_JAX <- qbr_2020 %>%
  select(season, week, home_team, away_team, posteam, play_type, result, season_type, third_down_converted, third_down_failed,
                       incomplete_pass, interception, pass_attempt, pass_touchdown, complete_pass,passer_player_name,
                       passing_yards, pass, result, sack
                       ) %>%
  mutate(result = case_when(
    away_team == "JAX"  ~ -result,
    home_team == "JAX"  ~ result)
    ) %>%
  filter(home_team == "JAX" | away_team == "JAX", posteam == "JAX",
                       play_type %in% c("pass"), season_type == "REG") %>%
  group_by(season, week, posteam, passer_player_name) %>%
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
    third_down_conversion_rate = signif(third_down_converted/(third_down_failed + third_down_converted), 3),
    result = unique(result)
    ) %>%
  mutate(win = case_when(
    result > 0  ~ 1,
    result < 0  ~ 0,
    result == 0 ~ .5
    )) %>%
  mutate(a = ((completions/passes_attempted - .3) * 5),
         b = ((passing_yards/passes_attempted - 3) * .25),
         c = ((pass_touchdowns/passes_attempted) * 20),
         d = (2.375 - (interceptions/passes_attempted * 25))
                       ) %>%
  mutate(
    a = case_when(
    a >= 2.375 ~ 2.375,
    a >= 0 ~ a,
    a < 0 ~ 0
    ), b = case_when(
    b >= 2.375 ~ 2.375,
    b >= 0 ~ b,
    b < 0 ~ 0
    ), c = case_when(
    c >= 2.375 ~ 2.375,
    c >= 0 ~ c,
    c < 0 ~ 0
    ), d = case_when(
    d >= 2.375 ~ 2.375,
    d >= 0 ~ d,
    d < 0 ~ 0
    ),
    qbr = signif((a + b + c +d)/6 * 100,4),
    win = as_factor(win)
    ) %>%
  select(-c(a,b,c,d))

CombinedPlot=ggplot(qbr_2020_JAX, aes(x=win, y=qbr, fill=win)) + geom_boxplot()
CombinedPlot

############# QBR OVER TIME ################
qbr_all <- load_pbp(1999:2020)

qbr_all_filter <- qbr_all %>%
  select(season, week, home_team, away_team, posteam, play_type, result, season_type, third_down_converted, third_down_failed,
         incomplete_pass, interception, pass_attempt, pass_touchdown, complete_pass,passer_player_name,
         passing_yards, pass, result, sack
         ) %>%
  mutate(result = case_when(
    away_team == posteam  ~ -result,
    home_team == posteam  ~ result)
    ) %>%
  filter(play_type %in% c("pass"), season_type == "REG") %>%
  group_by(season, week, posteam) %>%
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
    third_down_conversion_rate = signif(third_down_converted/(third_down_failed + third_down_converted), 3),
    result = unique(result),
    ypa = passing_yards/passes_attempted
    ) %>%
  mutate(win = case_when(
    result > 0  ~ 1,
    result < 0  ~ 0,
    result == 0 ~ .5
    )) %>%
  mutate(a = ((completions/passes_attempted - .3) * 5),
         b = ((passing_yards/passes_attempted - 3) * .25),
         c = ((pass_touchdowns/passes_attempted) * 20),
         d = (2.375 - (interceptions/passes_attempted * 25))
         ) %>%
  mutate(
    a = case_when(
    a >= 2.375 ~ 2.375,
    a >= 0 ~ a,
    a < 0 ~ 0
    ), b = case_when(
    b >= 2.375 ~ 2.375,
    b >= 0 ~ b,
    b < 0 ~ 0
    ), c = case_when(
    c >= 2.375 ~ 2.375,
    c >= 0 ~ c,
    c < 0 ~ 0
    ), d = case_when(
    d >= 2.375 ~ 2.375,
    d >= 0 ~ d,
    d < 0 ~ 0
    ),
    qbr = signif((a + b + c +d)/6 * 100,4),
    win = as.logical(win),
    season = as.factor(season)
    ) %>%
  select(-c(a,b,c,d)) %>%
  filter(!is.na(win))

ggplot(qbr_all_filter, aes(x=season, y= qbr, fill=win)) +
  geom_boxplot(position = position_dodge(0.9) ) +
  labs(title = "QBR For Winning and Losing",
       x = "Season",
       y = "QBR") +
  scale_fill_discrete(name = "Result", labels = c("Lose", "Win")) +
  theme_minimal() +
  geom_hline(yintercept= mean(qbr_all_filter$qbr),color = "black", linetype = "dashed", alpha=0.5) +
  geom_hline(yintercept = mean(qbr_all_filter$qbr[qbr_all_filter$win == TRUE]), color = "#00BFC4", size = 1, linetype = "dashed") +
  geom_hline(yintercept = mean(qbr_all_filter$qbr[qbr_all_filter$win == FALSE]), color = "#F8766D", size = 1, linetype = "dashed")


######### PREDICTIVE MODEL ###########

########### TRAIN #################
library(zoo)
model_2019 <- load_pbp(2017:2019)

# names(model_2019)

train <- model_2019 %>% 
  select(home_team, away_team, season, week, div_game, result, spread_line,
         passing_yards, pass_attempt, posteam, sack) %>% 
  filter(week %in% c(1:17),
         !is.na(posteam),
         season %in% c(2017,2018)) %>% 
  group_by(season, posteam, week ) %>% 
  summarise(home_team = unique(home_team),
            away_team = unique(away_team),
            div_game = unique(div_game),
            result = unique(result),
            spread = unique(spread_line),
            sacks = sum(sack, na.rm = TRUE),
            passing_yards = sum(passing_yards, na.rm = TRUE),
            pass_attempts = sum(pass_attempt, na.rm = TRUE)- sacks) %>% 
  mutate(fav_team = case_when(
    spread < 0 ~ away_team,
    spread >= 0 ~ home_team
  ), spread = case_when(
    fav_team == away_team ~ -spread,
    fav_team == home_team ~ spread
  ),fav_win = case_when(
    fav_team == away_team & result <= 0 ~ 1,
    fav_team == away_team & result > 0 ~ 0,
    fav_team == home_team & result >= 0 ~ 1,
    fav_team == home_team & result <0 ~ 0
  ), win = case_when(
    fav_team == posteam ~ fav_win,
    fav_team != posteam & fav_win == 0 ~ 1,
    fav_team != posteam & fav_win == 1 ~ 0
  ), 
  cum_pass_yards = lag(cumsum(passing_yards), k = 1, default = 0),
  avg_pass_yards = cum_pass_yards/(as.numeric(week)-1),
  cum_pass_attempts = lag(cumsum(pass_attempts), k = 1, default = 0),
  avg_pass_attempts = cum_pass_attempts/(as.numeric(week)-1),
  ) %>%
  ungroup(season) %>% 
  mutate(
    winning_form_5 = lag(rollsum(win, 5, align = "right", all.x = TRUE ,fill = NA), k = 1),
    winning_form_10 = lag(rollsum(win, 10, align = "right", all.x = TRUE ,fill = NA), k = 1),
  ) %>% 
  select(season, week, fav_team, everything()) %>% 
  filter(!(season == 2017 & week %in% c(1:11))) %>% 
  mutate( 
   previous = season - 1,
   ypa = case_when(
    week %in% c(2:17) ~ cum_pass_yards/cum_pass_attempts,
    week == 1 ~  max(cum_pass_yards[season %in% previous])
    )
   )%>% 
  arrange(season, week)

train_favorites <- train %>% 
  filter(fav_team == posteam)

train_unders <- train %>% 
  filter(fav_team != posteam)

train_favorites$under_form <- train_unders$winning_form_10

######## TESTING ###########

log.model <- glm(fav_win ~ ypa + winning_form_10 + under_form, 
                 family = binomial, data = train_favorites)
summary(log.model)

test <- model_2019 %>% 
  select(home_team, away_team, season, week, div_game, result, spread_line,
         passing_yards, pass_attempt, posteam) %>% 
  filter(week %in% c(1:17),
         !is.na(posteam)) %>% 
  group_by(season, posteam, week ) %>% 
  summarise(home_team = unique(home_team),
            away_team = unique(away_team),
            div_game = unique(div_game),
            result = unique(result),
            spread = unique(spread_line),
            passing_yards = sum(passing_yards, na.rm = TRUE),
            pass_attempts = sum(pass_attempt, na.rm = TRUE)) %>% 
  mutate(fav_team = case_when(
    spread < 0 ~ away_team,
    spread >= 0 ~ home_team
  ), spread = case_when(
    fav_team == away_team ~ -spread,
    fav_team == home_team ~ spread
  ),fav_win = case_when(
    fav_team == away_team & result <= 0 ~ 1,
    fav_team == away_team & result > 0 ~ 0,
    fav_team == home_team & result >= 0 ~ 1,
    fav_team == home_team & result <0 ~ 0
  ), win = case_when(
    fav_team == posteam ~ fav_win,
    fav_team != posteam & fav_win == 0 ~ 1,
    fav_team != posteam & fav_win == 1 ~ 0
  ), 
  cum_pass_yards = lag(cumsum(passing_yards), k = 1, default = 0),
  avg_pass_yards = cum_pass_yards/(as.numeric(week)-1),
  cum_pass_attempts = lag(cumsum(pass_attempts), k = 1, default = 0),
  avg_pass_attempts = cum_pass_attempts/(as.numeric(week)-1),
  ypa = cum_pass_yards/cum_pass_attempts
  ) %>%
  ungroup(season) %>% 
  mutate(
    winning_form_5 = lag(rollsum(win, 5, align = "right", all.x = TRUE ,fill = NA), k = 1),
    winning_form_10 = lag(rollsum(win, 10, align = "right", all.x = TRUE ,fill = NA), k = 1)
  ) %>% 
  select(season, week, fav_team, everything()) %>% 
  filter(
         season == 2019) %>% 
  arrange(season, week)
test

test_favorites <- test %>% 
  filter(fav_team == posteam)

test_unders <- test %>% 
  filter(fav_team != posteam)

test_favorites$under_form <- test_unders$winning_form_10



library(gains)
library(faraway)
# Logistic 
train_favorites$logprobs <- predict(log.model, train_favorites, type = "response")

train_favorites <- train_favorites %>% 
  ungroup() %>% 
  mutate(spread_performance = cumsum(fav_win)/as.numeric(row.names(train_favorites)),
         mod_pred = case_when(
           logprobs > .5 ~ 1,
           logprobs <= .5 ~ 0
         ),
         model_performance = cumsum(mod_pred == fav_win)/as.numeric(row.names(train_favorites))
         )
  



test$fav_win <- as.numeric(test$fav_win)
log.gains <- gains(test$fav_win, logprobs, groups = 6)
log.gains
plot(log.gains, ylim = c(0,1))
plot(log.gains$depth, log.gains$lift, col = "red", xlab = "Depth", ylab = "Lift", xlim = c(0,105), ylim = c(0, 180))
lines(log.gains$depth, log.gains$lift, col = "red")
points(log.gains$depth, log.gains$cume.lift, col = "blue")
lines(log.gains$depth, log.gains$cume.lift, col = "blue")
title("Logisitic Lift Plot")
abline(h=100)

