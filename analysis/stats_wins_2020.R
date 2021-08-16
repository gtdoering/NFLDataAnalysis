library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)


# 2020 SEASON STATS AND WINS --------------

pbp20 <- load_pbp(2020)

# Get the results for all the home teams
pbp20home <- pbp20 %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team) %>%
  distinct(week, team, .keep_all = TRUE)

# Get the results for all the away teams
pbp20away <- pbp20 %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  mutate(result = -result) %>%
  distinct(week, team, .keep_all = TRUE)

# Create a column that determines if a team won or lost
pbp20wins <- rbind(pbp20away, pbp20home) %>%
  mutate(win = case_when(
    result > 0 ~ 1,
    result < 0 ~ 0,
    result == 0 ~ .5
  )) %>%
  group_by(team) %>%
  summarise(wins = sum(win))

# Get the stats for each team in 2020
stats20 <- pbp20 %>%
  filter(!is.na(posteam)) %>%
  group_by(posteam) %>%
  rename(team = posteam) %>%
  summarise(
    passing_yards = sum(passing_yards, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE)
  )

# Join all of the data sets
offense <- left_join(pbp20wins, stats20) %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

offense %>%
  ggplot(aes(x = rushing_yards, y = passing_yards)) +
  # horizontal line with mean passing yards
  geom_hline(
    yintercept = mean(offense$passing_yards),
    color = "red",
    linetype = "dashed",
    alpha = 0.5
  ) +
  # vertical line with mean rushing yards
  geom_vline(
    xintercept = mean(offense$rushing_yards),
    color = "red",
    linetype = "dashed",
    alpha = 0.5
  ) +
  # add points for the teams with the tean logo
  ggimage::geom_image(aes(image = team_logo_espn),
    size = offense$wins / 250,
    asp = 16 / 9
  ) +
  # add a smooth line fitting passing yards and rushing yards
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = "lm") +
  # titles and caption
  labs(
    x = "Offensive Rushing Yards for 2020",
    y = "Offensive Passing Yards for 2020",
    title = "Offense Impact on Wins for 2020",
    caption = "Data: @nflfastR"
  ) +
  # uses the black and white ggplot theme
  theme_bw() +
  # center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  # make ticks look nice
  # if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
