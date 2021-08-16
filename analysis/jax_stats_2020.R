library(nflfastR)
library(dplyr)
library(ggplot2)
options(scipen = 9999)


# JAGUARS OFFENSE GGPLOT -------------

pbp <- load_pbp(1999:2020)

# Get results for all home teams
pbphome <- pbp %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team) %>%
  distinct(week, team, season, .keep_all = TRUE)

# Get results for all away teams
pbpaway <- pbp %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  mutate(result = -result) %>%
  distinct(week, team, season, .keep_all = TRUE)

# Calculate whether the winner based off the results
pbpwins <- rbind(pbpaway, pbphome) %>%
  mutate(win = case_when(
    result > 0 ~ 1,
    result < 0 ~ 0,
    result == 0 ~ .5
  )) %>%
  group_by(team, season) %>%
  summarise(wins = sum(win))

# Calculate the offensive stats for each team
stats <- pbp %>%
  filter(!is.na(posteam)) %>%
  group_by(posteam, season) %>%
  rename(team = posteam) %>%
  summarise(
    passing_yards = sum(passing_yards, na.rm = TRUE),
    rushing_yards = sum(rushing_yards, na.rm = TRUE)
  )

# Join all the data sets together using the team name
offense <- left_join(pbpwins, stats)
offense <- offense %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# Filter to only include the stats for the Jaguars
# Also calculate the misery index for each season using a custom function 
jaxoffense <- offense %>%
  filter(team == "JAX") %>%
  mutate(misery = misery(wins, rushing_yards + passing_yards))


jaxoffense %>%
  ggplot(aes(x = rushing_yards, y = passing_yards)) +
  # horizontal line with mean passing yards
  geom_hline(
    yintercept = mean(jaxoffense$passing_yards),
    color = "red",
    linetype = "dashed",
    alpha = 0.5
  ) +
  # vertical line with mean rushing yards
  geom_vline(
    xintercept = mean(jaxoffense$rushing_yards),
    color = "red",
    linetype = "dashed",
    alpha = 0.5
  ) +
  # add points for the teams with the right colors
  # cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = "#008080", cex = jaxoffense$wins, alpha = .6) +
  # add names using ggrepel, which tries to make them not overlap
  # include the calculated misery in the label
  geom_text_repel(aes(label = paste0(season, " Misery: ", misery))) +
  # add a smooth line fitting rushing and passing yards
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = "lm") +
  # titles and caption
  labs(
    x = "Offensive Rushing Yards",
    y = "Offensive Passing Yards",
    title = "JAX Offense Impact on Wins 1999:2020",
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
