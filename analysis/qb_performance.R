library(nflfastR)
library(dplyr)
library(ggplot2)
library(ggrepel)
options(scipen = 9999)


# FOURTH DOWN ANALYSIS ---------------------
data <- load_pbp(2019)

# Filter the data into attempted fourth down conversions
down4 <- data %>%
  filter(down == 4 & special == 0) %>%
  filter(fourth_down_converted != 0 | fourth_down_failed != 0) %>%
  select(down, ydstogo, desc, fourth_down_converted, fourth_down_failed)

head(down4)

down4$fourth_down_converted <- as.logical(down4$fourth_down_converted)

ggplot(down4, aes(
  x = fourth_down_converted,
  y = ydstogo,
  group = fourth_down_converted,
  fill = fourth_down_converted
  )) +
  geom_boxplot()


# QB EFFICIENCY --------------------
pbp <- load_pbp(2016:2020)

# Plays per season
pbp %>%
  group_by(season) %>%
  summarize(n = n())

# Plays of each type
pbp %>%
  group_by(play_type) %>%
  summarize(n = n())

# Filter regular season plays and calculate averages for each quarterback
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

# Add team colors to the dataset
qbs <- qbs %>%
  left_join(teams_colors_logos, by = c("team" = "team_abbr"))

# Plot the data
qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  # horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), 
             color = "red", 
             linetype = "dashed", 
             alpha = 0.5) +
  # vertical line with mean CPOE
  geom_vline(xintercept = mean(qbs$cpoe), 
             color = "red", 
             linetype = "dashed", 
             alpha = 0.5) +
  # add points for the QBs with the right colors
  # cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = qbs$team_color, cex = qbs$n_plays / 350, alpha = .6) +
  # add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label = name)) +
  # add a smooth line fitting cpoe + epa
  stat_smooth(geom = "line", alpha = 0.5, se = FALSE, method = "lm") +
  # titles and caption
  labs(
    x = "Completion % above expected (CPOE)",
    y = "EPA per play (passes, rushes, and penalties)",
    title = "Quarterback Efficiency, 2015 - 2019",
    caption = "Data: @nflfastR"
  ) +
  # uses the black and white ggplot theme
  theme_bw() +
  # center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  # make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
