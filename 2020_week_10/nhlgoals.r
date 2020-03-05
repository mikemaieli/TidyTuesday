#load libraries
library(tidyverse)
library(hrbrthemes)
library(directlabels)

#load data
game_goals <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')
top_250 <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')
season_goals <- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

#select top 10 players who have full records
top10_game_goals <- game_goals %>% 
  filter(player == "Wayne Gretzky" |
           player == "Jaromir Jagr" |
           player == "Brett Hull" |
           player == "Mike Gartner" |
           player == "Alex Ovechkin" |
           player == "Mark Messier" |
           player == "Steve Yzerman")

#create data frame to have player, cumulative games played, and cumulative goals
goals_by_game_played <- top10_game_goals %>%
  group_by(player) %>% 
  mutate(game_number = row_number()) %>% 
  mutate(cum_goals = cumsum(goals)) %>% 
  select(player, game_number, cum_goals)

#line plot
ggplot(goals_by_game_played,
       aes(x = game_number,
           y = cum_goals,
           color = player,
           size = player,
           group = player)) +
  geom_line() +
  scale_color_manual(values = c("Wayne Gretzky" = "#041E42",
                                "Jaromir Jagr" = "grey",
                                "Brett Hull" = "grey",
                                "Mike Gartner" = "grey",
                                "Alex Ovechkin" = "#C8102E",
                                "Mark Messier" = "grey",
                                "Steve Yzerman" = "grey"
                                )) +
  scale_size_manual(values = c("Wayne Gretzky" = 3,
                                "Jaromir Jagr" = 0.5,
                                "Brett Hull" = 0.5,
                                "Mike Gartner" = 0.5,
                                "Alex Ovechkin" = 3,
                                "Mark Messier" = 0.5,
                                "Steve Yzerman" = 0.5
                                )) +
  geom_point(aes(x = 886, y = 700), color = "#FF4C00") +
  annotate("text", x = 300, y = 750, label = "Gretzky only needed 886 games\nto reach 700 goals", color = "black", hjust = 0) +
  annotate("segment", x = 750, xend = 886, y = 740, yend = 700, color = "#FF4C00") +
  scale_y_continuous(name = "Goals scored", limits = c(0,1000), breaks = seq(0, 1000, by = 100)) +
  scale_x_continuous(name = "Game number", limits = c(0,2000),breaks = c(0, 500, 1000, 1500, 2000)) +
  geom_dl(aes(label = player), method = list(dl.trans(x = x + 0.2), "last.points", cex = 0.8)) +
  labs(title = "Will Ovi play enough games to outscore Gretzky?",
       subtitle = "Wayne Gretzky scored 894 goals in his 1,487 game NHL career. His pace of .60 goals per game is unmatched, 
but can a longer career help Alex Ovechkin break the career goals record?",
       caption = "source: hockeyreference.com
       analysis and visualization for #tidytuesday by @mikemaieli") +
  scale_alpha_manual(values = c("Alex Ovechkin" = 5)) +
  theme_ipsum_rc() +
  theme(legend.position = "none")
