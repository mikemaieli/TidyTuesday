#load libraries
library(tidyverse)
library(hrbrthemes)
library(tidytuesdayR)

#load data
tues_data <- tt_load("2020-10-06")
tournament <- tues_data$tournament

#highlight win pct < 85%
lessthan85 <- tournament %>% 
  filter(tourney_finish == "Champ") %>% 
  mutate(full_percent = (full_percent * .01)) %>% 
  filter(full_percent < .85)

#analyze and viz
tournament %>% 
  filter(tourney_finish == "Champ") %>% 
  mutate(full_percent = (full_percent * .01)) %>% 
  ggplot(aes(x = year,
             y = full_percent)) +
  geom_line() +
  geom_point(size = 2) +
  scale_y_continuous(name = "winning percentage",
                     labels = scales::percent, 
                     limits = c(0,1),
                     minor_breaks = 0,
                     breaks = c(0, .25, .50, .75, .85, 1)) +
  scale_x_continuous(name = "season",
                     limits = c(1980, 2020),
                     minor_breaks = 0) +
  geom_point(data = lessthan85, aes(x = year, y = full_percent), color = "#f77f00", size = 3) +
  geom_hline(yintercept = .85, size = .5, color = "#f77f00", linetype="dashed") +
  annotate("text", y = .7, x = 1983, label = "In 1987, Tennessee posted\na 23-6 regular season record\nand went on to win the\nNCAA Tournament.", family = "Roboto Condensed", color = "white", vjust = 1, hjust = 0, lineheight = 1) +
  annotate("text", y = .57, x = 1993, label = "In 1997, Tennessee won only\n74% of their games, the lowest\nwinning percentage of any NCAA\nTournament winner in history.", family = "Roboto Condensed", color = "white", vjust = 1, hjust = 0, lineheight = 1) +
  annotate("text", y = .67, x = 2003, label = "Tennessee did slightly better in 1998,\nwinning 80% of their games on the\nway to the NCAA Tournament title.", family = "Roboto Condensed", color = "white", vjust = 1, hjust = 0, lineheight = 1) +
  annotate(geom = "curve", x = 1986.5, y = .82, xend = 1983, yend = .72, curvature = .2, color = "white", size = .5) +
  annotate(geom = "curve", x = 1996.5, y = .74, xend = 1993, yend = .59, curvature = .3, color = "white", size = .5) +
  annotate(geom = "curve", x = 1998.5, y = .80, xend = 2003, yend = .69, curvature = -.2, color = "white", size = .5) +
  labs(title = "Tennessee is the only school to win less than 85% of their games and still win the NCAA Tournament",
       subtitle = "Winning percentage of NCAA Women's Basketball Tournament champions, 1982 to 2018",
       caption = "data source: FiveThirtyEight
       analysis and visualization for #tidytuesday by @mikemaieli") +
  theme_ft_rc()
