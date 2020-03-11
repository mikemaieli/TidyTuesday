#load libarary
library(tidyverse)
library(hrbrthemes)
library(ggrepel)
library(gghighlight)
library(skimr)
library(ggtext)

#load data
salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

#calculate quartiles
quantile(salary_potential$mid_career_pay, prob = seq(0, 1, length = 11), type = 5) #90th = 114000
quantile(salary_potential$make_world_better_percent, prob = seq(0, 1, length = 11), type = 5, na.rm = TRUE) #90th = 65

#plot
ggplot(data = salary_potential,
       aes(x = make_world_better_percent,
           y = mid_career_pay)) +
  geom_point(size = 5, alpha = .6, color = "blue") +
  scale_x_continuous(name = "PERCENT OF ALUMNI WHO SAY THEIR WORK MAKES THE WORLD A BETTER PLACE", 
                     limits = c(25,100),
                     labels = function(x) paste0(x, "%")) +
  scale_y_continuous(name = "MEDIAN MID-CAREER SALARY", 
                     limits = c(50000,170000),
                     labels = scales::dollar) +
  gghighlight(mid_career_pay >= 114000 & make_world_better_percent >= 65,
              label_key = name,
              label_params =  list(fill = "na", 
                                   force = 2,
                                   family = "Arial Narrow",
                                   fontface = 2)) +
  geom_hline(yintercept = median(salary_potential$mid_career_pay, na.rm = TRUE), size = 1) +
  geom_vline(xintercept = median(salary_potential$make_world_better_percent, na.rm = TRUE), size = 1) +
  annotate("text", x = 25, y = 92000, label = "median", hjust = 0, fontface = 2, family = "Arial Narrow") +
  annotate("text", x = 52.5, y = 55000, label = "median", hjust = 0, fontface = 2, family = "Arial Narrow") +
  annotate("text", x = 25, y = 170000, label = "MORE MONEY, LESS MEANING", hjust = 0, family = "Arial Narrow", color = "#808080") +
  annotate("text", x = 99, y = 170000, label = "MORE MONEY, MORE MEANING", hjust = 1, family = "Arial Narrow", color = "#808080") +
  annotate("text", x = 99, y = 55000, label = "LESS MONEY, MORE MEANING", hjust = 1, family = "Arial Narrow", color = "#808080") +
  annotate("text", x = 25, y = 55000, label = "LESS MONEY, LESS MEANING", hjust = 0, family = "Arial Narrow", color = "#808080") +
  labs(title = "Graduates of military- and healthcare-focused institutions report a combination of high salaries and meaningful work",
       subtitle = "A payscale.com salary survey asked millions of people about their salary, if and where they earned a bachelor’s degree, and if they felt their work makes the world a better place.<br/><span style = 'color:#575ECF'>**Eight US schools**</span> made the 90th percentile of both mid-career salary and percent who say their work is making the world a better place, all of which are military or healthcare focused schools.",
       caption = "source: payscale.com college salary report (bachelor's)
       analysis and visualization for #tidytuesday by @mikemaieli") +
  theme_ipsum(axis_title_size = 11) +
  theme(plot.subtitle = element_markdown())
