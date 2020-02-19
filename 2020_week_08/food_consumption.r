#load libraries
library(tidyverse)
library(hrbrthemes)

#load data
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

#arrange data
food_consumption_new <- food_consumption %>% 
  gather(consumption, co2_emmission, key = measure, value = "kg_per_capita_year") %>% 
  mutate(food_category=recode(food_category,
                              `Milk - inc. cheese` = "Milk & Cheese",
                              `Wheat and Wheat Products` = "Wheat",
                              `Nuts inc. Peanut Butter` = "Nuts"))

food_consumption_new$food_category <- factor(food_consumption_new$food_category, levels = c("Beef", "Milk & Cheese", "Lamb & Goat", "Pork", "Fish", "Poultry", "Rice", "Wheat", "Eggs", "Nuts", "Soybeans"))

ggplot(food_consumption_new, aes(measure, kg_per_capita_year, group = country)) +
  geom_line(color = "gray") +
  scale_x_discrete(limits = c("consumption", "co2_emmission"), labels = c("consumption" = "Supplied for\nconsumption", "co2_emmission" = "CO2 emission")) +
  labs(y = "Kg per capita per year",
       x = "",
       title = "Food Supplied for Consumption and Subsequent CO2 Emissions",
       subtitle = "The charts below show how much food is supplied for consumption and its subsequent carbon dioxide emissions\nin kilograms per capita, per year, for over 125 countries. The median values of the countries are shown in gold.\nAs we can see, beef supplied produces the most CO2 emissions followed by milk and cheese.",
       caption = "source: food and agriculture organization of the united nations via nu3
       data scraped and wrangled by kasia kulma
       analysis and visualization for #tidytuesday by @mikemaieli") + 
  facet_wrap(~food_category, ncol=3, scales = "free_x") +
  stat_summary(aes(group = food_category), fun = median, geom="line", color="#FFB600", size = 1.5) +
  stat_summary(aes(group = food_category), fun = median, geom="point", shape = 21, color="#FFB600", fill="#3C3C3C", size = 1.5, stroke = 1.5) +
  theme_ft_rc(strip_text_face = "bold")
