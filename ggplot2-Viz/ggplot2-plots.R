# github https://github.com/PrinceShamim022/R_Projects_101

library(RColorBrewer)
library(tidyverse)
library(forcats)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(gapminder)
library(metan)
library(ggcorrplot)
display.brewer.all(colorblindFriendly = TRUE)
theme_set(theme_bw())

gapminder %>%
  filter(continent %in% c("Africa", "Europe")) %>%
  filter(gdpPercap < 30000) %>%
  ggplot(aes(x= gdpPercap,
             y = lifeExp,
             size = pop,
             color = year))+
  geom_point() +
  facet_wrap(~continent)+
  labs(title = "Life expectancy explained by GDP per capita",
       x = "GDP per capita",
       y = "Life expectancy")

# One or more categorical variables

starwars %>%
  ggplot(aes(x = height)) +
  geom_histogram(binwidth = 20,
                 show.legend = F,
                 alpha = .5) +
  labs(title = "Histogram",
       x = "Height",
       y = "Count")+
  theme_bw()

starwars %>%
  ggplot(aes(x = height))+
  geom_density(aes(fill = "blue"),
               show.legend = F,
               alpha = .5)+
  labs(title = "Density plot",
       x = "Height",
       y = "Probability")

starwars %>%
  ggplot(aes(x = height))+
  geom_boxplot(show.legend = F, fill = "steelblue",
               alpha = .3)+
  labs(title = "Boxplot",
       x = "Height")

starwars %>%
  ggplot(aes(x = height, y = 1)) +
  geom_violin(aes(fill = "blue"),
              show.legend = F,
              alpha = .5)+
  labs(title = "Violin plot",
       x = "Height")





# One numberic and two categorical variable

starwars %>%
  drop_na(gender) %>%
  ggplot(aes(height, fill = gender))+
  geom_boxplot(alpha = 0.3)+
  labs(title = "Boxplot of a numeric variable",
       subtitle = "disagregated by one categorical variable",
       x = "Height") +
  theme(legend.position = "none")


starwars %>%
  drop_na(gender) %>%
  ggplot(aes(height, fill = gender)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density plot of a numeric variable",
       subtitle = "disagregated by one categorical variable",
       x = "Height",
       y = "Probability") +
  theme(legend.position = "none")


starwars %>%
  drop_na(hair_color, gender) %>%
  filter(hair_color %in% c("black", "brown")) %>%
  ggplot(aes(height, fill = gender))+
  geom_density(alpha = 0.3)+
  facet_wrap(~hair_color)+
  labs(title = "Density plot of a numeric variable",
       subtitle = "disagregated by two categorical variables",
       x = "Height",
       y = "Probability")+
  theme(legend.position = "none")

starwars %>%
  filter(height > 140 & height < 200) %>%
  drop_na(hair_color, gender) %>%
  filter(hair_color %in% c("black", "brown")) %>%
  ggplot(aes(height, fill = gender)) +
  geom_boxplot(alpha = 0.3) +
  facet_wrap(~hair_color) +
  labs(title = "Boxplot of a numeric variable",
       subtitle = "disagregated by two categorical variable",
       x = "Height")+
  theme(legend.position = "bottom")



# Two numberic and one categorical variable

starwars %>%
  filter( mass < 250) %>%
  ggplot(aes(x = height,
             y = mass))+
  geom_point(size = 2,
             alpha = 0.7)+
  geom_smooth(method = lm)+
  labs(title = "Scatter plot",
       subtitle = "with smoothed linear model",
       x = "Height",
       y = "Mass")


starwars %>%
  filter( mass < 250) %>%
  drop_na(gender) %>%
  ggplot(aes(height, mass, colour = gender))+
  geom_jitter()+
#  geom_smooth(method = lm)+
  geom_point(size = 5, show.legend = T)+
  labs(title = "Scatter plot",
       subtitle = "disagregated by colour",
       x = "Height",
       y = "Mass")

starwars %>%
  filter( mass < 250) %>%
  drop_na(gender) %>%
  ggplot(aes(height, mass, colour = gender))+
  geom_point(size = 5, show.legend = F)+
  facet_wrap(~gender)+
  labs(title = "Scatter plot",
       subtitle = "disagregated by colour and facets",
       x = "Height",
       y = "Mass")
