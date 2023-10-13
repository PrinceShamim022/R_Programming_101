# https://github.com/PrinceShamim022/R_Projects_101

# ggplot2 plots


# Install Packages
#install.packages("highcharter")
#install.packages("plotly")
#install.packages("ggthemes")
#install.packages("gpubr")
install.packages("devtools")

# Libraries
library(ggplot2)
library(dplyr)
library(plotly)
library(highcharter)
library(ggthemes) # use themes to clean up the data visualizations
library(gpubr) #load in library for multi-panel figures
library(tidyverse)
library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(gapminder)
library(patchwork)
theme_set(theme_bw())

# Read in dataset
data(iris)

df <- iris
view(df)

# Bar plot
view(starwars)
names(starwars)
unique(starwars$hair_color)

starwars %>% 
  filter(hair_color %in% c("black", "brown")) %>% 
  drop_na(sex) %>% 
  ggplot(aes(hair_color, fill = sex))+
  geom_bar(position = "dodge", alpha = 0.5)+
  theme(panel.grid.major = element_blank(),   # theme for get ride off grid
        panel.grid.minor = element_blank())+  # theme for get ride off grid
  facet_wrap(~sex)      # classify by Sex - Multi Panel
  labs(title = "Gender with Hair Color",
       x = "Hair Color",
       y = "Count")

# Alternative

starwars %>% 
  filter(hair_color %in% c("black", "brown")) %>% 
  drop_na(sex) %>% 
  ggplot(aes(hair_color, fill = sex))+
  geom_bar(position = "fill", alpha = 0.5)+
  theme(panel.grid.major = element_blank(),   # theme for get ride off grid
        panel.grid.minor = element_blank())+  # theme for get ride off grid
#  theme(legend.position = c(0.99, 0.78))+  # legend positon
  labs(title = "Gender with Hair Color",
       x = "Hair Color",
       y = "Count")
  

# Using attributes for sample plot
# Plot  
df %>% 
  ggplot(aes(Sepal.Length, Petal.Length, colour=Species))+
  geom_point(size = 3, alpha = 0.5)+  # point customization
  geom_smooth(method = lm)+    # line smooth by linear model
  labs(title = "Sepal Length vs Petal Length",
  subtitle = "classify by Species",
       x = "Sepal Length",
       y = "Petal Length")+
  theme_bw()+    # black and white theme
#  theme(legend.position="none")+ # Remove Legend
#  theme(legend.title = element_blank()+ # hide the legend title
#  theme(legend.position="top")+     # Change the legend position
#  theme(legend.position="bottom")+  # Change the legend position
  theme(legend.title = element_text(colour="blue", 
                                    size=10,        ## Change the legend title
                                    face="bold"))+
  theme(legend.text = element_text(colour="red", 
                                   size=8,         #Change the legend labels
                                   face="bold"))


# Scatter Plots and Jitter Plots
# Basic Scatter Plot

df %>% 
  ggplot(aes(Sepal.Length, Petal.Length,
             colour=Species))+  # color by species
  geom_point(shape = 1)+  # Change the shape of points (dot to circle)
  geom_jitter()
#  geom_smooth(method=lm, se=FALSE)+  # Add a linear regression line and SD is false 


# Jitter plots include special effects with which scattered plots 
# can be depicted. Jitter is nothing but a random value that is 
# assigned to dots to separate them as mentioned below:

data("mpg")
view(mpg)
names(mpg)

df1 %>% 
  ggplot(aes(cyl, hwy))+
  geom_point()+
  geom_jitter(aes(colour = class))

# Boxplot 1

mpg %>% 
  ggplot(aes(manufacturer, displ, col = manufacturer)) +
  geom_boxplot() +
  stat_summary(fun = "mean", position = "identity", geom = "point", shape = 8,
               size = 2, color = "white")+
  geom_jitter(width = 0.2, alpha = 0.8)+
  theme(panel.grid.major = element_blank(),   # theme for get ride off grid
        panel.grid.minor = element_blank())+ # theme for get ride off grid
  theme_bw()

ggsave("boxplot.png",  units="in", width=6.5, height=5.5)


# A bar count plot

df1 %>% 
  ggplot(aes(factor(cyl)))+
  geom_bar(stat="count")

# geom_bar() is the function which is used for creating bar plots. 
# It takes the attribute of statistical value called count. 

# A historgram count plot 
df1 %>% 
  ggplot(aes(hwy))+
  geom_histogram( col="red",
                  fill = "green",
                  alpha = 0.3,
                  binwidth = 5)


# Stacked Bar Chart
df1 %>% 
  ggplot(aes(class))+
  geom_bar(col="skyblue",
           fill = "steelblue",
           alpha = 0.3,
           binwidth = 5)

# Creating Pie Charts
df1 %>% 
ggplot(aes(" " , "freq", fill = factor(class)))+
geom_bar(width = 1, stat = "identity")+
theme(axis.line = element_blank(),
      plot.title = element_text(hjust=0.5))+
  labs(title="Pie Chart of class",
       fill="class",
       x=NULL,
       y=NULL,
       caption="Source: mpg")+
  coord_polar(theta = "y", start=0) # Creating co-ordinates (circular pie)


# Marginal Plots

# install.packages("ggExtra")
library(ggExtra)

plot <-  df1 %>%           # rename the code as "Plot"
  ggplot(aes(cty, hwy))+
  geom_smooth(method = lm, se = F)+
  geom_count()+
  theme(legend.position="none")

# Now see the Marginal plot
 
ggMarginal(plot, type = "histogram", fill="transparent")   
ggMarginal(plot, type = "boxplot", fill="transparent")
ggMarginal(plot, type = "density", fill="transparent")


# Barplot

df4 <- msleep %>%
  drop_na(vore) %>% 
  ggplot(aes(vore))+
  geom_bar(fill = "steelblue")+
  coord_flip()+                  # horizontal flip
  theme_bw()+
  labs(title = "Number of Observation per Order",
       x = "Vore",
       y = "NULL")

# Line Graph
## Two numeric and one Categorical variable

Orange %>% 
  filter(Tree != "1" & 
         Tree != "4") %>% 
  ggplot(aes(age, circumference, colour = Tree))+
  geom_point(sizze = 5, alpha = 0.3)+
  geom_line(size = 1)+
  theme(panel.grid.major = element_blank(),   # theme for get ride off grid
        panel.grid.minor = element_blank())+  # theme for get ride off grid
  labs(title = "Tree age and Circumference",
       x = "age",
       y = "circumference")


  
# Lolipor Graph

chickwts %>%
  group_by(feed) %>%
  mutate(mean_by_feed = mean(weight)) %>%
  ungroup() %>%
  mutate(feed = fct_reorder(feed, mean_by_feed)) %>%
  ggplot(aes(feed, weight, colour = feed,
             show.legend = F))+
  coord_flip() +
  geom_jitter(show.legend = F,
              size = 4,
              alpha = 0.2,
              width = 0.05) +
  stat_summary(fun = mean, geom = "point", size = 8, show.legend = F)+
  geom_hline(aes(yintercept = mean(weight)),
             colour = "gray70",
             size = 0.9)+
  geom_segment(aes(x = feed, xend = feed,
                   y = mean(weight), yend = mean_by_feed),
               size = 2, show.legend = F)+
  labs(title = "Weight of chickens by feed group",
       x = "Feed",
       y = "Weight of chickens") +
  theme(legend.position = "none") +
  theme_bw()


# tempareture Using ridges

ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,
                               alpha = 5) +
  scale_fill_viridis(name = "Temp. [F]", option = "C")+
  labs(title = 'Temperatures in Lincoln NE in 2016')+
  theme_bw()+
  theme(legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8))


# ErrorBar Plot
# 1 numeric and 1 categorical variable and 1 factor

df3 <- ToothGrowth
view(df3)
names(df3)
unique(df3$dose)
class(df3$len)

df3 %>% 
  filter(supp == "VC") %>% 
  mutate(dose = as.factor(dose)) %>% 
  group_by(dose) %>% 
  summarise(Mean_length = mean(len),
            Sd_length = sd(len)) %>% 
  ggplot(aes(dose, Mean_length))+
  geom_bar(stat = "identity",
           fill = "skyblue",
           alpha = 0.7)+
  geom_errorbar(aes(x = dose,
                    ymin = Mean_length - Sd_length,
                    ymax = Mean_length + Sd_length,
                    width = 0.05))+
  labs(title = "Average Tooth Growth for VC",
       xlab = "dose",
       ylab = "Average Tooth Growth")

# ErrorBar Plot (Alternative)             
             
df3 %>% 
  filter(supp == "VC") %>% 
  mutate(dose = as.factor(dose)) %>% 
  group_by(dose) %>% 
  summarise(Mean_length = mean(len),
            Sd_length = sd(len)) %>% 
  ggplot(aes(dose, Mean_length))+
  geom_point(size = 3,
           colour = "skyblue")+
  geom_errorbar(aes(x = dose,
                    ymin = Mean_length - Sd_length,
                    ymax = Mean_length + Sd_length,
                    width = 0.05))+
  labs(title = "Average Tooth Growth for VC",
       xlab = "dose",
       ylab = "Average Tooth Growth")

 
# create four separate Plots         

p1 <- starwars %>%
  drop_na(gender) %>%
  ggplot(aes(height, fill = gender))+
  geom_boxplot(alpha = 0.3)+
  labs(title = "Boxplot of a numeric variable",
       subtitle = "disagregated by one categorical variable",
       x = "Height")+
  theme(legend.position = "none")  

p2 <- starwars %>%
  drop_na(gender) %>%
  ggplot(aes(height, fill = gender))+
  geom_density(alpha = 0.3)+
  labs(title = "Density plot of a numeric variable",
       subtitle = "disagregated by one categorical variable",
       x = "Height",
       y = "Probability")+
  theme(legend.position = "none")

 p3 <- starwars %>%
  drop_na(hair_color, gender) %>%
  filter(hair_color %in% c("black", "brown")) %>%
  ggplot(aes(height, fill = gender))+
  geom_density(alpha = 0.3)+
#  facet_wrap(~hair_color)+
  labs(title = "Density plot of a numeric variable",
       subtitle = "disagregated by two categorical variables",
       x = "Height",
       y = "Probability")+
  theme(legend.position = "none")

p4 <- starwars %>%
  filter(height > 140 & height < 200) %>%
  drop_na(hair_color, gender) %>%
  filter(hair_color %in% c("black", "brown")) %>%
  ggplot(aes(height, fill = gender))+
  geom_boxplot(alpha = 0.3)+
#  facet_wrap(~hair_color)+
  labs(title = "Boxplot of a numeric variable",
       subtitle = "disagregated by two categorical variable",
       x = "Height")+
  theme(legend.position = "bottom")

# Multi-Panel 
p1 + p2 + p3 + p4