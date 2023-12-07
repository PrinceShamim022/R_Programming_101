library(palmerpenguins)
library(tidyverse)
library(rstatix)

df <- penguins %>%
  select(species, flipper_length_mm)

summary(df)


## Visualise the flipper length against Species
library(ggplot2)
ggplot(df) +
  aes(x = species, y = flipper_length_mm, color = species) +
  geom_jitter() +
  theme(legend.position = "none")



## Normality Test
res_aov <- aov(flipper_length_mm ~ species,
               data = df)

## 1 check normality visually:

par(mfrow = c(1, 2)) # combine plots

# a. histogram
hist(res_aov$residuals)

# b. QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE # id = FALSE to remove point identification)
       
       
       
       ## 2 shapiro.test
       shapiro.test(res_aov$residuals)
       
       
       ### homogeneity
       # H0:  variances are equal
       # H1: at least one variance is different
       
       par(mfrow = c(1, 2)) # combine plots
       # a. Boxplot
       boxplot(flipper_length_mm ~ species,
               data = df)
       
       # b. Dotplot
       library("lattice")
       dotplot(flipper_length_mm ~ species,
               data = df)
       
       
       ## 2 # Levene's test
       library(car)
       
       leveneTest(flipper_length_mm ~ species,
                  data = df)
       # Accept the null hypothesis
       
       
       ## Alternate ways
       par(mfrow = c(1, 2)) # combine plots
       
       # 1. Homogeneity of variances
       plot(res_aov, which = 3)
       
       # 2. Normality
       plot(res_aov, which = 2)
       
       
       ## Outliers
       boxplot(flipper_length_mm ~ species,
               data = df)
       
       # Outliers
       df %>% 
         group_by(species) %>%
         identify_outliers(flipper_length_mm)
       # There were no extreme outliers.
       
       
       ### ANOVA Test
       library(ggplot2)
       
       ggplot(df) +
         aes(x = species, y = flipper_length_mm) +
         geom_boxplot()
       
       
       
       aggregate(flipper_length_mm ~ species,
                 data = df,
                 function(x) round(c(mean = mean(x), sd = sd(x)), 2))
       
       
       
       library(dplyr)
       
       group_by(df, species) %>%
         summarise(
           mean = mean(flipper_length_mm, na.rm = TRUE),
           sd = sd(flipper_length_mm, na.rm = TRUE))
       
       
       
       res_aov <- aov(flipper_length_mm ~ species,
                      data = dat)
       
       summary(res_aov)