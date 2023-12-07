## 7 Step guide to make Publication Ready Bargraphs from Scratch

# Intsall these packages if you have not and load them:
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)

data("chickwts")
tibble(chickwts) #tibble is a function of dplyr package

## 2- Calculate means of you treatment groups and 
# the standard deviation SD to show on error bars as follows:
mean_data <- group_by(chickwts, feed) %>% #feed is our treatmnet in this example
  summarise(weight_mean=mean(weight), sd = sd(weight)) %>% #to calculate mean and SD
  arrange(desc(weight_mean)) #to arange in descending order
tibble(mean_data)


## 3- This step involves performing analysis of variance ANOVA, 
# using buitin aov() function.
# Here we will draw ANOVA of weight against the group of treatment (feed) in Chickwts data

anova <- aov(weight ~ feed, data = chickwts)
summary(anova)


## 4- If the ANOVA is significantly diffrent then, 
# we will draw a multiple mean comparison test 
# (TUKEY HSD, LSD, or Duncan Multiple Range) on anova object from previous step.
# Here is an example of TUKEY HSD test Important Note: 
# You can also use other tests and look for their commands by using 
# agricolae package in this step.

tukey <- TukeyHSD(anova)
tukey


## 5- Draw multiple comparison letters using multcomp R package as follows:
group_letters <- multcompLetters4(anova, tukey)
# we have to mention both anova model and tukey objects to get group letters
group_letters


## As we have group letters now in step-5, we can extract these group letters 
# add them to our mean_data a data frame developed in step-2 as follows:

#extracting group letters
group_letters <- as.data.frame.list(group_letters$feed)
#adding to the mean_data
mean_data$group_letters <- group_letters$Letters
tibble(mean_data)


# 6- Drawing the publication ready Barplot in ggplot2
p <- ggplot(mean_data, aes(x = feed, y = weight_mean)) +
  geom_bar(stat = "identity", aes(fill = feed), show.legend = FALSE, width = 0.6) + #barplot
  geom_errorbar( #this argument is putting sd as error bars
    aes(ymin = weight_mean-sd, ymax=weight_mean+sd), 
    width = 0.1
  ) + 
  geom_text(aes(label = group_letters, y = weight_mean + sd), vjust=-0.4) + #add letters
  scale_fill_brewer(palette = "BrBG", direction = 1) + #theme setting
  labs(#this will add labels 
    x = "Feed Type",
    y = "Chicken Weight (g)",
    title = "Publication Ready Barplot",
#    subtitle = "Feed type vs Chick Weight",
    fill = "Feed Type"
  ) +
  ylim(0,410)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p


# 7- Saving upto 4K barplots in R
# First choose a working directory by pressing ctrl+shift+H and 
# select a folder then run the following code to save in .tiff:

tiff('Barplot.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')
p
dev.off()
