####  Steps guide to make Publication Ready Bargraphs from Scratch (TWO-Way ANOVA)


# This guide will show you how to make publication ready barplots with TWO-WAY ANOVA from scratch.
# Group barplot with TWO-WAy ANova in R
# Install these packages if you have not and load them:
library(ggplot2)
library(ggthemes)
library(multcompView)
library(dplyr)
library(stats) 

# 1- Create/load a data set
# We will use the following built-in dataset for this example:
data2 <- ToothGrowth
data2$dose = as.factor(data2$dose)


# 2- Calculating Two Way ANOVA
anova <- aov(len ~ supp*dose, data = data2) #two way anova
summary(anova) #represents ANOVA


# 3- Multiple Mean Comparison Analysis (only if you have significant differences in Two-way anova)
tukey <- TukeyHSD(anova)
tukey


## 4- Extract lettering from TWO-WAY ANOVA and Tukey’s Test:
# compact letter display
group_lettering <- multcompLetters4(anova, tukey) #
group_lettering


group_lettering2 <- data.frame(group_lettering$`supp:dose`$Letters)
group_lettering2


# 5- Calculating and adding mean, sd and lettering columns to the data set:
mean_data2 <- data2 %>% 
  group_by(supp, dose) %>% 
  summarise(len_mean=mean(len), sd = sd(len)) %>% #to calculate mean and SD
  arrange(desc(len_mean)) #to arange in descending order

tibble(mean_data2)

mean_data2$group_lettering <- group_lettering2$group_lettering..supp.dose..Letters


## 6- Drawing Publication ready Barplots with TWO-WAY ANOVA
# Draw Basic Barplot
ggplot(mean_data2, aes(x = dose, y = len_mean,group=supp))  +
  geom_bar(position=position_dodge(0.9),stat = "identity", aes(fill = supp), 
           show.legend = TRUE)



## *Add erro Bars *(sd)* on the plot*
ggplot(mean_data2, aes(x = dose, y = len_mean,group=supp))  +
  geom_bar(position=position_dodge(0.9),stat = "identity", aes(fill = supp), show.legend = TRUE) + #barplot
  geom_errorbar(aes(ymin = len_mean-sd, ymax=len_mean+sd),width = 0.1, position=position_dodge(0.9))


## Add lettering to error bars
ggplot(mean_data2, aes(x = dose, y = len_mean,group=supp))+
  geom_bar(position=position_dodge(0.9),stat = "identity", aes(fill = supp), show.legend = TRUE) + #barplot
  geom_errorbar(aes(ymin = len_mean-sd, ymax=len_mean+sd),width = 0.1, position=position_dodge(0.9)) + 
  geom_text(aes(label = group_lettering, y = len_mean + sd), vjust=-0.4, position=position_dodge(0.9)) #add letters



## Final Publication Ready group bar plot with TWO-Way ANOVA (Type-1)
#grouped barplot
p1 <- ggplot(mean_data2, aes(x = dose, y = len_mean,group=supp))  +
  geom_bar(position=position_dodge(0.9),stat = "identity", aes(fill = supp), show.legend = TRUE) + #barplot
  geom_errorbar(aes(ymin = len_mean-sd, ymax=len_mean+sd),width = 0.1, position=position_dodge(0.9)) + 
  geom_text(aes(label = group_lettering, y = len_mean + sd), vjust=-0.4, position=position_dodge(0.9)) + #add letters
  scale_fill_brewer(palette = "BrBG", direction = 1) + #theme setting
  labs(#this will add labels 
    x = "Dose",
    y = "Length (cm)",
    title = "Publication Ready Group Barplot",
    subtitle = "Made by #RwithAammar",
    fill = "Supp"
  ) +
  #facet_wrap(~supp)+
  ylim(0,35)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p1



# Publication ready grouped barplot with TWO-Way ANOVA with Facets (Type-2)
p2 <- ggplot(mean_data2, aes(x = dose, y = len_mean,group=supp))  +
  geom_bar(position=position_dodge(0.9),stat = "identity", aes(fill = supp), show.legend = TRUE) + #barplot
  geom_errorbar(aes(ymin = len_mean-sd, ymax=len_mean+sd),width = 0.1, position=position_dodge(0.9)) + 
  geom_text(aes(label = group_lettering, y = len_mean + sd), vjust=-0.4, position=position_dodge(0.9)) + #add letters
  scale_fill_brewer(palette = "BrBG", direction = 1) + #theme setting
  labs(#this will add labels 
    x = "Dose",
    y = "Length (cm)",
    title = "Publication Ready Group Barplot",
    subtitle = "Made by #RwithAammar",
    fill = "Supp"
  ) +
  facet_wrap(~supp)+
  ylim(0,35)+ #change your yaxis limits based on the letters
  ggthemes::theme_par(); p2



# 7- Saving upto 4K barplots in R
# First choose a working directory by pressing ctrl+shift+H and select a folder then run the following code to save in .tiff:
tiff('Barplot_G1.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')
p1
dev.off()
## png 
##   2
tiff('Barplot_G2.tiff', units="in", width=10, height=6, res=300, compression = 'lzw')
p2
dev.off()
## png 
##   2