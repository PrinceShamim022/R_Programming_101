#2 one way anova with cld

install.packages("palmerpenguins")
install.packages("emmeans")
install.packages("multcomp")

library(multcompView)
library(emmeans)   # for calculate summary staistics
library(palmerpenguins)
head(penguins)
summary(penguins)
str(penguins)

model = aov(bill_length_mm~species,penguins) # dependent variable, df
summary(model)

#  Pr(>F)    <2e-16 ***
# sig > p-value , Reject th H0, 
# result: different penguins has different bill length

# pair-wise comparison (TukeyHSD)
TukeyHSD(model)

# $species
#                   diff       lwr     upr        p adj
# Chinstrap-Adelie 10.042433  9.024859 11.0600064 0.0000000
# Gentoo-Adelie     8.713487  7.867194  9.5597807 0.0000000
# Gentoo-Chinstrap -1.328945 -2.381868 -0.2760231 0.0088993

# adjusted p value is 0, less than sig value
# every pair is different from each others

# packages require for cld
library(emmeans)
library(multcomp)

# calculate summary statistics
emmean = emmeans(model, specs = "species") # specfication = species (categorical/dependent variable)
view(emmean)



# add cld to emmeans object
emmean_cld = cld(emmean,alpha=0.05,Letters=letters) # Letters=letters means a,b,c on bar
view(emmean_cld)

# plot barplot with errorbars and add cld
library(ggplot2)
ggplot(emmean_cld,aes(species,  emmean,label=.group))+ # label for group (a, b,c)
  geom_col(fill="steelblue")+   #bar and its color
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE),
                width=0.1, color = "darkblue", size = 0.1)+ #errorbar customization
  geom_text(vjust=-0.5)+  # text on bar, vjust for text movement upward~downward
  theme_classic()



#2 two way factorial anova with cld


library(emmeans)    # for calculate summary staistics
library(multcomp)    # for cld function
library(multcompView)


library(palmerpenguins)
head(penguins)
summary(penguins)
str(penguins)

# two way factorial anova

model2 = aov(bill_length_mm ~ species+sex, penguins)  
# aov(dependent variables, indpendent var1 + ind var2, df)
summary(model2)

# pair-wise comparison (TukeyHSD)
TukeyHSD(model2)

# result: bill_length_mm is significant difference in species and sex

# calculation for emmeans (arithmetic mean summary)

emmean <- emmeans(model2,specs = c("species","sex"))   # two categorical variables for 2 way anova
emmean_cld = cld(emmean,alpha=0.05,Letters=letters)  # Letters=letters means a,b,c on bar
emmean_cld

# create Steak bar plot for 2 way anova result (emmeans vs species, coloured by sex)
library(ggplot2)
ggplot(emmean_cld,aes(species, emmean,label=.group, fill=sex))+
  geom_col(position="dodge")+
  geom_errorbar(aes(ymin= emmean-SE, ymax= emmean+SE),
                position = position_dodge(width=0.9),
                width=0.1, size=0.7)+
  geom_text(vjust=-0.5,
            position = position_dodge(width=0.9))+   # position dodge side by side bar
  theme_classic()





#3 three way factorial anova with cld

# compact letter display using ggplot2
library(palmerpenguins)
library(emmeans)
library(multcomp)

# convert to data frame (bcs in 3 way anova doesnt support raw data)

penguins= as.data.frame(penguins)
head(penguins)

model3 = aov(flipper_length_mm~species+ island+sex, penguins)
# aov(dependent variables, indpendent var1 + ind var2 + ind var3, df)
summary(model3)

emmean = emmeans(model3,specs = c("species","island","sex"))

emmean_cld = cld(emmean,Letters=letters)
emmean_cld

library(ggplot2)
library(ggpattern)   # for 3rd variable to show

p <- ggplot(emmean_cld,aes(species,emmean,fill=island,
                          pattern=sex,label=.group))+
  geom_col_pattern(position="dodge", alpha=0.5, color="steelblue",
                   pattern_fill="black", 
                   pattern_density=0.1,
                   pattern_angle=45,
                   pattern_spacing=0.025,
                   pattern_key_scale_factor=0.6)+
  geom_errorbar(aes(ymin=emmean-SE, ymax=emmean+SE), 
                width=0.2, 
                size=0.75,
                position=position_dodge(width=0.9))+
  geom_text(position=position_dodge2(width=0.9),
            vjust=-0.5,size=4)+
  theme_classic()+
  theme(legend.position = "bottom")

ggsave("Three way anova with cld.png",p, 
       width=9, height=5, units="in")

# with theme pubr
install.packages("pubr")
library(pubr)
p1 <- ggplot(emmean_cld,aes(species,emmean,fill=island,
                          pattern=sex, label=.group))+
  geom_col_pattern(alpha=0.5,color="grey20",
                   position = "dodge",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6)+
  #scale_pattern_manual
  (values = c(Nerd = "stripe", NotNerd = "none"))+
  geom_errorbar(aes(ymin=emmean-SE,ymax=emmean+SE), 
                position = position_dodge(width=0.9), width=0.3,size=0.5)+
  geom_text(position = position_dodge2(width=0.9),vjust=-0.5,size=4)+
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  theme_pubr()

ggsave("AOV_plot.png",p1,
       width=7,height=4,units="in")

