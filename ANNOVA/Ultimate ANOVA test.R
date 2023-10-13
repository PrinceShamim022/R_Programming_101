#Factorial ANOVA with interaction analysis

#1 Data visualization 
#2 Assumption 1. Normality (of residuals)
#3 Assumption 2. Homogeneity of variances: Levene's test; Bartlett's test 
#4 Factorial ANOVA: one-way, two-way, three-way ANOVA
#5 Model fit: AIC, BIC, AICwt, and BICwt
#6 Effect size: (Partial) eta^2, omega^2, epsilon^2
#7 Post-hoc analysis: "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"
#8 visualize post-hoc results with confidence intervals

##############################################
### Data visualization 
df <- read.csv("ANOVA.csv")
head(df) 
library(ggplot2)
#Density plots
P1 <- ggplot(df, aes(x=Grade, color = SES2, fill=SES2)) +
  geom_density(alpha = 0.7)
P1

P2 <- ggplot(df, aes(x=Grade, color = Gen2, fill=Gen2)) +
  geom_density(alpha = 0.4) 
P2

P3 <- ggplot(df, aes(x=Grade, color = FirstLang2, fill=FirstLang2)) +
  geom_density(alpha = 0.1)
P3

#Boxplots
library(ggpubr)
P4 <- ggboxplot(df, "SES2", "Grade",
                fill = "SES2", palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                add = "jitter")
P4
P5 <- ggboxplot(df, "SES2", "Grade",
                fill = "SES2", palette = get_palette("Pastel2", 3),
                add = "jitter")
P5

ggarrange(P1, P2, P3, P4, P5 + rremove("x.text"), 
          labels = c("A", "B", "C", "D", "E"), 
          ncol = 2, nrow = 3, 
          common.legend = TRUE, 
          legend = "bottom")

#Factorial ANOVA

## Assumptions
# 1. Normality
library(psych)
describe(df)

mod <- aov(Grade ~ SES2, data = df)  # depedent variable ~ Independent variable

mod

# par(mfrow = c(1, 2)) # combine plots

# histogram
hist(mod$residuals)

# QQ-plot
library(car)
QQ<- qqPlot(res_aov$residuals, id = FALSE) # id = FALSE to remove point identification

shapiro.test(mod$residuals) #test normality of the residuals with the Shapiro-Wilk test

# 2. Homogeneity of variances: Levene's test & Bartlett's test (sphericity)
leveneTest(Grade ~ Gen2, data = df) #one variable
# Homogeneity of variance is not violated across gender (probability = 0.297)

leveneTest(Grade ~ Gen2*FirstLang2*SES2, data = df) #multiple variables
# Homogeneity of variance is violated across all variables (probability = 1.119e-05)
#Levene's test is less sensitive to deviations from normality.

bartlett.test(Grade ~ Gen2, data = df) #one variable   # no violance
bartlett.test(Grade ~ interaction(Gen2,FirstLang2,SES2), data = df) #multiple variable
# violanced


# in this situation, we have 2 ways
# 1. run ANOVA test
# 2. Non Parametric test

#ANOVA tests: one-way, two-way, etc.
one.way <- aov(Grade ~ SES2, data = df)
summary(one.way)
# probability <2e-16
# there's significant difference among 3 groups (Grade) 
# but sum sq of Residuals is higher than sum sq of SES2, it's not good

two.way <- aov(Grade ~ SES2 + Gen2, data = df)
summary(two.way)

#  sum sq of Residuals is still higher
# but signficantly reduced from one way anova, that's good

three.way <- aov(Grade ~ SES2 + Gen2 + FirstLang2, data = df)
summary(three.way)
# sum sq of Residuals IS signficantly reduced from one way anova, that's good!!
# if we add more variables, the sum sq of residuals will decrease, that a very good indication

interaction <- aov(Grade ~ SES2 + Gen2 + FirstLang2 + Gen2*FirstLang2, data = df)
# you can add more interaction like above, it's just a sample
summary(interaction)

#Model fit
install.packages("AICcmodavg")
library(AICcmodavg)
model.set <- list(one.way, two.way, three.way, interaction)
model.names <- c("one.way", "two.way", "three.way", "interaction")

# AIC Test
aictab(model.set, modnames = model.names, , sort = TRUE) 
# sort= TRUE, arrange value in descending (a to z) order
# the smaller the AICc is, the better for the model fit
# here Interaction model has low value (783.58) best model!
# the higher AICcWt the better the model is
# if any model AICcWt is less than 0.05, that model must be rejected
#here, last 3 model are less than 0.05!
# The smaller the "LL" value the better the Model!

bictab(model.set, modnames = model.names, , sort = TRUE)
# sort= TRUE, arrange value in descending (a to z) order
# the smaller the BICc is, the better for the model fit
# the higher BICcWt the better the model is!
# The smaller the "LL" value the better the Model!

#BICwt / AICwt (the BIC/AIC weights): The best  models receive the highest wt.
#BICwt add up to 1 (are normalized). Models with wt<0.05 to be neglected.

# Effect size
library(effectsize)
eta_squared(interaction, partial = TRUE)   # for best model, here interaction is best
eta_squared(interaction, partial = FALSE)
omega_squared(interaction, partial = TRUE)
epsilon_squared(interaction, partial = TRUE)

# Post-hoc analysis 

tukey.interaction <- TukeyHSD(interaction) 
tukey.interaction

tukey.plot.aov <- aov(Grade ~ Gen2:FirstLang2, data = df)
tukey.plot.test <- TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 2)

install.packages("DescTools")
library(DescTools)
ScheffeTest(interaction)

hsd <- PostHocTest(interaction, which = NULL,
                   method = c("hsd"),
                   conf.level = 0.95, ordered = FALSE)
hsd

#other test are "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan"

plot(hsd, las = 2) #plot 
