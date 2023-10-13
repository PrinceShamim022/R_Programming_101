# The codes 
library("klaR")
library("psych")
library("MASS")
library("ggord")
# Enable the r-universe repo
options(repos = c(
  fawda123 = 'https://fawda123.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
# Install ggord
# install.packages('ggord')

#################################
install.packages("devtools")
library("devtools")
library(ggplot2)

# load Iris dataset
df <- iris
head(df)
View(df)


#DATA PARTITION
#Use 75% of dataset as training set and remaining 25% as testing set
ldaIndex <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, 
                   prob=c(0.75,0.25))

TrainingSet <- df[ldaIndex, ] # Training Set
TestingSet <- df[-ldaIndex, ] # Test Set

#Fit the LDA Model
model <- lda(Species~., data = TrainingSet)

#view model output
model

# Equation of Coefficients of linear discriminants
# LD1 = 0.86 * Sepal.Length + 1.80 * Sepal.Width - 2.13 * Petal.Length - 2.93 * Petal.Width 
# LD2 = -0.17 * Sepal.Length - 1.99 * Sepal.Width + 1.10 * Petal.Length - 3.03 * Petal.Width

#use LDA model to make predictions on test data
predicted <- predict(model, TestingSet)

#view predicted class
names(predicted)

#view linear discriminants
print(predicted$x)
print(predicted$class)

#view posterior probabilities 
print(predicted$posterior)

#find accuracy of model
mean(predicted$class == TrainingSet$Species)
# 0.4362416 means this model is 43% is correct

#Visualize the Results
LDAplot <- ggord(model, TrainingSet$Species, 
        ylim = c(-5, 5),size = 1,
        repel = TRUE,txt = 4,
        grp_title = "Linear Discriminant Analysis")
  LDAplot+theme_classic()