


## Reshape datasets
## 2-1) Import dataset & libraries
insurance <- read.csv("insurance.csv")
options(warn=-1)

str(insurance)


## 2-2) Remove ‘region’ columns
insurance$region <- NULL

## 2-3) Convert Factor to Numeric
ins <- insurance

#ins$sex <- as.numeric(ins$sex)
#ins$smoker <- as.numeric(ins$smoker)

# convert binary/charater to numeric
ins$sex <- ifelse(ins$sex == "female", 1, 2)
ins$smoker <- ifelse(ins$smoker == "yes", 1, 2)

str(ins)


## 2-4) Add columns to distinguish between low charges and high charges (standard : mean(charges))
ins$group <- ifelse(ins$charges > mean(ins$charges), "high", "low")


## 2-5) Check the Correlation Coefficient (except: group)
cor(ins[-7])


## 3. Visualize (To see the total corr, you have to remove group columns : ins[-7])
# 3-1) Using chart.Correlation
library(PerformanceAnalytics)

chart.Correlation(ins[-7], histogram=TRUE, pch=1, 
                  main="Insurance Scatterplot Matrix")


## 3-2) Using pairs.pannels
library(psych)
pairs.panels(ins[-7], pch=1, lm=TRUE, cex.cor=1, 
             smoother=F, stars = T, main="Insurance Scatterplot Matrix")


## 3-3) Using ggpairs
library(GGally)
ggpairs(ins[-7])

## 3-4) Visualize including group (View in different colors according to group)
library(ggplot2)

ggpairs(ins, aes(color=group, alpha=0.75), 
        lower=list(continuous="smooth"))+ 
  theme_bw()



## 4. Full Analysis
# 4-1) The value(columns) that most influences charges
cor(ins[-7])[,"charges"] # correlation coefficient

ggcorr(ins[-7], name = "corr", label = TRUE)+
  theme(legend.position="none")


## Biplot
library("factoextra")
my_data <- ins[, c(1,3,4,5)];

res.pca <- prcomp(my_data, scale = TRUE)

fviz_pca_biplot(res.pca, col.ind = ins$group, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Charges")


fviz_pca_biplot(res.pca, geom.ind = "point", col.ind = ins$group, 
                pointsize = 2, palette = "jco", addEllipses = TRUE, 
                label="var", col.var="black", repel=TRUE,
                legend.title="Charges")


## 5. Detailed Analysis
# 5-1) How do you make a model if you want to give a higher penalty to an obese&smoke person?
# If ‘bmi’ exceeds 30, Let’s categorized as obesity

insurance$obese <- as.factor(ifelse(ins$bmi >=30, "yes", "no"))


# 5-2) Combine multiple variance and predict the best combination (to help raise the premiums)
lm(charges ~ obese * smoker, data = insurance)


# 7. Prediction Function
pre_charges <- function(m, a, b, c){
pre_new <- predict(m, data.frame(age = a, bmi = b, children = c))
msg <- paste("age: ",a,", bmi: ",b,", children: ",c,"   ==> Expect Charges: $",round(pre_new),sep="")
print(msg) 
}


model <- lm(charges ~ age + bmi + children, data = insurance)

pre_charges(model, 19, 27.9, 0)
