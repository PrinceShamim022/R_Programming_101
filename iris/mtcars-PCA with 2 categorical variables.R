# PCA- Method 02 PCA with 2 categorical variables


View(mtcars)
names(mtcars)

df <- mtcars

#install.packages("permute")
#install.packages("lattice")
#install.packages("ggfortify")

# Required Libraries
library(vegan)
library(ggplot2)
library(factoextra)
library(ggrepel)
library(ggfortify)
library(dplyr)
library(tidyverse)


# Prearation for PCA and 0's
p <- df %>%
  select(,3:7)
  

pca <- prcomp(p, scale = TRUE, center = TRUE)
pca


# Apply "Hellinger" transformation
pca.h <- decostand(p, "hellinger")
pca <- prcomp(pca.h, scale = TRUE, center = TRUE)
pca

summary(pca)

# Eigenvalues
eig.val <- get_eigenvalue(pca)
eig.val


# Results for Variables
var <- get_pca_var(pca)
var


head(var$cos2)  # Quality of Representation = var.cood^2
head(var$contrib)  # Contribution to the PC's in% (var.cos2*100)/(Total cos2 of the component)
head(var$coord)   # coordinates = loading*component of SDs

# Results for Individuals
ind <- get_pca_ind(pca)
ind

head(ind$cos2)
head(ind$contrib)
head(ind$coord)

pca.ind.coord <- ind$coord
pca.ind.coord

str(pca)

pca$x

pcax <- cbind(p, pca$x[,1:2])
View(pcax)

# Screeplot
screeplot(pca, bstick = TRUE, type = "l", main = NULL)

# Manually: PC1 Represent about % Variation in the data
pca.var <- pca$sdev^2   # to see how much variation each PC account for
pca.var

pca.var.percent <- round(pca.var/sum(pca.var)*100, 1)   # converted to percentages
pca.var.percent  


# Bar Plot to show Percentage values
barplot(pca.var.percent, 
        main = "Scree Plot for PCA (Varination in Percentages",
        xlab = "Principal Component",
        ylab = "Percentage Variation")


# Customized With FVIZ
fviz_eig(pca)



# BiPlot
biplot(pca, scaling = "Symmetric")

# for more organized biplot
fviz_pca_biplot(pca, repel = TRUE,
                col.var = "#2E9FDF",  # variable color
                col.ind = "#696969")  # individual color

# Plot
plot(pca$x[,1],pca$x[,2])
# x contains the PCs for drawing a graph, we are specifying have to only draw
# first 2 PCs (Sometimes you may want to use PC2 & PC3, can check scree plot)


# Group similar individuals together
fviz_pca_ind(pca, col.ind = "cos2",  # color by Quantity of Representation
             gradient.cols = c("#00AFBB", "#E78880", "#FC4E07"),
             repel = TRUE)  # Avoid text overlapping


# Positive Correlated variables point to  the same side of the plot
# Negative Correlated variables point to  the opposite side of the plot


# Package ggplot2 and/or ggfortify
pca.df <- data.frame(pca$x)  # creates PCs as column, samples as rows

pca.df$plotx <- pca.df[,1]  # assigns first column as Plotx
pca.df$ploty <- pca.df[,2]  # assigns Second column as Ploty


# Adding Categorical variable/s in pca.df dataframe
Gear = as.character(df$gear)
pca.df$Gear <- Gear


# similar way we can add more categorical variables in pca.df dataframe below-
Carb = as.character(df$carb)
pca.df$Carb <- Carb

View(pca.df)


# Plot 
pca.plot <- ggplot(pca.df, aes(plotx, ploty, color = Carb)) +
  geom_point() +
  ggtitle("PCA with Carb Status") +
  xlab("PC1") + ylab("PC2") +
  coord_fixed(ratio = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1) +
  theme(panel.grid = element_blank()) +
  stat_ellipse(geom = "polygon", aes(fill = after_scale(alpha(color, 0.3))))

pca.plot

# when we want to remove a level from the categorical value
# if we don't want to see viriginica for example, than the plot will be-
pca.plot <- ggplot(pca.df, aes(plotx, ploty, color = Species, shape = Species)) +
  geom_point(size = 3, alpha = 0.5) +   # point Size & opacity
  ggtitle("PCA with Species Status") +
  xlab("PC1") + ylab("PC2") +
  coord_fixed(ratio = 1) +
  theme_bw() + 
  theme(aspect.ratio = 1) +
  theme(panel.grid = element_blank()) +
  stat_ellipse(geom = "polygon", aes(fill = after_scale(alpha(color, 0.3))),
               data = pca.df[pca.df$Species != "virginica",])

pca.plot


# Plot with Gear and Carb

pca.plot <- ggplot(pca.df, aes(plotx, ploty, 
                               color = Carb, label = Gear)) +
  geom_point() +
  ggtitle("PCA with Gear and Carb Status") +
  geom_text_repel(fontface = "bold", show.legend = FALSE) +  # minimize text overlapping
  xlab("PC1") + ylab("PC2") +
  coord_fixed(ratio = 2) +
  stat_ellipse(geom = "polygon", 
               aes(fill = after_scale(alpha(color, 0.3)))) +
  theme_bw(base_size = 3) +
  theme(panel.grid = element_blank()) +
  theme(aspect.ratio = 0.5)
  

pca.plot
