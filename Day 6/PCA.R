##################################################
## Project: R Training
## Script purpose: Principal Component Analyis
## Author: Ege Ulgen
##################################################
# install.packages("factoextra")
# install.packages("car")
library(factoextra)
library(car)

# Example 1 ---------------------------------------------------------------
?iris

dim(iris)
head(iris)

### compute PCA
res.pca <- prcomp(iris[, -5])

## Visualize eigenvalues (scree plot) - the percentage of variances explained by each principal component
plot(res.pca)
fviz_eig(res.pca)

### Graph of individual iris samples. Samples with a similar profile are grouped together
plot(res.pca$x[, 1], res.pca$x[, 2], xlab = "PC1", ylab = "PC2")

fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

### Graph of variables. Positive correlated variables point to the same side of 
# the plot. Negative correlated variables point to opposite sides of the graph.
plot(res.pca$rotation[, 1], res.pca$rotation[, 2], xlab = "PC1", ylab = "PC2")

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)     # Avoid text overlapping

### Biplot of individuals and variables
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969")  # Individuals color

fviz_pca_ind(res.pca, label = "none", 
             habillage = iris$Species,
             addEllipses = TRUE, ellipse.level = 0.95, 
             palette = "Dark2")

#### Predict the class of a new sample using PCA
new_data <- data.frame(Sepal.Length = 5.2,
                       Sepal.Width = 3.6,
                       Petal.Length = 1.3,
                       Petal.Width = 0.3)

ind.sup.coord <- predict(res.pca, newdata = new_data)
ind.sup.coord

# Plot of active samples
p <- fviz_pca_ind(res.pca, label = "none", habillage = iris$Species,
                  addEllipses = TRUE, ellipse.level = 0.95, palette = "Dark2")
# Add supplementary sample
fviz_add(p, ind.sup.coord, color = "red", shape = 19)

scatter3d(x = res.pca$x[, 1], y = res.pca$x[, 2], z = res.pca$x[, 3], 
          groups = as.factor(iris$Species),
          id = TRUE, surface = FALSE,
          xlab = "PC1", ylab = "PC2", zlab = "PC3")

# Example 2 ---------------------------------------------------------------
# install.packages("BiocManager")
# BiocManager::install("GEOquery")
library(GEOquery)

gset <- getGEO("GSE57297", GSEMatrix =TRUE, getGPL=FALSE)
gset <- gset[[1]]

exprs(gset)[1:5, 1:5]
head(pData(gset))

exp_mat <- exprs(gset)
pheno_df <- pData(gset)

table(pheno_df$`cancer subtype:ch1`)


res.pca2 <- prcomp(t(exp_mat))
fviz_eig(res.pca2, ncp = 20)

my_cols <- c("gray60", "red", "orange", "blue")
names(my_cols) <- c("--", "Luminal A", "Luminal B", "Triple Negative")
my_cols <- my_cols[match(pheno_df$`cancer subtype:ch1`, names(my_cols))]

my_pch <- c(3, 4, 5, 6)
names(my_pch) <- c("--", "Luminal A", "Luminal B", "Triple Negative")
my_pch <- my_pch[match(pheno_df$`cancer subtype:ch1`, names(my_pch))]

plot(res.pca2$x[, 1],  res.pca2$x[, 2], 
     col = my_cols, pch = my_pch, xlab = "PC1", ylab = "PC2")


scatter3d(x = res.pca2$x[, 1], y = res.pca2$x[, 2], z = res.pca2$x[, 3], 
          groups = as.factor(pheno_df$`cancer subtype:ch1`),
          id = TRUE, surface = FALSE,
          xlab = "PC1", ylab = "PC2", zlab = "PC3")
