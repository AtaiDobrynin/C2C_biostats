

library(factoextra)
library(car)

##################################################

#To demonstrate PCA we'll use a dataset called `bioenv.txt' from a book called "Biplots in Practice" (M. Greenacre, 2010). 
#The context is in marine biology and the data consist of two sets of variables observed at the same locations on the sea-bed: 
#the first is a set of biological variables, the counts of five groups of species, and the second is a set of four environmental variables.
bioenv <- read.delim('https://github.com/Bio723-class/example-datasets/raw/master/bioenv.txt')
names(bioenv)
#Notice that the first column got assigned the generic name This is because there is a missing column header in the bioenv.txt file. 
colnames(bioenv)[1] <- "Site" 

head(bioenv)

#The columns labeled a to e contain the counts of the five species at each site, 
#while the remaining columns give additional information about the physical properties of each sampling site. 
#For our purposes today we'll confine our attention to the abundance data

abundance <- bioenv[, c("Site", "a", "b", "c", "d", "e")]
  
head(abundance)

#The data is currently in a "wide" format. For the purposes of plotting 
#it will be more convenient to generate a "long" version of the data using functions from the tidyr library
long.abundance <- reshape2::melt(abundance)
head(long.abundance)
colnames(long.abundance) <- c("Site", "Species", "Count")

boxplot(Count~Species, data = long.abundance, xlab = "Species", ylab = "Count",
        main = "Distribution of\nSpecies Counts per Site")


#From the boxplot it looks like the counts for species `e' are smaller on average, and less variable. 
#The means confirm that.
summary(abundance)

#A correlation matrix suggests weak to moderate associations between the variables:
abundance.only <- abundance[, -1]
cor(abundance.only)

# PCA of the Bioenv dataset ---------------------------------------------------------------
#Linearity is not a requirement for PCA, as it's simply a rigid rotation of the original data.
?prcomp
abundance.pca <- prcomp(abundance.only)
summary(abundance.pca)

abundance.pca

# PC Score Plots ---------------------------------------------------------------
##### 2 PCs
fviz_pca_ind(abundance.pca, label = "none", habillage = bioenv$Sediment,
             addEllipses = TRUE, ellipse.level = 0.95, palette = "Dark2")

abundance.pca$rotation

##### 3 PCs
pca.scores.df <- as.data.frame(abundance.pca$x)
head(pca.scores.df)

## we'll color according to "Sediment"
table(bioenv$Sediment)

my_cols <- ifelse(bioenv$Sediment == "C", "red", 
                  ifelse(bioenv$Sediment == "G", "blue", "orange"))

max(c(pca.scores.df$PC1, pca.scores.df$PC2, pca.scores.df$PC3))

par(mfrow = c(2, 2))
plot(pca.scores.df$PC1, pca.scores.df$PC2, xlab = "PC1", ylab = "PC2", 
     xlim = c(-30, 30), ylim = c(-30, 30), col = my_cols)
plot(pca.scores.df$PC1, pca.scores.df$PC3, xlab = "PC1", ylab = "PC3", 
     xlim = c(-30, 30), ylim = c(-30, 30), col = my_cols)
plot(pca.scores.df$PC2, pca.scores.df$PC3, xlab = "PC2", ylab = "PC3", 
     xlim = c(-30, 30), ylim = c(-30, 30), col = my_cols)
par(mfrow = c(1,1)) # reset layout

scatter3d(x = pca.scores.df$PC1, y = pca.scores.df$PC2, z = pca.scores.df$PC3, 
          id = TRUE, groups = as.factor(bioenv$Sediment), surface = FALSE,
          xlab = "PC1", ylab = "PC2", zlab = "PC3")
