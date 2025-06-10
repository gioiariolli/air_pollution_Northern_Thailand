install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
library(FactoMineR)
library(ggcorrplot)
library(corrr)
library(dplyr)
library(factoextra)

data <- read.csv2("~/Desktop/Progetto Thailandia/R scripts/Norm.csv")
head(data)

data_Sel <- data %>% select (!c("PM", "OC1", "OC2", "OC3", "OC4", "OP", "EC2", "EC1.OP", "Val..RESIDUI"))

# Normalizing the data
dataNorm <- scale(data_Sel)
head(dataNorm)

# Conducting PCA analysis on the normalized data
data.PCA <- princomp(dataNorm)
summary(data.PCA)
data.PCA$loadings[, 1:10]

# Recall of the package factoextra, used to visualize eigenvector
# THE FIRST FUNCTION fviz_eig() is used to visualize the importance of each factor
fviz_eig(data.PCA, addlabels = TRUE)

# THE SECOND FUNCTION fviz_pca_var() is used to visualize 
# the similarities and dissimilarities between the components of PM 2.5
# It further shows the impact of each attribute on each of the principal components.
fviz_pca_var(data.PCA, col.var = "black")

# How to read the graphic? 
# The closer the components, the releated between each other they are
# The longer the arrow, the better their representation
# Opposite sides variables, are negatively correlated

# THE THIRD FUNCTION fviz_cos2() is to determine how much each variable is represented in a given component
# high value = good representation
# low value = bad representation

fviz_cos2(data.PCA, choice = "var", axes = 1:2)

# THE FOURTH FUNCTION fviz_pca_var() is a combination between biplot and cos2 scores:
# same colours are closer components
fviz_pca_var(data.PCA, col.var = "cos2",
             gradient.cols = c("black", "red", "orange", "darkgreen"),
             repel = TRUE)

# TO OBTAIN RESULTS we run the following codes
# Eigenvalues
eig.val <- get_eigenvalue(data.PCA)
eig.val

# Results for Variables
res.var <- get_pca_var(data.PCA)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(data.PCA)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2       
