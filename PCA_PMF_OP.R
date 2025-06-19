#--------------------------------------------------------------------------------
# Packages installing and library usage
install.packages("corrr")
install.packages("ggcorrplot")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("Hmisc")
library(FactoMineR)
library(ggcorrplot)
library(corrr)
library(dplyr)
library(factoextra)
library(Hmisc)

#-------------------------------------------------------------------------------- 
# Data import and selection

PMF_OP_2020 <- read.csv2("~/Desktop/Progetto Thailandia/PMF_OP_2020.csv")
PMF_OP_2021 <- read.csv2("~/Desktop/Progetto Thailandia/PMF_OP_2021.csv")
head(PMF_OP_2020)
head(PMF_OP_2021)

data_Sel_2 <- PMF_OP_2020 %>% select (!c("Date","Rain", "Rain.x.3", "Temp", "RH", "Solar.in.hour.", 
                                "max.wind.speed..km.hr.", "Cl.", "NO3.", "SO42.",
                                "Na.", "NH4.", "K.", "Ca2.", "Mg2.", "Formate", "Succinate",
                                "Malonate", "Oxalate", "Arabitol", "Mannitol",
                                "Glucose", "Levoglucosan", "Ba", "Ca","Cu", "K",  "Mg", "Mn", "Na",
                                "Pb", "Sr", "Zn", "OC1", "OC2", "OC3", "OC4", "OP", "EC1.OP", 
                                "EC2", "POC","SOC",  "TOC", "TEC"))

data_Sel_3 <- PMF_OP %>% select (!c("Date","Rain", "Rain.x.3", "Temp", "RH", "Solar.in.hour.", 
                                         "max.wind.speed..km.hr.", "Cl.", "NO3.", "SO42.",
                                         "Na.", "NH4.", "K.", "Ca2.", "Mg2.", "Formate", "Succinate",
                                         "Malonate", "Oxalate", "Arabitol", "Mannitol",
                                         "Glucose", "Levoglucosan", "Ba", "Ca","Cu", "K",  "Mg", "Mn", "Na",
                                         "Pb", "Sr", "Zn", "OC1", "OC2", "OC3", "OC4", "OP", "EC1.OP", 
                                         "EC2", "POC","SOC",  "TOC", "TEC"))

data_Sel_21 <- PMF_OP_2021 %>% select (!c("Date","Rain", "Rain.x.3", "Temp", "RH", "Solar.in.hour.", 
                                          "max.wind.speed..km.hr.", "Cl.", "NO3.", "SO42.",
                                          "Na.", "NH4.", "K.", "Ca2.", "Mg2.", "Formate", "Succinate",
                                          "Malonate", "Oxalate", "Arabitol", "Mannitol",
                                          "Glucose", "Levoglucosan", "Ba", "Ca","Cu", "K",  "Mg", "Mn", "Na",
                                          "Pb", "Sr", "Zn", "OC1", "OC2", "OC3", "OC4", "OP", "EC1.OP", 
                                          "EC2", "POC","SOC",  "TOC", "TEC"))

data_Sel_sm <- PMF_OP_sm %>% select (!c("Date","Rain", "Rain.x.3", "Temp", "RH", "Solar.in.hour.", 
                                         "max.wind.speed..km.hr.", "Cl.", "NO3.", "SO42.",
                                         "Na.", "NH4.", "K.", "Ca2.", "Mg2.", "Formate", "Succinate",
                                         "Malonate", "Oxalate", "Arabitol", "Mannitol",
                                         "Glucose", "Levoglucosan", "Ba", "Ca","Cu", "K",  "Mg", "Mn", "Na",
                                         "Pb", "Sr", "Zn", "OC1", "OC2", "OC3", "OC4", "OP", "EC1.OP", 
                                         "EC2", "POC","SOC",  "TOC", "TEC"))
data_Sel_nsm <- PMF_OP_nsm %>% select (!c("Date","Rain", "Rain.x.3", "Temp", "RH", "Solar.in.hour.", 
                                         "max.wind.speed..km.hr.", "Cl.", "NO3.", "SO42.",
                                         "Na.", "NH4.", "K.", "Ca2.", "Mg2.", "Formate", "Succinate",
                                         "Malonate", "Oxalate", "Arabitol", "Mannitol",
                                         "Glucose", "Levoglucosan", "Ba", "Ca","Cu", "K",  "Mg", "Mn", "Na",
                                         "Pb", "Sr", "Zn", "OC1", "OC2", "OC3", "OC4", "OP", "EC1.OP", 
                                         "EC2", "POC","SOC",  "TOC", "TEC"))

#--------------------------------------------------------------------------------
# Correlation Matrix for 2020 dataset
rcorr(dataNorm, type = c("pearson", "spearman"))

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



#--------------------------------------------------------------------------------
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



#--------------------------------------------------------------------------------
# Same process for 2021
# Correlation Matrix
rcorr(dataNorm_21, type = c("pearson", "spearman"))

# Normalizing the data
dataNorm_21 <- scale(data_Sel_21)
head(dataNorm)

# Conducting PCA analysis on the normalized data
data.PCA.21 <- princomp(dataNorm_21)
summary(data.PCA.21)
data.PCA.21$loadings[, 1:10]

fviz_eig(data.PCA.21, addlabels = TRUE)

fviz_pca_var(data.PCA.21, col.var = "black")

fviz_cos2(data.PCA.21, choice = "var", axes = 1:2)

fviz_pca_var(data.PCA.21, col.var = "cos2",
             gradient.cols = c("black", "red", "orange", "darkgreen"),
             repel = TRUE)

#--------------------------------------------------------------------------------
# Same process for the full dataset 2020 2021
# Correlation Matrix
rcorr(dataNorm_3, type = c("pearson", "spearman"))

# Normalizing the data
dataNorm_3 <- scale(data_Sel_3)
head(dataNorm)

# Conducting PCA analysis on the normalized data
data.PCA.3 <- princomp(dataNorm_3)
summary(data.PCA.3)
data.PCA.3$loadings[, 1:10]

fviz_eig(data.PCA.3, addlabels = TRUE)

fviz_pca_var(data.PCA.3, col.var = "black")

fviz_cos2(data.PCA.3, choice = "var", axes = 1:2)

fviz_pca_var(data.PCA.3, col.var = "cos2",
             gradient.cols = c("black", "red", "orange", "darkgreen"),
             repel = TRUE)

#--------------------------------------------------------------------------------
# Only with smoke haze
# Correlation Matrix
rcorr(dataNorm_sm, type = c("pearson", "spearman"))

# Normalizing the data
dataNorm_sm <- scale(data_Sel_sm)
head(dataNorm)

# Conducting PCA analysis on the normalized data
data.PCA.sm <- princomp(dataNorm_sm)
summary(data.PCA.sm)
data.PCA.sm$loadings[, 1:10]

fviz_eig(data.PCA.sm, addlabels = TRUE)

fviz_pca_var(data.PCA.sm, col.var = "black")

fviz_cos2(data.PCA.sm, choice = "var", axes = 1:2)

fviz_pca_var(data.PCA.sm, col.var = "cos2",
             gradient.cols = c("black", "red", "orange", "darkgreen"),
             repel = TRUE)

#--------------------------------------------------------------------------------
# Only with non-smoke haze data
# Correlation Matrix
rcorr(dataNorm_nsm, type = c("pearson", "spearman"))

# Normalizing the data
dataNorm_nsm <- scale(data_Sel_nsm)
head(datansm)

# Conducting PCA analysis on the normalized data
data.PCA.nsm <- princomp(dataNorm_nsm)
summary(data.PCA.nsm)
data.PCA.nsm$loadings[, 1:10]

fviz_eig(data.PCA.nsm, addlabels = TRUE)

fviz_pca_var(data.PCA.nsm, col.var = "black")

fviz_cos2(data.PCA.nsm, choice = "var", axes = 1:2)

fviz_pca_var(data.PCA.nsm, col.var = "cos2",
             gradient.cols = c("black", "red", "orange", "darkgreen"),
             repel = TRUE)

