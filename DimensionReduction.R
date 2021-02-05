#Dimension reduction

install.packages("factoextra")
install.packages("gridExtra")
install.packages("tidyverse")
install.packages("ggplot2")

library(factoextra)
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(reshape2)
library(corrplot)
library(gridExtra)
library(grid)


setwd("D:\\R and R Studio\\Dimension Reduction\\DimensionReduction")
getwd()

data1 <- read.csv("cardio_train.csv", sep = ";")

View(data1)

head(data1)

summary(data1)


corr_data = cor(data1,method='pearson')
corrplot(corr_data)


cor.matrix <- cor(data1, method = c("spearman"))
corrplot(cor.matrix, type = "lower", order = "hclust", tl.col = "black", tl.cex = 0.5)


data.pca <- prcomp(data1, center=TRUE, scale=TRUE)
eigen(cor(data1))$values



var <- get_pca_var(data.pca)
a<-fviz_contrib(data.pca, "var",axes = 2)s
b<-fviz_contrib(data.pca, "var",axes = 3)
c<-fviz_contrib(data.pca, "var",axes = 4)
grid.arrange(a,b,c,top='Contribution to the Principal Components')


fviz_eig(data.pca, choice='eigenvalue')


fviz_eig(data.pca)


summary(data.pca)


fviz_pca_var(data.pca, col.ind = "Age")



View(fviz_pca_var(data.pca, choice='eigenvalue'))


