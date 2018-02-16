library(caret)
library(tidyverse)
library(scales)
library(cluster)
library(factoextra)
library(NbClust)
#iris----------
#1. get label
iris.label <- iris$Species
#pre-processing standardize 
iris.preProcValues <- preProcess(iris[,1:4], method = c("center", "scale"))
iris.standard <- predict(preProcValues, iris[,1:4])

#pre-processing normalize
normalize <- function(x){
    y <- (x - min(x))/(max(x)-min(x))
    return(y)
}
iris.normal <- apply(iris[,1:4], 2, normalize)
#train without rescaling
n_clusters <- 1:8
md.iris <- list()
for(i in seq_along(n_clusters)){
    md.iris [[i]]<- kmeans(x = iris[,1:4],centers = n_clusters[i])
}

#train with standardize
n_clusters <- 1:8
md.iris.standard <- list()
for(i in seq_along(n_clusters)){
    md.iris.standard [[i]]<- kmeans(x = iris.standard[,1:4],centers = n_clusters[i])
}


#train with normalize
n_clusters <- 1:8
md.iris.normalize <- list()
for(i in seq_along(n_clusters)){
    md.iris.normalize [[i]]<- kmeans(x = iris.normal,centers = n_clusters[i])
}

#elbow method
k <- map_dbl(md.iris.standard,~ length(.x$size))
pct_explained <- map_dbl(md.iris.standard,~.x$betweenss/.x$totss)
elbow_data <- tibble(k = k, pct_explained = pct_explained)
ggplot(elbow_data,mapping = aes(x = k, y = pct_explained)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(labels = percent)
#silhouetee method


#silhouette plot