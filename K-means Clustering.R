
## K-means Clustering

# The ItalianWineSamples.csv datasetwill be used, 
# which contains 13 chemical measurements on 178 Italian wine samples. 
# More information about the data can be found here: https://archive.ics.uci.edu/ml/datasets/wine

# In this dataset, the samples come from three wine varietals, 
# as indicated by the “Type” variable. I remove this variable first.

# I apply K-Means Clustering to the wine samples. 
# First, I determine the optimal number of clusters with the Elbow Method and 
# Average Silhouette Method, and then perform the K-Means Clustering using 
# the optimal K I have just identified. 

# Finally, I compare my K-Means Clustering solution with the “Type” variable, 
# T see how well did my K-Means Clustering solution match the actual wine 
# varietal contained in the “Type” variable.

wine <- read.csv("ItalianWineSamples.csv", header = TRUE)
str(wine)

#Remove Type
wine_new <- wine[,2:14]
str(wine_new)


#Check missing data
any(is.na(wine_new))

#Scale all variables
wine_new_scale <- scale(wine_new)

#Finding the optimal clusters
wss <- function(k,data){
  kmeans(data, center = k,nstart = 25)$tot.withinss
}
wss_wine <- sapply(2:10, wss, data = wine_new_scale)
wss_wine

#Use average Silhouette method:

library("cluster")
avg.sil <- function(k, data) {                   
  kmeans_result <- kmeans(data, k, nstart = 25) 
  silhouette_coefficient <- silhouette(kmeans_result$cluster, dist(data))
  mean(silhouette_coefficient[, 3])
}

average_silhouette <- sapply(2:10, avg.sil, data = wine_new_scale)
average_silhouette


#Arrange the plot for 2 graphs
library(ggplot2)
library(gridExtra)
df_wss_wine <- data.frame(wss_wine)
df_wss_wine$k <- c(2:10)
df_sil_wine <- data.frame(average_silhouette)
df_sil_wine$k <- c(2:10)
elbow_plot <- ggplot(df_wss_wine, aes(k, wss_wine)) + geom_line() + geom_point() + labs(x = "Number of clusters K", y = "Total within-clusters sum of squares") + theme_classic()
elbow_plot
sil_plot <- ggplot(df_sil_wine, aes(k,average_silhouette)) + geom_point() + geom_line() + labs(x = "Number of clusters K", y = "Average Silhouette") + theme_classic()
sil_plot

grid.arrange(elbow_plot, sil_plot, ncol = 2)


#Base on the graphs of 2 method (elbow point and highest average silhouette), we conclude that the dataset should be split into 3 clusters.
wine_cluster <- wine_new
wine_cluster$cluster <- kmeans(wine_new_scale, centers = 3, nstart = 25)$cluster

table(wine_cluster$cluster)
#There are 51 observation in Cluster 1, 62 observations in cluster 2 and 65 observations in cluster 3

head(wine_cluster)
wine_cluster
wine$Type


#use a dataframe to see our result comparing with defined Type

df_difference <- data.frame(wine$Type, wine_cluster$cluster)
df_difference

table(df_difference)
#as indicated in table:
#our Cluster 1 consits 3 Type-2 wines and 48 Type-3 wines
#our Cluster 2 consits 59 Type-1 wines and 3 Type-2 wines
#our Cluster 3 consits 52 Type-2 wines
#In total, we cluster differently only 6 observations in comparsion with defined Type

