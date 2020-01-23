
## Hierachical Clustering

# The EUProteinConsumption.csv dataset contains 25 European countries 
# and their protein intakes (in percentage) from 9 major food sources. 
# It is of interest to learn whether the 25 countries can be separated 
# into a smaller number of clusters.

# First, Hierarchical Clustering will be applied to this dataset using
# the four linkage methods and I draw the dendrogram for each linkage method.
# Next, I choose the appropriate linkage method and determine the optimal 
# number of clusters for the chosen one.

prot_cons <- read.csv("EUProteinConsumption.csv", header = TRUE, row.names = 1)
str(prot_cons)
is.na(prot_cons)
any(is.na(prot_cons))


prot_cons_scaled <- scale(prot_cons)
head(prot_cons_scaled)

d_prot <- dist(prot_cons_scaled, method = "euclidean")  ## Euclidean Distance is used by default

prot_hcl_complete <- hclust(d_prot, method = "complete")

prot_hcl_single <- hclust(d_prot, method = "single")     ## clustering with "single" linkage
prot_hcl_average <- hclust(d_prot, method = "average")   ## clustering with "average" linkage
prot_hcl_ward <- hclust(d_prot, method = "ward.D2")      ## clustering with "Ward.D2" linkage, this is the "ward" linkage discussed in our lecture
par(mfrow = c(2, 2))  

plot(prot_hcl_complete, cex = 0.8, hang = -1, main = "Hierarchical Clustering with Complete Linkage")
plot(prot_hcl_single, cex = 0.8, hang = -1, main = "Hierarchical Clustering with Single Linkage")
plot(prot_hcl_average, cex = 0.8, hang = -1, main = "Hierarchical Clustering with Average Linkage")
plot(prot_hcl_ward, cex = 0.8, hang = -1, main = "Hierarchical Clustering with Ward Linkage")

par(mfrow = c(1, 1))  ## Stop placing multiple plots in one figure.

###Question 2###
# Both the the dendrograms from the "ward" and "complete" linkage seems to be alright 
# Chose Ward Method because in the dendrogram, the height in each branch is small when the height between different branch are big
# In Dendrogram of "ward" method, it suggests that we can split into 2,3,4 or 5 clusters

plot(prot_hcl_ward, cex = 0.8, hang = -1, main = "Hierarchical Clustering with Ward Linkage")
rect.hclust(prot_hcl_ward, k = 2, border = 2:3) ## "border" is used to specify the border colors for the rectangles.

rect.hclust(prot_hcl_ward, k = 3, border = 2:4)  

rect.hclust(prot_hcl_ward, k = 4, border = 2:5)
rect.hclust(prot_hcl_ward, k = 5, border = 2:6) 

#I decide to test for 5 clusters
prot_cluster_ward_2 <- cutree(prot_hcl_ward, 2)
prot_cluster_ward_3 <- cutree(prot_hcl_ward, 3)
prot_cluster_ward_4 <- cutree(prot_hcl_ward, 4)
prot_cluster_ward_5 <- cutree(prot_hcl_ward, 5)

prot_cons_ward <- prot_cons
prot_cons_ward$cluster_2 <- prot_cluster_ward_2
prot_cons_ward$cluster_3 <- prot_cluster_ward_3
prot_cons_ward$cluster_4 <- prot_cluster_ward_4
prot_cons_ward$cluster_5 <- prot_cluster_ward_5

#Compare median value
prot_cluster_ward_2_median <- aggregate(prot_cons_ward[, 1:9], list(prot_cons_ward$cluster_2), median)
prot_cluster_ward_3_median <- aggregate(prot_cons_ward[, 1:9], list(prot_cons_ward$cluster_3), median)
prot_cluster_ward_4_median <- aggregate(prot_cons_ward[, 1:9], list(prot_cons_ward$cluster_4), median)
prot_cluster_ward_5_median <- aggregate(prot_cons_ward[, 1:9], list(prot_cons_ward$cluster_5), median)

prot_cluster_ward_2_median
prot_cluster_ward_3_median
prot_cluster_ward_4_median
prot_cluster_ward_5_median

#Compare min and max value, 
prot_cluster_ward_2_range <- aggregate(prot_cons_ward[, 1:9], list(prot_cons_ward$cluster_2), range)
prot_cluster_ward_3_range <- aggregate(prot_cons_ward[, 1:9], list(prot_cons_ward$cluster_3), range)
prot_cluster_ward_4_range <- aggregate(prot_cons_ward[, 1:9], list(prot_cons_ward$cluster_4), range)
prot_cluster_ward_5_range <- aggregate(prot_cons_ward[, 1:9], list(prot_cons_ward$cluster_5), range)

prot_cluster_ward_2_range
prot_cluster_ward_3_range
prot_cluster_ward_4_range
prot_cluster_ward_5_range
#In case we want to have a broad picture
data.frame(prot_cluster_ward_2_median, prot_cluster_ward_2_range, table(prot_cluster_ward_2))
data.frame(prot_cluster_ward_3_median, prot_cluster_ward_3_range, table(prot_cluster_ward_3))
data.frame(prot_cluster_ward_4_median, prot_cluster_ward_4_range, table(prot_cluster_ward_4))
data.frame(prot_cluster_ward_5_median, prot_cluster_ward_5_range, table(prot_cluster_ward_5))

# We can use separately median and range to interpret the result
# Group 1 and 3 in 3-cluster model are merged into Group 1 in 2-cluster mode
# However, they are much different from each other -> 3-cluster is better
# Group 2 and 3 in 4-cluster model are merged into Group 2 in 3-cluster mode
# However, they are much different from each other -> 4-cluster is better
# Group 2 and 4 in 5-cluster model are merged into Group 2 in 4-cluster mode
# However, they are much different from each other -> 5-cluster is better

## In the 5-cluster solution, the median values of Cluster 2 Cluster 3 
## are quite far away from each other (for example Cereals: 24.95 vs 55.90 ) So it makes more sense to keep them as two clusters.

## The following difference between Cluster 2 and Cluster 3 of the 5-cluster solution can be seen:
##
## - Cluster 2: 8.0 <= Redmeat <= 18, 5.7 <= Whitemeat <= 14, 17.5 <= Milk <= 25.8
## - Cluster 3: 4.4 <= Weight <= 7.8, 5 <= Whitemeat <= 6.3, 8.3 <= Milk <= 11.1
subset(prot_cons_ward,select = "cluster_5")
