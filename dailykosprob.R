#Kunal Agarwal 31/5/2016 #Clustering
#Assignment #Document Clustering

dailykos <- read.csv("dailykos.csv")
str(dailykos)

#Calculating distances
distances = dist(dailykos, method = "euclidean")

#Hierarchical Clustering
kosClusters = hclust(distances, method = "ward.D")

#Plotting Dendogram
plot(kosClusters)

#Splitting Data Into 7 clusters
KosGroups <- cutree(kosClusters, k =7)

#Creating 7 datasets containing datapoints from diff clusters
kos1 <- subset(dailykos, KosGroups == 1)
kos2 <- subset(dailykos, KosGroups == 2)  
kos3 <- subset(dailykos, KosGroups == 3)  
kos4 <- subset(dailykos, KosGroups == 4)
kos5 <- subset(dailykos, KosGroups == 5)
kos6 <- subset(dailykos, KosGroups == 6)
kos7 <- subset(dailykos, KosGroups == 7)

#Top 6 words in each Cluster 
tail(sort(colMeans(kos1)))
tail(sort(colMeans(kos2)))
tail(sort(colMeans(kos3)))
tail(sort(colMeans(kos4)))
tail(sort(colMeans(kos5)))
tail(sort(colMeans(kos6)))
tail(sort(colMeans(kos7)))

#k-means Clustering
set.seed(1000)
kclusters <- kmeans(dailykos, centers = 7)
str(kclusters)
#Creating 7 datasets containing datapoints from diff k-clusters
cluster1 <- subset(dailykos, kclusters$cluster == 1)
cluster2 <- subset(dailykos, kclusters$cluster == 2)  
cluster3 <- subset(dailykos, kclusters$cluster == 3)  
cluster4 <- subset(dailykos, kclusters$cluster == 4)
cluster5 <- subset(dailykos, kclusters$cluster == 5)
cluster6 <- subset(dailykos, kclusters$cluster == 6)
cluster7 <- subset(dailykos, kclusters$cluster == 7)

#Top 6 words in each Cluster got from k-means clustering 
tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

#Comparing both methods of clustering
table(KosGroups, kclusters$cluster)
