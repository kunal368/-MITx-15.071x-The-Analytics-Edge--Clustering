#Kunal Agarwal 31/5/2016 #Clustering
#Assignment #Market Segmentation for Airlines

#Loading Airlines Dataset
Airlines <- read.csv("AirlinesCluster.csv")

#Inspecting the Dataset
str(Airlines)
summary(Airlines)

#Normalizing the data
install.packages("caret")
library(caret)

#Commands for Normalization
preproc = preProcess(Airlines)
airlinesNorm = predict(preproc, Airlines)

summary(airlinesNorm)# every variable has mean 1 and sd =0
sd(airlinesNorm$Balance)

#Hierarchical Clustering
distancesair <- dist(airlinesNorm, method = "euclidean")
airHclusters <- hclust(distancesair, method = "ward.D")

#Plotting Dendogram
plot(airHclusters)

#Dividing Datapoints in 5 clusters
customerGroups <- cutree(airHclusters, k = 5) 

a1 <- subset(airlinesNorm, customerGroups == 1)

#Average value of each variable in each cluster
tapply(Airlines$Balance, customerGroups, mean)
tapply(Airlines$QualMiles, customerGroups, mean)
tapply(Airlines$BonusMiles, customerGroups, mean)
tapply(Airlines$BonusTrans, customerGroups, mean)
tapply(Airlines$FlightMiles, customerGroups, mean)
tapply(Airlines$FlightTrans, customerGroups, mean)
tapply(Airlines$DaysSinceEnroll, customerGroups, mean)

lapply(split(Airlines, customerGroups), colMeans)


#K-means Clustering
set.seed(88)
airkclusters <- kmeans(airlinesNorm, centers = 5, iter.max = 1000)
str(airkclusters)

k1 <- subset(airlinesNorm, airkclusters$cluster == 1)
k2 <- subset(airlinesNorm, airkclusters$cluster == 2)
k3 <- subset(airlinesNorm, airkclusters$cluster == 3)
k4 <- subset(airlinesNorm, airkclusters$cluster == 4)
k5 <- subset(airlinesNorm, airkclusters$cluster == 5)

#Getting Mean
airkclusters$centers
tapply(Airlines$Balance, airkclusters$cluster, mean)
tapply(Airlines$QualMiles, airkclusters$cluster, mean)
tapply(Airlines$BonusMiles, airkclusters$cluster, mean)
tapply(Airlines$BonusTrans, airkclusters$cluster, mean)
tapply(Airlines$FlightMiles, airkclusters$cluster, mean)
tapply(Airlines$FlightTrans, airkclusters$cluster, mean)
tapply(Airlines$DaysSinceEnroll, airkclusters$cluster, mean)
