#Kunal Agarwal 31/5/2016
#Assignment #Analytics Edge #Clustering
#Stocks Prediction Prob

#Loading stocks Dataset
stocks <- read.csv("StocksCluster.csv")
str(stocks)

#proportion of positive returns in December
prop.table(table(stocks$PositiveDec))

#corelation between variables
cor(stocks)
summary(stocks)

#Splitting the dataset into training and testing dataset
set.seed(144)
library(caTools)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)

#Logistic Regression
StocksModel <- glm(PositiveDec ~., data = stocksTrain, family = "binomial")

#Making Predictions on training set
stocksTrainPredict <- predict(StocksModel, stocksTrain, type = "response")

table(stocksTrain$PositiveDec, stocksTrainPredict >= 0.5)

#Accuracy on training data (990+3640)/(990+3640+2689+787)= 0.5711818
(990+3640)/(990+3640+2689+787)

#Predictions on test data
stocksTestPredict <- predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, stocksTestPredict >= 0.5)

#Accuracy on testing data (417+1553)/(417+1553+344+1160) = 0.5670697
(417+1553)/(417+1553+344+1160)

#Accuracy of baseline model that always predicts PositiveDec =1 is 0.5460564
table(stocksTest$PositiveDec)
1897/(1897+1577)

#########Now clustering the stocks dataset
#Removing dependent variable before clustering

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

#Normalizing data
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

#Calculating mean for normalized data variable ReturnJan
mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

#k-means Clustering of normTrain
set.seed(144)
km <- kmeans(normTrain, centers = 3)
summary(km)

table(km$cluster) #cluster2 has maximum observations

#Testing site Cluster Assignments
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

table(clusterTest)

##Forming 3 Stocks training dataset corresponding to 3 clusters of stocksTrain
stocksTrain1 <- subset(stocksTrain, clusterTrain == 1)
stocksTrain2 <- subset(stocksTrain, clusterTrain == 2)
stocksTrain3 <- subset(stocksTrain, clusterTrain == 3)

##Forming 3 Stocks testing dataset corresponding to 3 clusters of stocksTest
stocksTest1 <- subset(stocksTest, clusterTest == 1)
stocksTest2 <- subset(stocksTest, clusterTest == 2)
stocksTest3 <- subset(stocksTest, clusterTest == 3)

#Mean-values of dependent variable in each training dataset
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

#Building Logistic Models for each dataset
StocksModel1 <- glm(PositiveDec ~., data = stocksTrain1, family = "binomial")
StocksModel2 <- glm(PositiveDec ~., data = stocksTrain2, family = "binomial")
StocksModel3 <- glm(PositiveDec ~., data = stocksTrain3, family = "binomial")

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

#Making Predictions on StocksTest1,2 and 3
PredictTest1 <- predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 <- predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 <- predict(StocksModel3, newdata = stocksTest3, type = "response")

#Calulating Accuracy of each dataset by confusion Matrix
table(stocksTest1$PositiveDec, PredictTest1 >= 0.5) # = 0.6194145
(774+30)/(804+471+23)

table(stocksTest2$PositiveDec, PredictTest2 >= 0.5) # = 0.5504808
(388+757)/(388+757+626+309)

table(stocksTest3$PositiveDec, PredictTest3 >= 0.5) # = 0.6458333
62/96

#Overall Test-set accuracy of cluster-then-prediction approach
AllPredictions <- c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes <- c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

table(AllOutcomes, AllPredictions >= 0.5)
#Overall ACCuracy = (467+1544)/3474 = 0.5788716
(467+1544)/3474
