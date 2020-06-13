#Random Forest in R
#Normal, Suspect and Pathologeical

# Read Data

data <- read.csv(file.choose(), header = TRUE)
str(data)
data$NSP <- as.factor(data$NSP)
table(data$NSP)

# Data Partition
set.seed(123)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

# Random Forest
#Random Forest Developed by aggregating tress, instead of one decision tree, we can develop hundreds of trees, then aggregate from those trees
#Can be used for classification or regression
#IF Y VARIABLE is factor variable, we sue classification
#if y variable is continous, we use it for regression
#Avoids overfitting: 
#can be dealt with large number of features
#helps us with feature selection based on importance
#User Friendly: only 2 parameters
#trees: default 500
#variables randomly sampled, like mtry
#if we using classification: default is sq.root(p) for classification &
#if we using regression: p/3

library(randomForest)
set.seed(222)
rf <- randomForest(NSP~., data=train,
                   ntree = 300,
                   mtry = 8,
                   importance = TRUE,
                   proximity = TRUE)
#when we see: No of variables tried at each split is 8
#this means, we are using only 8 variables in this model
#OOB is 5.75% pretty good, bse accuracy is 95% 
#Confusion Matrix: Quite Good
#prediciting class 1 is 0.018, 
#higher for second class:
#12% for predciint NSP in third class

print(rf)
attributes(rf)

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, train)
confusionMatrix(p1, train$NSP)#accuracy is very high
#Remember: Classification: Accuracy
#regression: Root square and RMSE


# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, test)
confusionMatrix(p2, test$NSP)

# Error rate of Random Forest
plot(rf)
#as number of trees grow, error drops down and becomes constant

# Tune Random Forest Model mtry
t <- tuneRF(train[,-22], train[,22],#removing 22nd coln/variable(these are input variables), & response variable in our training is 22 which is nsp
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 300,
            trace = TRUE,
            improve = 0.05)#here as per graph we choose mtry value to be 4

# No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "green")
#revisit video for explanation here

# Variable Importance
varImpPlot(rf)#this shows which variables, through gini and accuracy

#more info
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Variable Importance")

importance(rf)#this is quantative values
varUsed(rf)
#this shows how many a variable has been occured how many times
#6th variable occured 5 times

# Partial Dependence Plot
partialPlot(rf, train, ASTV, "2")#skipped, 

# Extract Single Tree
getTree(rf, 1, labelVar = TRUE)

# Multi-dimensional Scaling Plot of Proximity Matrix
MDSplot(rf, train$NSP)