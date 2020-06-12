#Decision Trees Complete
setwd("/Users/riteshtripathi/Desktop")
mydata <- read.csv(file.choose(), header = T)
View(mydata)
#Response variable is NSP (N = Normal, S = Suspect, P = Pathological)
str(mydata)#repsonse variable is here as int

#convertign to factor
mydata$NSPF <- as.factor(mydata$NSP)#thsi will create a new coln

#Partionong data
set.seed(1234)
#we take sample of size 2
pd <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.8, 0.2))
train <- mydata[pd == 1, ]#this means selecting all colns
validate <- mydata[pd == 2, ]

#Decision Tree with party package
library(party)
tree <- ctree(NSPF~LB+AC+FM, data = train)#just for illustration purpose we are using LB...
tree
plot(tree)
#root at top, 
#leaf at the bottom

#out of the three variables we used, AC si the most important
#Expalantion i skipped
#Sometimes u need to prun the tree by controlling some paramters
tree <- ctree(NSPF~LB+AC+FM, data = train, controls = ctree_control(mincriterion = 0.99, minsplit = 500))#0.9. 90% confidence level that the variable is significant
#200: a branch will split into 2 only if the sample size is atleast 200
#if we make 0.99, and 200, the tree will get less complicated 
tree
plot(tree)

#we have now 3 variables
#9 nodes
#we have a new patient, AC is less than 0.001
#lb is  for the new patient is 20, its less than 136
#we see response for response 1 is normal, 
#its more likely the patient is more likely to be 70% Normal

#this is how we predict for new patients coming in
#so we predict now
predict(tree, validate, type = 'prob')
#we get 3 numbers, 
#lets check for patient number 408
#prob of 1st response is 0.4, 
#prob of 2nd repsonse is 0.49
#etc

#Decison Tree with rpart Package
library(rpart)
tree1 <- rpart(NSPF~LB+AC+FM, train)

install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(tree1)

#i want extra information
rpart.plot(tree1, extra = 1)#u get sample sizes
#look at the video
#easy to understand,
#this part talks about sample sizes of patients

#more info
rpart.plot(tree1, extra = 3)#this will talk about 348 out of total patients were 1
rpart.plot(tree1, extra = 4)#this will talk about prob of a patient


#Prediction
predict(tree1,validate)#some of the sample sizes not shown here bexause those sample size patients are in training set

#Missclassification Error for train data
tab <- table(predict(tree), train$NSPF)
print(tab)
#there are 1222 patients who were normall and also predicted normal
#there were 70 patients who were suspect, but model predicted normal
#and so on
1-sum(diag(tab)) / sum(tab)

#Missclassification Error for test data
testPred <- predict(tree, newdata = validate)
tab <- table(testPred, validate$NSPF)
print(tab)
1-sum(diag(tab))/sum(tab)

#but remember, we have used only 3 variables, 
#its always to use all the variables to built the model
