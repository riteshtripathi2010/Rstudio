#ROC Curve

#Includes examples
#Logistic Regression Model
#Confusion matrix
#Missclassification Rate
#rocr package
#true positive rate or senstivity
#false positive rate or 1 - specificity
#area under curve
data <- read.csv(file.choose(), header = T)
str(data)

#Logistic Regression Model
library(nnet)
mymodel <- multinom(admit~., data = data)

#Confusion Matrix & Missclassification Error
p <- predict(mymodel, data)
tab <- table(p, data$admit)
tab
#actually 20 people not admited but model says, they should be admitted
#and so on
sum(diag(tab))/sum(tab)#this is classification rate
1-sum(diag(tab))/sum(tab) #this is missclassification rate

#qns is whetehr this 70 % classification is good?
#how many students were admitted and how many students were not admmited
table(data$admit)
#127 students were admited and 273 were not admmited
#one way to see if these applicants will be admited or not
#by using the higher value btn 273 and 127
#273/400 = 68, if we predict all students who wont be accepted, will be 68% of the time
#if we create a model and we see the accuracy is less than 68%, we shouldnt use that model
#right now we have used logistic and gives 70% accuracy
#better than this benchmark number

#Model Performance Evaluation
install.packages("ROCR")
library(ROCR)
pred <- predict(mymodel, data, type = 'prob')#i want to find probability values
head(pred)#we see first 6 probability values
head(data)#we see first applicant wasnt admitted, our pred prob is 0.18 which is very low
hist(pred)#we see probability ranging from 0.0 to 0.8
#most of the valeus are below 0.4

pred <- prediction(pred, data$admit)
eval <- performance(pred, "acc")
plot(eval)
#we get new cut off values thayt change
#for different cut of values, we see what is the exact accuracy level
#as cut off value increases, so does accuraacy level, 
#our default value for cut off was 0.5, 
#now i want to see the best value, we make use of line on this chart
abline(h = 0.71)#somewhere we have a peak here

#lets try to find what is the value of that peak
abline(h = 0.71, v = 0.45)

#Identifying Best Cutoff and Accuracy
max <- which.max(slot(eval, "y.values")[[1]])
max#it says that it is the 61st value
acc <-  slot(eval, "y.values")[[1]][max]
acc#0.7175
#now i want to find what is the optimal value of the cut off
#it might not be exactky 0.45 as seen earlier
cut <- slot(eval, "x.values")[[1]][max]
cut#0.468

print(c(Accuracy = acc, Cutoff = cut))
#this will give accurac and cut off values

#ROC Curve
#true positive rate is 29/29+28 = 22%,which is very low for accucracy level
#falsE:20/253+20  
pred <- prediction(pred, data$admit)
roc <- performance(pred, "tpr", "fpr")
plot(roc)
#accuracy to be 100 per, the curve should start from 0.0 to 1.0
#in reality we get such curves which are not ideal values 
abline(a = 0, b = 1)
#this starigt line, without any model, out of 400 students reject everyone
#we wil be right 68% of all the time
#if model does worst of the time, the cuve would be below the line
#but in this case, the model is doing better 
#this curve can be compared to differetn models

#lets add in more few things
plot(roc, colorize = T,
     main = "Roc Curve",
     ylab = "Senstivity",
     xlab = "1-Specifity")#the color is based on cut off

#Area Under Curve
#another way, people calcualte area under the curve
#when u have many curves on this chart, it will be difficulat to differntiate between the performance
#if area under the curve is higher,that means the model performance is better

auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc
auc <- round(auc, 4)#rounding off

#lets get Legend
#i will select any values where to place the AUC value
legend(.6, .2, auc, title = "AUC", cex = 0.8)
