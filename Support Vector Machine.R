#Includes an example with
#brief definition of what svm is
#svm classification model
#svm classification plot
#interpretation
#tunning or hyperparmeter optimization
#best model selection
#confuison matrix
#Missclassificagtion error

data(iris)
str(iris)
#using all the above variables, i want to build classification models
library(ggplot2)
qplot(Petal.Length, Petal.Width, data = iris)
#lets see any possiblities of separation
qplot(Petal.Length, Petal.Width, data = iris, 
      color = Species)
#red is very far away from other group

#Support Vector Machine
#Exxplanation, relook into it
install.packages("e1071")
library(e1071)

#i want to predict Species, so
mymodel <- svm(Species~., data= iris)
summary(mymodel)

#it shows result as classification
#but if it was quantiative problem, we would have done regression
#8 belonging to 1st type of species
#22 to 2nd type and so on

plot(mymodel, data= iris)#upto here was okay if we had only two variables

#but we have more variables, we need more info
plot(mymodel, data= iris, 
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))#widht on Y and Petal.leng on X axis
#it shows decision boundaries
#listen to explanation, easy

#Confusion Matrix and Misclassification Error
pred <- predict(mymodel, iris)

tab <- table(Predicted = pred, Actual = iris$Species)#pred vs actual
tab
#48 species belonging to versicolor, predicted also predicted to be versicolor
#2 datapoints belonged to vriginica and predicted to be versicolor

1-sum(diag(tab))/sum(tab)
#2.7%, this is done using radial function

#we can use linear function to get miscclassifcation
mymodel <- svm(Species~., data= iris,
               kernel = "linear")
summary(mymodel)
#alll details change now

#lets calculate 
pred <- predict(mymodel, iris)

tab <- table(Predicted = pred, Actual = iris$Species)#pred vs actual
tab

1-sum(diag(tab))/sum(tab)
#3.3%

#we can check using polynomial, 
#just change kernel to polynomial and same steps after
#4.4%

#we can check sigmoid
#erro is higher now, 11.3%
#for this dataset, sigmoid is the worst

#lets use fine tune to get better classification
#tuning is also called hyperparameter optimization
set.seed(123)
tmodel <- tune(svm, Species~., data = iris,
               ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9)))#starts with 0 and goes to 1 with 0.1 increment
#if cost is too high, model might store many svm, and will lead to overfitting
#if cost is too low, under fitting
#default value is 1
#so we use large range to calculate optimal value of cost
#we have to be careful with large dataset


plot(tmodel)
#darker regions means: lower missclassifcation error, lower values of cost and many values for epsilon
summary(tmodel)

#Using above result we choose our best model
mymodel <- tmodel$best.model
summary(mymodel)
#this shows classification, bse our y variable is a factor variable
#for classification problems always radial is best

plot(mymodel, data = iris, Petal.Width~Petal.Length, slice = list(Sepal.Width = 3, Sepal.Length = 4))

#after this, look at the classification and missclassifcation error
pred <- predict(mymodel, iris)

tab <- table(Predicted = pred, Actual = iris$Species)#pred vs actual
tab

1-sum(diag(tab))/sum(tab)
#error is 1.3%
