#Linear Discriminant Analysis
#purpose is to estimate relationships between a single categorical dependent variable
#and a set of quantative independent variables

#Eg
#predciting success / failure of new products
#accepting / rejecting admission to an apllicant
#Predciting credit risk category for a person
#Classifying patients into different categories
#iris data

data("iris")
str(iris)

install.packages("psych")
library(psych)

pairs.panels(iris[1:4],#making scatter plots with first 4 variables
             gap = 0,
             bg = c("red","green", "blue") [iris$Species],
             pch = 21)
#so each of the numeric variables, we have histograms
#we also have scatter plot between variables, sepal length vs sepal width and so on

#we use discriminant analysis to find linear combinations of these variables
#Data Partion
set.seed(555)
ind <- sample(2, nrow(iris), replace = T, prob = c(0.7, 0.3))
training <- iris[ind == 1,]
test <- iris[ind == 2,]

#Linear Discriminant Analysis
library(MASS)
linear <- lda(Species ~., training)
linear
#Discriminant functions are scalled
#Percentage sepearations achieved by the first discrimant function is 99.37%
#and so on

#Stacked Histograms of Discriminant Function Values
p <- predict(linear, training)
ldahist(data = p$x[,1], g = training$Species)#,1 > we are choosing ld1, less of overlapping we see
ldahist(data = p$x[,2], g = training$Species)#, alot of over lapping we see
#separation achoeved by LD2 is not that great

#Bi-Plot
install.packages("devtools")
library(devtools)

install_github("fawda123/ggord")
library(ggord)
ggord(linear, training$Species, ylim = c(-10, 10))
#we see three species are spearated quite well
#but there is some amount of overlap btn 3rd and 2nd speicei
#for new point, if value is above 5, we say its in red color as setosa
#point btn 0 and -3 should be versicolor
#less than 5...

#Partition Plots
install.packages("klaR")
library(klaR)
partimat(Species~., data = training, method = "lda")
#we get 6 plots with their variables
#so far we have developed linear discriminant model

#for quadratic discriminant analysis
partimat(Species~., data = training, method = "qda")
#there wwill be some changes 

#COnfusion Matrix and Accuracy - Training Data
p1 <- predict(linear, training)$class
tab <- table(Predicted = p1, Actual = training$Species)
tab

sum(diag(tab))/sum(tab)#accuracy of model based on training data

#testing data
p2 <- ppredict(linear, testing)$class
tab1 <- table(Predicted = p2, Actual = testing$Species)
tab1

sum(diag(tab))/sum(tab)

#Linear Discriminant Analysis Advantage
#1. Historgam and biplot provide useful insights and are helpful for intepretation of the analysis
#2. if there is not a great difference in the group covariance matrices, then linear discriminant analysis
#will perform as well as quadratic

#Linear Discriminant Analysis Limitation
#not useful for non linear problems
#not meeting multivariate normality assumptions of the independent variables can cause problems
#not meeting assumptions of equal covariance matrices can adversely affect the classification process
