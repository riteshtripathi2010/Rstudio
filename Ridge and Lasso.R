#*Ridge Lasso Regression
#Includes:
#Boston Housing data
#*illustrates use of caret package
#*data partition
#*custom control paramters
#*cross validations
#*linear model
#*residuals plot
#*use of glmnet package
#*ridge regression
#*plot results
#*log lambda plot
#*fraction deviance explained plot
#*variable importance plot
#*interpretation
#*lasso regression
#*elastic net regression
#*compare models
#*best models
#*saving and reading final model for later use
#*prediction
library(caret)
install.packages("glmnet")
library(glmnet)
install.packages("mlbench")
library(mlbench)
install.packages("psych")
library(psych)

data("BostonHousing")
data <- BostonHousing
str(data)
#medv is the prediction

#lets look at corelation between independent variables
#below is part of pysch package
#mlbench is part of boston housing
pairs.panels(data[c(-4, -14)], cex = 2)

#when numeric variables are highly corelated, that creates multi coleanirty problem
#and when we do multiple linear regression problem the estimates we get are not very stable
#and because of that the predcition model we are trying to develop may not be very accurate

#Problem:
#Collinearity leads to Overfitting

#Solution:
#* 1. Ridge Regression: shrinks coefficients to non zero values to prevent overfit, but keeps all variables
#* 2. Lasso Regression: shrinks regression coefficients with some shrunk to zero. Helps with feature selection
#* 3. Elastic Net Regression: Mix of Ridge and LAsso

set.seed(222)
ind <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]

#Custom Control Paramters
#trainControl with in caret package to create our custom control parameters
# Custom Control Parameters
custom <- trainControl(method = "repeatedcv",
                       number = 10,#in 10 fold cross validation,training data is broken into 10 parts
                       #and then model is made from 9 parts and 1 part is used for error estimation
                       #this is repeated 10 times with a different part used for error estimation
                       repeats = 5,#this repeats the process 5 times
                       verboseIter = T)#when model is running we also see whats going on

# Multiple Linear Model
set.seed(1234)
lm <- train(medv ~ .,
            train,
            method = 'lm',
            trControl = custom)

# Results
lm$results
#* it tells us that we have a model with intercept
#* Rsquared is the coefficient of determination, 
#* this number indicates more than 78% of variablity seen in response (medv) is bse of the model
summary(lm)
#variables that dont have stars are not statistically significant
plot(lm$finalModel)

# Ridge Regression
set.seed(1234)
ridge <- train(medv ~ .,
               train,
               method = 'glmnet', 
               tuneGrid = expand.grid(alpha = 0,
                                      lambda = seq(0.0001, 1, length = 5)),#we have 5 values between 0.0001 to 1
               trControl = custom)
#when we execute above, it finds the best value of lambda is 0.5
#as we increase lamda, we make coefficients to shrink

# Plot Results
plot(ridge)
#we see for higher values of lambda, erorr increases
#best value of lambda we see is 0.5

plot(ridge$finalModel, xvar = "lambda", label = T)
#when log lambda is around 9, we see all the coeffiecents to be more or less 0
#as we relax lambda, we see coefficients to grow
#on top, we see all the 13 variables
#so, increasing lambda helps to reduce size of the coefficients, it doesnt make the size of those coefficnets which are not contributing major way 0

plot(ridge$finalModel, xvar = 'dev', label=T)
#this gives us fraction deviance
#0.2 or 20% of the variablity is being explained at this point, with the slight growth in the coefficents
#by the time we reach at 0.6 or 60% of deviance is being explained
#and then there is a sudden jumpe, and coefficents become highly inflated and in this area we are likely doing overfitting

#plot variable importance
plot(varImp(ridge, scale=T))
#check the difference whenyou make scale as F

# Lasso Regression (least absolute shrinkage and selection operator)
#if there is high coleanirty in two variables, lasso selects one variable and it will ignore others
set.seed(1234)
lasso <- train(medv ~ .,
               train,
               method = 'glmnet',
               tuneGrid = expand.grid(alpha = 1,
                                      lambda = seq(0.0001, 1, length = 5)),
               trControl = custom)
#we see lambda value to be very small

# Plot Results
plot(lasso)
#shows, higher value increases RMSE
#
plot(lasso$finalModel, xvar = 'lambda', label=T)
#lambda shows the labels, that indicates variables on the graph
#we see variable 5 is growing much rapidly as we reduce lambda when compare to var 6
#therefore var 6 is performing better than 5

plot(lasso$finalModel, xvar = 'dev', label=T)
#0.6or 60% of variablity explains by only 3 variables
#remember lasso does coefficients shrinkage as well as feature selection
#we can see that with just 3 variables we can explain 60% of variability
#we have situations for coefficients to grow very aggressively and wesee alot of overfitting going on
#when we see the blue line variable growing rapidly, means it has less importance in model
#compare to var 6

#plot variable importance
plot(varImp(lasso, scale=T))


# Elastic Net Regression: is the combination of ridge and lasso
set.seed(1234)
en <- train(medv ~ .,
            train,
            method = 'glmnet',
            tuneGrid = expand.grid(alpha = seq(0, 1, length = 10),
                                   lambda = seq(0.0001, 1, length = 5)),
            trControl = custom)
#it gives optimal alpha at 0.111 with low lambda 

#we see l# Plot Results
plot(en)
#we see for 1(yellow), rmse is high for that
#as the value goes down from 1, rmse goes down
#try to change the lambda value to 0.0001, 0.2
plot(en$finalModel, xvar = 'lambda', label=T)
#when log lambda is 4, coeff are almost 0, model has 2 var
#when log lambda is 2, soe of coeff grow and have 10variable
#when log lambda is 0, we see light blue coeff grwoing very rapidly as compared to pink and dark blue above
#that means pink coeff will have larger importance in the model comapred to light blue coeff


plot(en$finalModel, xvar = 'dev', label=T)
#its similar to taht of lasso regression model

plot(varImp(en))

# Compare Models
model_list <- list(LinearModel = lm, Ridge = ridge, Lasso = lasso, ElasticNet = en)
res <- resamples(model_list)
summary(res)
bwplot(res)#we see difficulty in knowing which model is better, we look at numeric summary
xyplot(res, metric = 'RMSE')#data we used, is not differentiating between ridge and linear model bse points are very close to each other
#but in other datasets, we can see much better 
#the first point we see, means that ridge value is much smaller than that of linear model
#dots above dotted line perform better way when we have ridge regression model
#dots below the dotted line, perform better when we have linear model 
#we can also compare lasso with elastic and so on


# Best Model for elastic model
en$bestTune
#shows best value for alpha, and lambda
#so 0.111 is closer to 0 than 1 that means that our final elastic modpel is of ridge model and less of lasso model
best <- en$finalModel
coef(best, s = en$bestTune$lambda)
#so highest coeff is for nox than followed by rm


# Save Final Model for Later Use
saveRDS(en, "final_model.rds")#if we close everything and open again, we can read this file for prediction
#when u open again, run all packages, run dataset, run data partion and then run below command
fm <- readRDS("final_model.rds")
print(fm)

# Prediction
p1 <- predict(fm, train)
sqrt(mean((train$medv-p1)^2))
#4.11

p2 <- predict(fm, test)
sqrt(mean((test$medv-p2)^2))
#6.14



