#*Prediction in Multiple Linear Regression

#*Boston Housing Data Set
#*variables: predictiors consist of mix of continous and factor
#*medv: median property value in $1000s, (Y VAriable)
#*rm number of rooms
#*rad: index of accessibility to highways (Factor)

library(MASS)
data(Boston)
head(Boston)
str(Boston)

table(Boston$rad)
#different values are 1,2,3,4,5,6,7,8,24 along with their counts
#1,2,3...are indexes

#rad is a factor
m_medv <- lm(medv~rm+crim+factor(rad), data = Boston)
coef(m_medv)

#in result we see factor for rad1 is omitted, its a rule, the first has to be removed

#Manually predicting for following values
#crim = 0.00632
#rad = 1
#rm = 6.5
#we substitute in above coeff: 
-27.48 - 0.161 * 0.00632 + 7.9 * 6.575 + 0 * 8
#reuslts in 24.461 thousand dollars


#Above was for one row, lets do it for the entire data
fitted_m <- fitted.values(m_medv)
head(fitted_m)

#suppose we have new observations
predict(m_medv, data.frame(crim = 3.6, rad = "5", rm = 6.28))
#we get 24.39, above we had got using manually the same answer

#what if i have to predict several values
newX <- data.frame(crim = c(3.6, 3.6), rad = c(5, 24), rm = c(6.28, 6.28))
predict(m_medv, newX)

#Use the model to identify whetehr a property with 
#crim = 3.6, rad = 5, rm = 6 with price $36K is fairly priced?
newX <- data.frame(crim = 3.6, rad = 5, rm = 6)
predict(m_medv, newX)
#it gives 22.12

#therefore our model says, 22.12 thousand dollars it should be

#one step ahead of analysis, predicition level
predict(m_medv, newX, interval = "prediction", level = 0.95)
#at 95% level the above 36 is outside the band of my upper limit which is 34.10
#if it was below 10K instead of 36k, it would be underpriced, as model tells lower limit as 10.15

#one step ahead of analysis, confidence level
predict(m_medv, newX, interval = "confidence", level = 0.95)

#when to use predcition or confidence
#if your interested individually, look at prediction 
#if a company is interested in the mean value, choose confidence 