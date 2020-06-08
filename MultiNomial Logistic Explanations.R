setwd("/Users/riteshtripathi/Desktop")
#mydata <- read.csv("~/Dropbox/Public/Cardiotocographic.csv")
mydata <- read.csv("Cardiotocographic.csv", header = TRUE)
#1 means normal, 2 means suspect, 3 means phatological
#we are using all the features to predicit nsp
str(mydata)
mydata

#lets create a new variable to be categorical from NSP to NSPF
mydata$NSPF <- as.factor(mydata$NSP)

#Multinomial Logistic regression
library(nnet)
mydata$out<-relevel(mydata$NSPF, ref="1")#we are referencing 1 as to be normal since there are 3levels for response variables
#so 1 will mean normal patient

#Now we are ready to develop Multinomial Logistic Regression
library(nnet)

#lets make my model
mymodel<-multinom(out~LB+AC+FM, data=mydata)#just for this illustration, we are suing LB, AC and FM
summary(mymodel)#check video again for explanation on interpretation

#Equations for Calculating Probabilities
#Predict
predict(mymodel, mydata)
#model predicts first patient to be normal, again normal for second patient and so on
#if we compare above results with actual data, the report says, 1st patient to be 2, hes a suspect but predicted is normal
#so there is missclassification for first patient

#for second patient, report says hes normal, pred is normal
#good match

#lets find predition of probablities
predict(mymodel, mydata, type ="prob")
#what we see, for every patient, there are three probablities given
#like for last patient, prob for patient to be normal is 0.37%

#i want to find possibilites for specific patients:
predict(mymodel, mydata[c(3, 100, 400),], type ="prob")#3rd patient, 100th patient and 400th patient
#3rd patient, 96% chance to be normal

#Misclassification Error
#we can compare predicition of model with actual data, to see what match is not there
cm <- table(predict(mymodel), mydata$NSPF)
print(cm)

#top 123 are actual values
#side 123 are predicted values
#1592 patients were classified as normal and even pred were normal
#there wre 61 patients who were classifid as Normal, but pred them as Suspect and so on

#Misclassification Error is
1-sum(diag(cm))/sum(cm)
#18% model miss clasifies a paitent


#2-tailed z test
z <- summary(mymodel)$coefficients/summary(mymodel)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p
#coeff for LB has pvalue of almost 0, confidence level is high
#LB for 3, i have p value as 0.46, confid is 1-0.46: 54% , its too small, it needs to be 90 and above %
#so this means LB doesnt have siginifcant contribution in the model, when 1 is reference adn looking for 3rd level response

#AC, both pvalues are small, hence playing siginifcant role in model
#FM confidence level are significant, bse 0.0004...for first pvalue, hence playing important role
