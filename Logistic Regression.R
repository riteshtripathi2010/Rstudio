#Logistic Regression

#Categorical Response Variable at Two Levels
# Logistic Regression
#Will use student file
#Goal is to classify student application as admit or reject
#dependent variable: Admit, this is not continous, its a categorical or factor variable
#Predictros or independent variables
#Gre
#GPA
#Rank

# Read data file
mydata <- read.csv(file.choose(), header = T)
str(mydata)
mydata$admit <- as.factor(mydata$admit)
mydata$rank <- as.factor(mydata$rank)#there are 4 categories in here

# Two-way table of factor variables, & make sure u dont have any cells with 0 values
xtabs(~admit + rank, data = mydata)
#we see max value as 97 and min is 12, since we dont have 0 values, we can move forward

# Partition data - train (80%) & test (20%)
set.seed(1234)
ind <- sample(2, nrow(mydata), replace = T, prob = c(0.8, 0.2))
train <- mydata[ind==1,]
test <- mydata[ind==2,]

# Logistic regression model(General Linear Model)
mymodel <- glm(admit ~ gre + gpa + rank, data = train, family = 'binomial')
#we use binomial bse admit takes only two values, 0 and 1
summary(mymodel)
#more stars we have, more signinifcant it wil be
#gre: 1-0.18 = 0.82, 82% confidence level, since its less, thats why its not statistically significant
#gpa: 1-0.003 = 0.997, 99% level of confidnece that its statistically signifdicant

#we should drop gre, 
#rerun without gre

mymodel <- glm(admit ~ + gpa + rank, data = train, family = 'binomial')

summary(mymodel)
#gpa: p value has gone further down


# Prediction
p1 <- predict(mymodel, train, type = 'response')
head(p1)#5 has gone to testing dataset, 1,2,3..are observation values
head(train)
#first applocant was not admitted, with gre of 380, gpa of 3.61 and school they came from was ranked 3
#when we see prob: it shows 28% chance this applicant should get admitted

#Interpretation & Coefficient
#as ranks go lower and lower, the odds of that applicant gets worse and worse, as per the coefficients


# Misclassification error - train data
pred1 <- ifelse(p1>0.5, 1, 0)#if p1 is greater than 0.5, repsonse should be 1 otherwise 0(we are converting from prob to 0 and 1)
tab1 <- table(Predicted = pred1, Actual = train$admit)
tab1#this is also called confusion matrix
#there were 208 applciants who were actual not admitted and model also predicts to be not admmited 208 applicants
#there were 29 applicants who were actual admmited, and predicts also the same

#there were 73 applicants who were actual admitted, model predcited them not to be admitted

#
1 - sum(diag(tab1))/sum(tab1)#27% is the misssclassification error for training set

# Misclassification error - test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$admit)
tab2
1 - sum(diag(tab2))/sum(tab2)

# Goodness-of-fit test
with(mymodel, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))
#p value is 0.00000145, our p value is so small, means our confidecne level is too high meaning this model is statistically significant
