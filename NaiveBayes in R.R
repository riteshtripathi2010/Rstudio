#Binary File

#Application Examples
#Email, spam or ham
#tweet sentiment: positive or negative
#face recognition
#classifiy news articles

install.packages("naivebayes")
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

data <- read.csv(file.choose(), header = T)

#first we do cross tabulation for admit and rank
xtabs(~admit + rank, data = data)
data$rank <- as.factor(data$rank)
data$admit <- as.factor(data$admit)

# Visualization
pairs.panels(data[-1])#removing first variable
#for naive bayes, we make sure the independent variables are not highly corelated
#the only numeric variables are gre and gpa
#and ocrelation btn them isnt really that strong
#its only 0.38

#lets create box plots 
data %>%
  ggplot(aes(x=admit, y=gpa, fill = admit)) +
  geom_boxplot() +
  ggtitle("Box Plot")
#red one is where the studnet is not admitted, and thats the gpa spread
#same we can do for gre, by replacing gpa to gre
#we see there is significant amount of overlap, overall average gpa is higher for students who are admitted
#since there is good amount of over lap btn two, model is unlikely to be 100% accurate

#lets do a density plot
data %>% ggplot(aes(x=gpa, fill = admit)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")
#we see there is clearly significant amount of over lap



# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(admit ~ ., data = train, usekernel = T)
model
#68% of students not admitted and so on
#we see three tables, gre with its mean and other stats
#gpa with its mean and other stats
#please note that when we have mean and std deviation, we can calculate any probablity
#whene evre we see indepenedent variable, that is numeric, we see its mean and std dev always given

#for categorical variables, we get probablities (rank)



train %>%
  filter(admit == "1") %>%
  summarise(mean(gre), sd(gre))
#run same thing with admit == 1, and see the values



plot(model)
#we see yellow means students getting admiited



# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))#this to compare this p with original values
#first applicant has prob 0.85% that will not be admitted
#reality is that he wasnt admitted as admit is 0, gre 380 and low rank of college as 3


# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$admit))
1 - sum(diag(tab1)) / sum(tab1)
#miscclasification level 27% f


# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$admit))
1 - sum(diag(tab2)) / sum(tab2)

