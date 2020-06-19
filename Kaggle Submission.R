#Competing on Analytics at Kaggle
#Data: https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/data?select=sample_submission.csv

# Data
data <- read.csv(file.choose(), header=T)
data$is_attributed <- as.factor(data$is_attributed)

# Problem
prop.table(table(data$is_attributed))

# Data partition
set.seed(123)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# Addressing Class Imbalance
library(ROSE)
under <- ovun.sample(is_attributed ~ ., 
                     data = train, 
                     method = "under", 
                     N = 376)$data

# Prediction Model
library(randomForest)
under <- under[,c(1:5, 8)]
model <- randomForest(is_attributed ~ ., data = under)

# Evaluation
library(e1071)
library(caret)
confusionMatrix(predict(model, test), 
                test$is_attributed, 
                positive='1')

# File Submission on Kaggle
new <- read.csv(file.choose(), header=T)
new1 <- new[,-1]
new1$attributed_time <- 0
p <- predict(model, new1)
d <- data.frame(click_id = new$click_id, is_attributed = p)
write.csv(d, '~/Desktop/KAGGLE.csv', row.names = F)
