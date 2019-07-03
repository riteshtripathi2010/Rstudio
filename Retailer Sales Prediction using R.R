#Retailer Mart Sales Prediction Using R
#Predicting Product Wise and Store Wise Sales
#Will be using Regression Problem
#Will be using three steos:
#Step1: Exloratory Data Analysis
#Step2: Data Preparation
#Step3: Predictive Modelling using different techniques
#Similar steps as of Jupyter, difference with "Encoding Categorical Variables"
#Label Encoding and One Hot Encoding
#Loading Libraries

#install.packages("data.table")
library(data.table) # used for reading and manipulation of data 

#install.packages("dplyr")
library(dplyr)      # used for data manipulation and joining 

#install.packages("ggplot2")
library(ggplot2)    # used for ploting 

#install.packages("caret")
library(caret)      # used for modeling 

#install.packages("corrplot")
library(corrplot)   # used for making correlation plot 

#install.packages("xgboost")
library(xgboost)    # used for building XGBoost model 

#install.packages("cowplot")
library(cowplot)    # used for combining multiple plots 

train = fread("/Users/riteshtripathi/Desktop/R Class/Train Sales.csv") 
test = fread("/Users/riteshtripathi/Desktop/R Class/Test Sales.csv") 


#Understanding Data
#Let’s check the dimensions of the data,
dim(train);dim(test)
#train dataset has 8523 rows and 12 features 
#test has 5681 rows and 11 columns
#Remember, train has 1 extra column which is the target variable

# feature names of train and test datasets
names(train)
names(test)
#item_Outlet_Sales is present in train but not in test dataset 

#summary of all the features present in a dataframe.
str(train)

str(test)
#REsult: there are 4 numeric and 7 categorical variables.

#Combining Train and Test
#We combine because we have to modify data, which is more efficient
#will carry out data visualization, feature engineering, one-hot encoding, and label encoding
#Later we will split this combined data back to train and test datasets.
test[,Item_Outlet_Sales := NA] 
combi = rbind(train, test) # combining train and test
dim(combi)

#Univariate Analysis
#We will visualize the continuous variables using histograms 
#and categorical variables using bar plots
#Since our target variable is continuous, we can visualise it by plotting its histogram
ggplot(train) + geom_histogram(aes(train$Item_Outlet_Sales), 
                               binwidth = 100, fill = "darkgreen") +  xlab("Item_Outlet_Sales")
#With above code, its seen that its right skewed and needed data transformation to treat
#its skewness

#Lets look at the Independent Variables (numeric variables)
p1 = ggplot(combi) + geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi) + geom_histogram(aes(Item_Visibility), binwidth = 0.005, 
                                    fill ="blue") 
p3 = ggplot(combi) + geom_histogram(aes(Item_MRP), binwidth = 1, 
                                    fill = "blue") 
plot_grid(p1, p2, p3, nrow = 1) # plot_grid() from cowplot package
#above code runs perfectly with proper indenting as shown above
#there seems to be no clear pattern in item_weight
#item_Visibility is right-skewed and should be transformed to curb its skewness
#We see 4 different distributions for Item_MRP.


#Independent Variables(Categorical Variables)
#plotting Item_Fat_Content, since its categorical, we use bar chart
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
#Its seen that, ‘LF’, ‘low fat’, and ‘Low Fat’ are the same category 
#and can be combined into one. 
#Similarly ‘reg’ and ‘Regular’ can be done into one
#and than we plot
combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat" 
combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular" 
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + 
  geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
#Run the code, and see the bar chart

#Now let’s check the other categorical variables.
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") +  xlab("") +  
  geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  
  ggtitle("Item_Type")# plot for Item_Type

p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))# plot for Outlet_Identifier

p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))# plot for Outlet_Size

second_row = plot_grid(p5, p6, nrow = 1) 
plot_grid(p4, second_row, ncol = 1)
#Run above the code, with exact indenting
#Insight: In Outlet_Size’s plot, for 4016 observations, Outlet_Size is blank or missing

#Checking the remaining categorical variables.
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) +   
  geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") +
  geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) +  
  xlab("Outlet_Establishment_Year") +  
  theme(axis.text.x = element_text(size = 8.5))# plot for Outlet_Establishment_Year

p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) +   
  geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") +  
  geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) +  
  theme(axis.text.x = element_text(size = 8.5))# plot for Outlet_Type

plot_grid(p7, p8, ncol = 2)#Plotting both graphs together
#Insights: Lesser number of observations in the data for the outlets established in the year 1998 as compared to the other years.
#Insights: Supermarket Type 1 seems to be the most popular category of Outlet_Type


#--------------------------------------------------------------------------------------------
#BiVariate Analysis
#explore the independent variables with respect to the target variable
#We will make use of scatter plots for the continuous or numeric variables and 
#violin plots for the categorical variables.
train = combi[1:nrow(train)] # extracting train data from the combined data
#Let’s explore the numerical variables first.
p9 = ggplot(train) +    
  geom_point(aes(Item_Weight, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +     
  theme(axis.title = element_text(size = 8.5))# Item_Weight vs Item_Outlet_Sales

p10 = ggplot(train) +       
  geom_point(aes(Item_Visibility, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))# Item_Visibility vs Item_Outlet_Sales

p11 = ggplot(train) +       
  geom_point(aes(Item_MRP, Item_Outlet_Sales), colour = "violet", alpha = 0.3) +      
  theme(axis.title = element_text(size = 8.5))# Item_MRP vs Item_Outlet_Sales

second_row_2 = plot_grid(p10, p11, ncol = 2) 
plot_grid(p9, second_row_2, nrow = 2)
#Insights: Item_Outlet_Sales is spread well across the entire range of the Item_Weight without any obvious pattern
#Insights: In Item_Visibility vs Item_Outlet_Sales, there is a string of points at Item_Visibility = 0.0 which seems strange as item visibility cannot be completely zero.
#Insights: In the third plot of Item_MRP vs Item_Outlet_Sales, we can clearly see 4 segments of prices that can be used in feature engineering to create a new variable


#Target Variable vs Independent Categorical Variables
#Lets visualize, categorical variables with respect to Item_Outlet_Sales.
#We use violin plots
#categorical variables with respect to Item_Outlet_Sales
#The height of a violin tells us about the range of the target variable values.
p12 = ggplot(train) +       
  geom_violin(aes(Item_Type, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),            
        axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8.5))# Item_Type vs Item_Outlet_Sales

p13 = ggplot(train) +       
  geom_violin(aes(Item_Fat_Content, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),            
        axis.text = element_text(size = 8),            
        axis.title = element_text(size = 8.5))# Item_Fat_Content vs Item_Outlet_Sales

p14 = ggplot(train) +       
  geom_violin(aes(Outlet_Identifier, Item_Outlet_Sales), fill = "magenta") +      
  theme(axis.text.x = element_text(angle = 45, hjust = 1),            
        axis.text = element_text(size = 8), 
        axis.title = element_text(size = 8.5))# Outlet_Identifier vs Item_Outlet_Sales

second_row_3 = plot_grid(p13, p14, ncol = 2)
plot_grid(p12, second_row_3, ncol = 1)
#Insights
#1. Distribution of Item_Outlet_Sales across the categories of Item_Type is not very distinct 
#and same is the case with Item_Fat_Content.
#2. The distribution for OUT010 and OUT019 categories of Outlet_Identifier are quite similar 
#and very much different from the rest of the categories of Outlet_Identifier.

#Let’s check the distribution of the target variable across Outlet_Size.
ggplot(train) + geom_violin(aes(Outlet_Size, Item_Outlet_Sales), fill = "magenta")
#Insights: The distribution of ‘Small’ Outlet_Size is almost identical to the distribution 
#of the blank category (first vioin) of Outlet_Size. 
#Therefore we can substitute the blanks in Outlet_Size with ‘Small’.
#Remember this is not the only way to impute missing values

#Let’s examine the remaining variables.
p15 = ggplot(train) + geom_violin(aes(Outlet_Location_Type, Item_Outlet_Sales), 
      fill = "magenta") 
p16 = ggplot(train) + geom_violin(aes(Outlet_Type, Item_Outlet_Sales), 
      fill = "magenta") 
plot_grid(p15, p16, ncol = 1)
#Insights:
#1. Tier 1 and Tier 3 locations of Outlet_Location_Type look similar.
#2. In the Outlet_Type plot, Grocery Store has most of its data points around the lower sales values as compared to the other categories.

#So far we been doing data analysis on the given data
#Missing Values
sum(is.na(combi$Item_Weight))
#missing values: 2439
sum(is.na(combi$Item_Outlet_Sales))
#missing values: 5681

#lets impute at the moment Item_Weight
missing_index = which(is.na(combi$Item_Weight))
 for(i in missing_index)
   {
     item = combi$Item_Identifier[i]
     combi$Item_Weight[i]=mean(combi$Item_Weight[combi$Item_Identifier==item],na.rm=T)
 }

#Now let’s see if there is still any missing data in Item_Weight
sum(is.na(combi$Item_Weight))
#results 0

#Replacing 0’s in Item_Visibility variable
#zeroes in Item_Visibility variable can be replaced with Item_Identifier wise mean values 
#of Item_Visibility
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)

#Let’s replace the zeroes.
zero_index = which(combi$Item_Visibility == 0)
 for(i in zero_index)
 {    
   item = combi$Item_Identifier[i]  
   combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)  
  }
#After the replacement of zeroes, We’ll plot the histogram of Item_Visibility 
ggplot(combi) + geom_histogram(aes(Item_Visibility), bins = 100)
#hence the 0 values have been resolved as per the histogram

#Feature Engineering
#Most of the times, 
#the given features in a dataset are not sufficient to give satisfactory predictions
#Therefore, we have to create new features which might help..
#in improving the model’s performance
#Let’s try to create some new features for our dataset
#1. Item_Type_new: Broader categories for the variable Item_Type.
#2. Item_category: Categorical variable derived from Item_Identifier
#3. Outlet_Years: Years of operation for outlets.
#4. price_per_unit_wt: Item_MRP/Item_Weight
#5. Item_MRP_clusters: Binned feature for Item_MRP
#Lets look at the Item_Type variable and classify the categories into perishable and non_perishable
perishable = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood")
non_perishable = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Health and Hygiene", "Household", "Soft Drinks")

# create a new feature 'Item_Type_new'
combi[,Item_Type_new := ifelse(Item_Type %in% perishable, "perishable", 
                               ifelse(Item_Type %in% non_perishable, "non_perishable", "not_sure"))]

#Let’s compare Item_Type with the first 2 characters of Item_Identifier, 
#i.e., ‘DR’, ‘FD’, and ‘NC’. 
#These identifiers most probably stand for drinks, food, and non-consumable.
table(combi$Item_Type, substr(combi$Item_Identifier, 1, 2))
#Based on the above table we can create a new feature. Let’s call it Item_category.
combi[,Item_category := substr(combi$Item_Identifier, 1, 2)]

#Lets change the values of Item_Fat_Content wherever Item_category is ‘NC’ 
#because non-consumable items cannot have any fat content
#lets create more features
#Outlet_Years (years of operation) and price_per_unit_wt (price per unit weight)
combi$Item_Fat_Content[combi$Item_category == "NC"] = "Non-Edible"
# years of operation for outlets
combi[,Outlet_Years := 2013 - Outlet_Establishment_Year]
combi$Outlet_Establishment_Year = as.factor(combi$Outlet_Establishment_Year)
# Price per unit weight
combi[,price_per_unit_wt := Item_MRP/Item_Weight]

#Earlier in the Item_MRP vs Item_Outlet_Sales plot, 
#we saw Item_MRP was spread across in 4 chunks.
#let’s assign a label to each of these chunks and use this label as a new variable.
# creating new independent variable - Item_MRP_clusters 
combi[,Item_MRP_clusters := ifelse(Item_MRP < 69, "1st",
                                   ifelse(Item_MRP >= 69 & Item_MRP < 136, "2nd",
                                          ifelse(Item_MRP >= 136 & Item_MRP < 203, "3rd", "4th")))]

#Encoding Categorical Variables
#it is essential to treat the categorical variables present in the data. 
#Label Encoding and One Hot Encoding, with these two steps, we can convert 
#categorical variables into numerical ones
#We will label encode Outlet_Size and Outlet_Location_Type as these are ordinal variables.
combi[,Outlet_Size_num := ifelse(Outlet_Size == "Small", 0,                                 
                                 ifelse(Outlet_Size == "Medium", 1, 2))] 
combi[,Outlet_Location_Type_num := ifelse(Outlet_Location_Type == "Tier 3", 0,                                          
                                          ifelse(Outlet_Location_Type == "Tier 2", 1, 2))] 

# removing categorical variables after label encoding
combi[, c("Outlet_Size", "Outlet_Location_Type") := NULL]

#One hot encoding for the categorical variable
ohe = dummyVars("~.", data = combi[,-c("Item_Identifier",
                                  "Outlet_Establishment_Year", "Item_Type")], fullRank = T)
ohe_df = data.table(predict(ohe, combi[,-c("Item_Identifier",
                                           "Outlet_Establishment_Year", "Item_Type")]))
combi = cbind(combi[,"Item_Identifier"], ohe_df)

#PreProcessing Data
#Its the step applied before feeding to the algorithm
#(skewness and scale the numerical variables)

#Removing Skewness
combi[,Item_Visibility := log(Item_Visibility + 1)]
# log + 1 to avoid division by zero
combi[,price_per_unit_wt := log(price_per_unit_wt + 1)]

#Scaling numeric predictors
num_vars = which(sapply(combi, is.numeric)) # index of numeric features
num_vars_names = names(num_vars)
combi_numeric = combi[,setdiff(num_vars_names, "Item_Outlet_Sales"), with = F]
prep_num = preProcess(combi_numeric, method=c("center", "scale"))
combi_numeric_norm = predict(prep_num, combi_numeric)

combi[,setdiff(num_vars_names, "Item_Outlet_Sales") := NULL]#removing numeric independent variables
combi = cbind(combi, combi_numeric_norm)

#Splitting the combined data combi back to train and test set
train = combi[1:nrow(train)] 
test = combi[(nrow(train) + 1):nrow(combi)]
test[,Item_Outlet_Sales := NULL]#removing Item_Outlet_Sales as it contains only NA for test dataset

#Correlated Variables
#It is not desirable to have correlated features if we are using linear regressions.
cor_train = cor(train[,-c("Item_Identifier")])
corrplot(cor_train, method = "pie", type = "lower", tl.cex = 0.9)
#Insights:
#The correlation between any two variables is represented by a pie
#A blueish pie indicates positive correlation and reddish pie indicates negative correlation. 
#the magnitude of the correlation is denoted by the area covered by the pie.
#Variables price_per_unit_wt and Item_Weight are highly correlated as the former one was created from the latter
#price_per_unit_wt and Item_MRP are highly correlated for the same reason.


#Model Building
#Linear Regression
linear_reg_mod = lm(Item_Outlet_Sales ~ ., data = train[,-c("Item_Identifier")])
#Making Predictions on test Data
# preparing dataframe for submission and writing it in a csv file
pred = predict(linear_reg_mod, test[,-c("Item_Identifier")])
pred

#Random Forest
set.seed(1237)
my_control = trainControl(method="cv", number=5) # 5-fold CV 
tgrid = expand.grid(
  .mtry = c(3:10),
  .splitrule = "variance",
  .min.node.size = c(10,15,20)
)
rf_mod = train(x = train[, -c("Item_Identifier", "Item_Outlet_Sales")],
               y = train$Item_Outlet_Sales,
               method='ranger',
               trControl= my_control,
               tuneGrid = tgrid,
               num.trees = 400,
               importance = "permutation")
rf_mod
plot(rf_mod)

#Variable Importance
plot(varImp(rf_mod))
#Insight: 
#Item_MRP is the most important variable in predicting the target variable
#New features created by us, like price_per_unit_wt, Outlet_Years, Item_MRP_Clusters, 
#are also among the top most important variables.
#This is why feature engineering plays such a crucial role in predictive modeling.


#XGBOOST
param_list = list(
  objective = "reg:linear",        
  eta=0.01,        
  gamma = 1,        
  max_depth=6,        
  subsample=0.8,        
  colsample_bytree=0.5        
  )
dtrain = xgb.DMatrix(data = as.matrix(train[,-c("Item_Identifier", "Item_Outlet_Sales")]),
                     label= train$Item_Outlet_Sales)
dtest = xgb.DMatrix(data = as.matrix(test[,-c("Item_Identifier")]))

#Cross Validation
#We are going to use the xgb.cv() function for cross validation
#This function comes with the xgboost package itself
set.seed(112) 
xgbcv = xgb.cv(params = param_list,
               data = dtrain,                
               nrounds = 1000,                
               nfold = 5,                
               print_every_n = 10,                
               early_stopping_rounds = 30,                
               maximize = F)
#as per the verbose above, we got the best validation/test score at the 424th iteration
#Hence, we will use nrounds = 424 for building the XGBoost model.
xgb_model = xgb.train(data = dtrain, params = param_list, nrounds = 430)
xgb_model

var_imp = xgb.importance(feature_names = setdiff(names(train), c("Item_Identifier", "Item_Outlet_Sales")),
                         model = xgb_model) 
xgb.plot.importance(var_imp)
#Insights
#Again the features created by us, like price_per_unit_wt, Outlet_Years, 
#Item_MRP_Clusters, are among the top most important variables.




                             

























































