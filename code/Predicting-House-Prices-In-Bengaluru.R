
# Problem Statement: 
# With the given 9 features(categorical and continuous) build a model to predict the price of houses in Bengaluru.

# Setting the working directory
getwd()
setwd("C:/Users/DELL/Desktop/R_projects/House_prices_Bengaluru")

#Loading data for predicting house prices in Bengaluru - train data
prices_train <- read.csv("Predicting-House-Prices-In-Bengaluru-Train-Data.csv")

#### Exploratory Data Analysis

#checking the dimension of train data
dim(prices_train)

#looking top 5 rows
head(prices_train)

# Checking the structure and the summary of the data
str(prices_train)
summary(prices_train)
sapply(prices_train, is.numeric)

# Scatter Plot
pairs(~ price + area_type + availability + location +
        size + society + total_sqft  + bath +
        balcony , data = prices_train)

# Scatter Plot
pairs(~ price + total_sqft  + bath +
        balcony, data = prices_train)

# install.packages("GGally")
library(GGally)
ggcorr(prices_train, palette = "RdBu", label = TRUE)
ggpairs(prices_train[,7:9])

# checking for outliers
summary(prices_train$bath)
table(prices_train$bath)

summary(prices_train$balcony)
table(prices_train$balcony)

summary(as.numeric(prices_train$total_sqft))
table(prices_train$total_sqft)

# Finding names of columns which contain missing values
names(which(sapply(prices_train, anyNA)))
which(is.na(prices_train$bath))
which(is.na(prices_train$balcony))
sum(is.na(prices_train$bath))
sum(is.na(prices_train$balcony))

# Scatter Plot
ggplot(prices_train, aes(x=total_sqft, y= price)) + geom_point()

# OBSERVATIONS 
# There are some outliers but they do not affect our data, so we leave them as it is (extreme but not bad)
# bath and balcony has some missing values
# price is not uniformly distributed
# price has some other functional relationship with total_sqft

## Data Cleaning and Data Pre-processing

# Missing Value Imputation

# Imputing mean value to the missing values in the bath column
unique(prices_train$bath)
# As number of NAs are less so it can be replaced by mean value of bath
m_bath <- mean(prices_train$bath, na.rm = TRUE)
prices_train$bath[is.na(prices_train$bath)] <- m_bath
# We have removed all the na values from the bath column

# Imputing mean value to the missing values in the balcony column
unique(prices_train$balcony)
#As number of NAs are less so it can be replaced by mean value of balcony
m_balcony <- mean(prices_train$balcony, na.rm = TRUE)
prices_train$balcony[is.na(prices_train$balcony)] <- m_balcony
# We have removed all the na values from the balcony column

# Encoding categorical data

# area_type
unique(prices_train$area_type)
levels(prices_train$area_type)
prices_train$area_type <- factor(prices_train$area_type,
                                 levels = c("Built-up  Area", "Carpet  Area", "Plot  Area", "Super built-up  Area"),
                                 labels = c(1, 2, 3, 4))
head(prices_train$area_type)
#There are four different area types are converted to numerical representation for calculation purpose
#Built-up Area = 1
#Carpet Area = 2
#Plot Area = 3
#Super built-up Area = 4

#availability
unique(prices_train$availability)
levels(prices_train$availability)
# Converting the categorical values in the column 'availability' into 3 categories
# i.e. Ready to move, Immediate Possession, Dates and encoding them into 0, 1, 2.

replace_availability <- function(availability){
  if (availability == 'Ready To Move') return(0)
  else if (availability == 'Immediate Possession') return(1)
  else 
    return(2)
}

prices_train$availability  <- sapply(prices_train$availability , replace_availability)

prices_train$availability <- as.factor(prices_train$availability)
head(prices_train$availability)
unique(prices_train$availability)
levels(prices_train$availability)

# size
unique(prices_train$size)
levels(prices_train$size)
# Size is basically the number of bedrooms
# so creating a new column named bedroom 
# and deleting the size column

nrow(prices_train)

prices_train$bedroom <- 0
for (i in c(1:13320)){
  prices_train$bedroom[i] = as.numeric(strsplit(as.character(prices_train$size)," ")[[i]][1])
}

# Removing size column as it is replaced by bedroom column
prices_train <-  prices_train[,-4]

# Imputing mean value to the missing values in the bedroom column
unique(prices_train$bedroom)
m_bedroom <- mean(prices_train$bedroom, na.rm = TRUE)
prices_train$bedroom[is.na(prices_train$bedroom)] <- m_bedroom

# total_sqft
unique(prices_train$total_sqft)
#As some of the rows in the total_sqft shows the range, so taking the mean of them for calculation purpose.

prices_train$total_sqfts <- 0
for (i in c(1 : 13320)){
  prices_train$total_sqfts[i] <- mean(as.numeric(strsplit(as.character(prices_train$total_sqft), "-")[[i]]))
}

#removing total_sqft column as it is no longer needed as it is replaced by total_sqfts column
prices_train = prices_train[,-5]

# Checking if there is any missing values
any(is.na(prices_train$total_sqfts))
sum(is.na(prices_train$total_sqfts))

#As number of NAs are less so it can be replaced by mean value of total_sqfts
m_total_sqfts = mean(prices_train$total_sqfts, na.rm = TRUE)
prices_train$total_sqfts[is.na(prices_train$total_sqfts)] = m_total_sqfts

## Visualizing the correlation data
library(corrplot) #all variables must be numeric

# Checking the Correlation between the numeric variables
# Num only
num.cols <- sapply(prices_train,is.numeric)
# Filtering the numeric columns for correlation
cor.data <- cor(prices_train[,num.cols])
print(cor.data)

corrplot(cor.data, type = "lower")
corrplot(cor.data, method = "number")
corrplot(cor.data, method = "color")

library(corrgram)
corrgram(prices_train,order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

## Data Visualization
library(ggplot2)

# Price Distribution Histogram
ggplot(prices_train, aes(x = price), binwidth = 2 ) + 
  geom_histogram(aes(y = ..density..),fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('House Prices'))) + 
  ylab(expression(bold('Density')))
# The distribution of price looks right skewed

# Price Distribution Histogram with log (normalised)
ggplot(prices_train, aes(x = log(price)), binwidth = 2) + 
  geom_histogram(aes(y = ..density..),fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('House Prices Normalised'))) + 
  ylab(expression(bold('Density')))
# We took the log to make it normalised or a uniformed distribution

# total_sqfts Distribution Histogram
ggplot(prices_train, aes(x = total_sqfts), binwidth = 2 ) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Area of the House'))) + 
  ylab(expression(bold('Density')))

# total_sqfts Distribution Histogram with log (normalised)
ggplot(prices_train, aes(x = log(total_sqfts)), binwidth = 2 ) + 
  geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + 
  geom_density(colour = 'blue') + xlab(expression(bold('Area of the House'))) + 
  ylab(expression(bold('Density')))
# We took the log and got somewhat a normalised distribution

# Bar Plot
ggplot(prices_train, aes(x = area_type, fill = factor(balcony))) + geom_bar(position = "dodge", alpha = 0.5) 
# There are four different area types, but most of the houses are in Super built-up Area 
# and they usually have either 1 or 2 balcony

# Bar Plot
ggplot(prices_train, aes(x = area_type, fill = factor(availability))) + geom_bar(position = "dodge", alpha = 0.5) 
# There are four different area types and availability status is Ready to move for most of them
# Mostly for houses are in Super built-up Area

# Scatter Plot
ggplot(prices_train, aes(x = price, y = total_sqfts, color = availability)) + geom_point(alpha= 0.5)
# It looks densed on only one side of the plot

# Scatter Plot with log of the dependent variable
ggplot(prices_train, aes(x = log(price), y = total_sqfts, color = availability)) + geom_point(alpha= 0.5)
# taking log of the price variable to get some linear relationship

# Scatter Plot
ggplot(prices_train, aes(x = price, y = total_sqfts, color = area_type)) + geom_point(alpha= 0.5)
# It looks densed on only one side of the plot

# Scatter Plot with log of the dependent variable
ggplot(prices_train, aes(x = log(price), y = total_sqfts, color = area_type)) + geom_point(alpha= 0.5)
# taking log of the price variable to get some linear relationship


#### TEST DATA

#Loading data for predicting house prices in Bengaluru - train data
prices_test <- read.csv("Predicting-House-Prices-In-Bengaluru-Test-Data.csv")

#### Exploratory Data Analysis

#checking the dimension of train data
dim(prices_test)

#looking top 5 rows
head(prices_test)

# Checking the structure and the summary of the data
str(prices_test)
summary(prices_test)
sapply(prices_test, is.numeric)

# checking for outliers
summary(prices_test$bath)
table(prices_test$bath)

summary(prices_test$balcony)
table(prices_test$balcony)

summary(as.numeric(prices_test$total_sqft))
table(prices_test$total_sqft)

# Finding names of columns which contain missing values
names(which(sapply(prices_test, anyNA)))
which(is.na(prices_test$bath))
sum(is.na(prices_test$bath))
which(is.na(prices_test$balcony))
sum(is.na(prices_test$balcony))

## Data Cleaning and Data Pre-processing

# Missing Value Imputation

# Imputing mean value to the missing values in the bath column
unique(prices_test$bath)
# As number of NAs are less so it can be replaced by mean value of bath
mu_bath <- mean(prices_test$bath, na.rm = TRUE)
prices_test$bath[is.na(prices_test$bath)] <- mu_bath
# We have removed all the na values from the bath column

# Imputing mean value to the missing values in the balcony column
unique(prices_test$balcony)
# As number of NAs are less so it can be replaced by mean value of balcony
mu_balcony <- mean(prices_test$balcony, na.rm = TRUE)
prices_test$balcony[is.na(prices_test$balcony)] <- mu_balcony
# We have removed all the na values from the balcony column

# Encoding categorical data

# area_type
unique(prices_test$area_type)
levels(prices_test$area_type)
prices_test$area_type <- factor(prices_test$area_type,
                                 levels = c("Built-up  Area", "Carpet  Area", "Plot  Area", "Super built-up  Area"),
                                 labels = c(1, 2, 3,4))
head(prices_test$area_type)

# availability
unique(prices_test$availability)
levels(prices_test$availability)

prices_test$availability  <- sapply(prices_test$availability , replace_availability)

prices_test$availability <- as.factor(prices_test$availability)
head(prices_test$availability)

unique(prices_test$availability)
levels(prices_test$availability)
any(is.na(prices_test$availability))

# size
unique(prices_test$size)
levels(prices_test$size)
table(prices_test$size)

nrow(prices_test)

prices_test$bedroom <- 0
for (i in c(1:1480)){
  prices_test$bedroom[i] = as.numeric(strsplit(as.character(prices_test$size)," ")[[i]][1])
}

any(is.na(prices_test$bedroom))
summary(prices_test$bedroom)
table(prices_test$bedroom)
# Removing size column as it is replaced by bedroom column
prices_test <-  prices_test[,-4]

# Imputing mean value to the missing values in the bedroom column
unique(prices_test$bedroom)
mu_bedroom <- mean(prices_test$bedroom, na.rm = TRUE)
prices_test$bedroom[is.na(prices_test$bedroom)] <- mu_bedroom

# total_sqft
unique(prices_train$total_sqft)
summary(prices_test$total_sqft)
table(prices_test$total_sqft)
#As some of the rows in the total_sqft shows the range, so taking the mean of them for calculation purpose.

prices_test$total_sqfts <- 0
for (i in c(1 : 1480)){
  prices_test$total_sqfts[i] <- mean(as.numeric(strsplit(as.character(prices_test$total_sqft), "-")[[i]]))
}

#removing total_sqft column as it is no longer needed as it is replaced by total_sqfts column
prices_test = prices_test[,-5]

# Checking if there is any missing values
any(is.na(prices_test$total_sqfts))
sum(is.na(prices_test$total_sqfts))

#As number of NAs are less so it can be replaced by mean value of total_sqfts
mu_total_sqfts = mean(prices_test$total_sqfts, na.rm = TRUE)
prices_test$total_sqfts[is.na(prices_test$total_sqfts)] = mu_total_sqfts

str(prices_test)
summary(prices_test)
summary(prices_test$total_sqfts)

# Final na check
any(is.na(prices_test))
names(which(sapply(prices_test, anyNA)))

# Creating a copy
train_copy <- prices_train
test_copy <- prices_test

#### Model Building
library(Metrics)

# Liner Models
model_0 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = prices_train)
model_0
summary(model_0)
price_m0 <-predict(model_0)
head(price_m0)

rmse(prices_train$price, price_m0)
rmsle(prices_train$price, price_m0)
pred_pricem0 <- predict(model_0, newdata = prices_test)
head(pred_pricem0)

model_1 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = prices_train)
model_1
summary(model_1)
price_m1 <- predict(model_1)
price_m1 <- exp(price_m1)
head(price_m1)

rmse(prices_train$price, price_m1)
rmsle(prices_train$price, price_m1)
pred_pricem1 <- predict(model_1, newdata = prices_test)
pred_pricem1 <- exp(pred_pricem1)
head(pred_pricem1)

# Decision Tree Model
library(rpart)
set.seed(100)
dt0 <- rpart(formula = price ~ area_type + availability + bath + balcony + bedroom + total_sqfts,
             data = prices_train)
summary(dt0)
plot(dt0)
text(dt0)
 
price_dt0 <- predict(dt0)
head(price_dt0)
rmse(prices_train$price, price_dt0)
rmsle(prices_train$price, price_dt0)
pred_pricedt0 <- predict(dt0, newdata = prices_test)
head(pred_pricedt0)

# Random Forest Model
library(randomForest)
set.seed(100)
rf0 <-randomForest(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, 
                   data = prices_train , ntree=500) 
summary(rf0)
plot(rf0)

price_rf0 <- predict(rf0)
head(price_rf0)

rmse(prices_train$price, price_rf0)
rmsle(prices_train$price, price_rf0)

pred_pricerf0 <- predict(rf0, newdata = prices_test)
head(pred_pricerf0)


# Generalized Boosted Models (GBM)
#install.packages('gbm')
library(gbm)

fit_gbm0 <- gbm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts
                , distribution = "gaussian"
                , n.trees = 1000
                , shrinkage = 0.01
                , interaction.depth = 4
                , n.minobsinnode = 10
                , data=prices_train)

fit_gbm0
plot(fit_gbm0)
summary(fit_gbm0)

price_gbm0 <- predict(fit_gbm0,n.trees = 1000)
head(price_gbm0)

rmse(prices_train$price, gbm_price0)
rmsle(prices_train$price, gbm_price0)

pred_pricegbm0 <- predict(fit_gbm0, newdata = prices_test, n.trees = 1000)
head(pred_pricegbm0)

#Xgboost model

# Data Preparation for Xgboost model
# The outcome column
(outcome <- "price")

# The input columns
(vars <- c("area_type", "availability", "bath", "balcony", "bedroom", "total_sqfts"))

# Load the packages
library(vtreat)
library(dplyr)
library(magrittr)

# Create the treatment plan from prices_train (the training data)
treatplan <- designTreatmentsZ(prices_train, vars, verbose = FALSE)

# Get the "clean" and "lev" variables from the scoreFrame
(newvars <- treatplan %>%
    use_series(scoreFrame) %>%        
    filter(code %in% c("clean", "lev")) %>%  # get the rows you care about
    use_series(varName))          # get the varName column

# Prepare the training data
train_treat <- prepare(treatplan, prices_train,  varRestriction = newvars)

# Prepare the test data
test_treat <- prepare(treatplan, prices_test,  varRestriction = newvars)

# Call str() on the treated data
str(train_treat)
str(test_treat)

# Load the package xgboost
library(xgboost)

# Run xgb.cv
cv <- xgb.cv(data = as.matrix(train_treat), 
             label = prices_train$price,
             nrounds = 500,
             nfold = 10,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 3,
             early_stopping_rounds = 10,
             verbose = 0    # silent
)

# Get the evaluation log 
elog <- as.data.frame(cv$evaluation_log)

# Determine and print how many trees minimize training and test error
elog %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)
# In most cases, ntrees.test is less than ntrees.train. 
# The training error keeps decreasing even after the test error starts to increase.
# It's important to use cross-validation to find the right number of trees (as determined by ntrees.test) 
# and avoid an overfit model.

# The number of trees to use, as determined by xgb.cv
# 12

# Run xgboost
price_model_xgb <- xgboost(data = as.matrix(train_treat), # training data as matrix
                          label = prices_train$price,  # column of outcomes
                          nrounds = 12,       # number of trees to build
                          objective = "reg:linear", # objective
                          eta = 0.3,
                          depth = 6,
                          verbose = 0  # silent
)

# Make predictions
prices_test$pred <- predict(price_model_xgb, as.matrix(test_treat))
head(prices_test$pred)

prices_train$pred <- predict(price_model_xgb, as.matrix(train_treat))
head(prices_train$pred)

# Plot predictions (on x axis) vs actual bike rental count
ggplot(prices_train, aes(x = pred, y = price)) + 
  geom_point(alpha= 0.3) + 
  geom_abline()

# Calculate RMSE
rmse_fin0 <- rmse(prices_train$price, prices_train$pred)
rmsle_fin0 <- rmsle(prices_train$price, prices_train$pred)

# Creating a sample submission file(1)
# create a dataframe with our results
Price_1 <- prices_test$pred
my_submission1 <- as.data.frame(Price_1)
head(my_submission1)
# save our file
write.csv(my_submission1, 'submission1.csv')

######## ONE LAST TRY ################
# making a new df 
train_copy <- prices_train[-10]
str(train_copy)
test_copy <- prices_test[-10]
str(test_copy)

# Feature Engineering

train_copy$location <- as.numeric(factor(train_copy$location))
table(train_copy$location)
summary(train_copy$location)

train_copy$society <- as.numeric(factor(train_copy$society))
table(train_copy$society)
summary(train_copy$society)

test_copy$location <- as.numeric(factor(test_copy$location))
table(test_copy$location)
summary(test_copy$location)

test_copy$society <- as.numeric(factor(test_copy$society))
table(test_copy$society)
summary(test_copy$society)

# Model

# Decision Tree Model
library(rpart)
set.seed(100)
dt1 <- rpart(formula = price ~ .,
             data = train_copy)
summary(dt1)
plot(dt1)
text(dt1)

price_dt1 <- predict(dt1)
head(price_dt1)
rmse(train_copy$price, price_dt1)
rmsle(train_copy$price, price_dt1)
pred_pricedt1 <- predict(dt1, newdata = test_copy)

# Random Forest Model
library(randomForest)
set.seed(100)
rf1 <-randomForest(price ~ ., 
                   data = train_copy , ntree=500) 
rf1
summary(rf1)
plot(rf1)

price_rf1 <- predict(rf1)
head(price_rf1)

rmse(train_copy$price, price_rf1)
rmsle(train_copy$price, price_rf1)

pred_pricerf1 <- predict(rf1, newdata = test_copy)
head(pred_pricerf1)


# Generalized Boosted Models (GBM)
#install.packages('gbm')
library(gbm)

fit_gbm1 <- gbm(price ~ .
                , distribution = "gaussian"
                , n.trees = 1000
                , shrinkage = 0.01
                , interaction.depth = 4
                , n.minobsinnode = 10
                , data=train_copy)

fit_gbm1
plot(fit_gbm1)
summary(fit_gbm1)

price_gbm1 <- predict(fit_gbm1,n.trees = 1000)
head(price_gbm1)

rmse(train_copy$price, price_gbm1)
rmsle(train_copy$price, price_gbm1)

pred_pricegbm1 <- predict(fit_gbm1, newdata = test_copy, n.trees = 1000)
head(pred_pricegbm1)


#Xgboost model

# Data Preparation for Xgboost model
# The outcome column
(outcome1 <- "price")

# The input columns
(vars1 <- c("area_type", "availability", "location", "society", "bath", "balcony", "bedroom", "total_sqfts"))

# Load the packages
library(vtreat)
library(dplyr)
library(magrittr)

# Create the treatment plan from train_copy (the training data)
treatplan1 <- designTreatmentsZ(train_copy, vars1, verbose = FALSE)

# Get the "clean" and "lev" variables from the scoreFrame
(newvars1 <- treatplan1 %>%
    use_series(scoreFrame) %>%        
    filter(code %in% c("clean", "lev")) %>%  # get the rows you care about
    use_series(varName))          # get the varName column


# Prepare the training data
train_treat1 <- prepare(treatplan1, train_copy,  varRestriction = newvars1)

# Prepare the test data
test_treat1 <- prepare(treatplan1, test_copy,  varRestriction = newvars1)

# Call str() on the treated data
str(train_treat1)
str(test_treat1)

# Load the package xgboost
library(xgboost)

# Run xgb.cv
cv1 <- xgb.cv(data = as.matrix(train_treat1), 
             label = prices_train$price,
             nrounds = 500,
             nfold = 10,
             objective = "reg:linear",
             eta = 0.3,
             max_depth = 3,
             early_stopping_rounds = 10,
             verbose = 0    # silent
)

# Get the evaluation log 
elog1 <- as.data.frame(cv1$evaluation_log)

# Determine and print how many trees minimize training and test error
elog1 %>% 
  summarize(ntrees.train = which.min(train_rmse_mean),   # find the index of min(train_rmse_mean)
            ntrees.test  = which.min(test_rmse_mean))   # find the index of min(test_rmse_mean)
# In most cases, ntrees.test is less than ntrees.train. 
# The training error keeps decreasing even after the test error starts to increase.
# It's important to use cross-validation to find the right number of trees (as determined by ntrees.test) 
# and avoid an overfit model.

# The number of trees to use, as determined by xgb.cv
# 17


# Run xgboost
price_model_xgb1 <- xgboost(data = as.matrix(train_treat1), # training data as matrix
                           label = prices_train$price,  # column of outcomes
                           nrounds = 17,       # number of trees to build
                           objective = "reg:linear", # objective
                           eta = 0.3,
                           depth = 6,
                           verbose = 0  # silent
)

# Make predictions
test_copy$pred <- predict(price_model_xgb1, as.matrix(test_treat1))
head(test_copy$pred)

train_copy$pred <- predict(price_model_xgb1, as.matrix(train_treat1))
head(train_copy$pred)

# Plot predictions (on x axis) vs actual bike rental count
ggplot(train_copy, aes(x = pred, y = price)) + 
  geom_point(alpha= 0.3) + 
  geom_abline()

# Calculate RMSE
rmse_fin1 <- rmse(train_copy$pred, prices_train$pred)
rmsle_fin1 <- rmsle(train_copy$pred, prices_train$pred)

# Creating a sample submission file(2)
# create a dataframe with our results
Price_2 <- test_copy$pred
my_submission2 <- as.data.frame(Price_2)
head(my_submission2)
# save our file
write.csv(my_submission2, 'submission2.csv')
