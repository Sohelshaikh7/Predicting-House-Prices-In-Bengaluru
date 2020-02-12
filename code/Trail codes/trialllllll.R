##### PROBLEM STATEMENT: TO PREDICT THE PRICE OF A HOUSE #####
# Getting the current working Directory
getwd()
setwd("C:/Users/DELL/Desktop/R_projects/House_prices_Bengaluru")

# Reading the csv file
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
        balcony , 
        data = prices_train)

# Scatter Plot
pairs(~ price + total_sqft  + bath +
        balcony, 
      data = prices_train)

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
# bath and balcony has missing values
# total_sqft has some other functional relationship with price

## Data Cleaning

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
                                 labels = c(1, 2, 3,4))
head(prices_train$area_type)
#There are four different area types are converted to numerical representation for calculation purpose
# i.e One Hot Encoding
#Built-up Area = 1
#Carpet Area = 2
#Plot Area = 3
#Super built-up Area = 4

# Converting the categorical values in the column 'availability' into 3 categories
# i.e. Ready to move, Immediate Possession, Dates and encoding them into 0, 1, 2.

# availability
unique(prices_train$availability)
levels(prices_train$availability)

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
any(is.na(prices_train$availability))

# size
unique(prices_train$size)
levels(prices_train$size)
table(prices_train$size)
nrow(prices_train)
# Size is basically the number of bedrooms
# so creating a new column named bedroom 
# and deleting the size column

prices_train$bedroom <- 0
for (i in c(1:13320)){
  prices_train$bedroom[i] = as.numeric(strsplit(as.character(prices_train$size)," ")[[i]][1])
}

any(is.na(prices_train$bedroom))
summary(prices_train$size)
summary(prices_train$bedroom)
table(prices_train$bedroom)
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

str(prices_train)
summary(prices_train)
summary(prices_train$total_sqfts)

# Final na check
any(is.na(prices_train))

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

summary(prices_train$price)
summary(log(prices_train$price))

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
summary(prices_train$total_sqfts)
summary(log(prices_train$total_sqfts))

# Bar Plot
ggplot(prices_train, aes(x = area_type, fill = factor(balcony))) + geom_bar(position = "dodge", alpha = 0.5) 
# There are four different area types, but most of the houses are in Super built-up Area 
# and there are either 1 or 2 balcony in every category

# Bar Plot
ggplot(prices_train, aes(x = area_type, fill = factor(availability))) + geom_bar(position = "dodge", alpha = 0.5) 
# There are four different area types and availability status is Ready to move for most of them
# and some have dates when the house will be  availabile
# Mostly the houses are in Super built-up Area

# Scatter Plot
ggplot(prices_train, aes(x = price, y = total_sqfts, color = availability)) + geom_point(alpha= 0.5)
# It looks densed on only one side of the plot

# Scatter Plot with log of the dependent variable
ggplot(prices_train, aes(x = log(price), y = total_sqfts, color = availability)) + geom_point(alpha= 0.5)
# taking log of the price variable to get some linear relationship

# Scatter with log of the both variable
ggplot(prices_train, aes(x = log(price), y = log(total_sqfts), color = availability)) + geom_point(alpha= 0.5)
# taking log of the price variable to get some linear relationship

# Model Building
try <- prices_train

# trial_model0 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = try )
# summary(trial_model0)
# try$price_pred0 <- predict(trial_model0)
# 
# trial_model1 <- lm(price ~ area_type + availability + bath + balcony + total_sqfts, data = try )
# summary(trial_model1)
# try$price_pred1 <- predict(trial_model1)
# 
# library(Metrics)
# rmse(try$price, try$price_pred0)
# rmsle(try$price, try$price_pred0)
# 
# rmse(try$price, try$price_pred1)
# rmsle(try$price, try$price_pred1)

trial_model2 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = try )
summary(trial_model2)
try$price_pred2 <- predict(trial_model2)

rmse(try$price, try$price_pred2)
rmsle(try$price, try$price_pred2)

# trial_model3 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + log(total_sqfts), data = try )
# summary(trial_model3)
# try$price_pred3 <- predict(trial_model3)
# 
# rmse(try$price, try$price_pred3)
# rmsle(try$price, try$price_pred3)
# 
# fry<- prices_train
# sapply(fry, is.numeric)
# 
# fry[,9] <- scale(fry[,9])
# 
# trial_model4 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = fry )
# summary(trial_model4)
# fry$price_pred4 <- predict(trial_model4)
# 
# rmse(fry$price, fry$price_pred4)
# rmsle(fry$price, fry$price_pred4)
# 
# trial_model5 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = fry )
# summary(trial_model5)
# fry$price_pred4 <- predict(trial_model5)
# 
# rmse(fry$price, fry$price_pred5)
# rmsle(fry$price, fry$price_pred5)  
# 
# cry <- prices_train
# cry[, c(7,9)] <- scale(cry[, c(7,9)])
# 
# trial_model6 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = cry )
# summary(trial_model6)
# cry$price_pred6 <- predict(trial_model6)
# 
# rmse(cry$price, cry$price_pred6)
# rmsle(cry$price, cry$price_pred6)
# 
# trial_model7 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = cry )
# summary(trial_model7)
# cry$price_pred7 <- predict(trial_model7)
# 
# rmse(cry$price, cry$price_pred7)
# rmsle(cry$price, cry$price_pred7)
# 
# nri <- prices_train
# nri[,5:9] <- scale(nri[,5:9])
# 
# trial_model8 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = nri )
# summary(trial_model8)
# nri$price_pred8 <- predict(trial_model8)
# 
# rmse(nri$price, nri$price_pred8)
# rmsle(nri$price, nri$price_pred8)

# trial_model2 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = try )
# summary(trial_model2)

library(randomForest)
set.seed(100)
rf0 <-randomForest(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = try , ntree=500) 
print(rf0)
summary(rf0)
plot(rf0)

# trial_model6 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = cry )
# summary(trial_model6)
#
# set.seed(100)
# rf1 <-randomForest(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = cry, ntree=500) 
# print(rf1)
# summary(rf1)
# plot(rf1)
#
# trial_model8 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = nri )
# summary(trial_model8)
# 
# set.seed(100)
# rf2 <-randomForest(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = nri , ntree=500) 
# print(rf2)
# summary(rf2)
# plot(rf2)
# 
# str(prices_train)
