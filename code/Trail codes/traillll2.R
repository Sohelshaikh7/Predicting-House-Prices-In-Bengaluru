
# # location can be converted to numerical representation for calculation purpose
# prices_train$location <- as.numeric(prices_train$location)
# unique(prices_train$location)
# any(is.na(prices_train$location))

# # society can be converted to numerical representation for calculation purpose
# prices_train$society <- as.numeric(prices_train$society)
# unique(prices_train$society)
# any(is.na(prices_train$location))

# Model building

trail_model_1 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = prices_train )
summary(trail_model_1)

trail_model_2 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = prices_train )
# summary(trail_model_2)
# 
# trail_model_3 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = last_try)
# summary(trail_model_3)
# 
# trail_model_101 <- lm(price ~., data = last_try)
# summary(trail_model_101)
# 
# trail_model_201 <- lm(log(price) ~., data = full_try)
# summary(trail_model_201)
# 
# trail_model_301 <- lm(price ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = last_try )
# summary(trail_model_301)
# 
# trail_model_302 <- lm(price ~ ., data = last_try )
# summary(trail_model_302)
# 
# trail_model_202 <- lm(log(price) ~ area_type + availability + bath + balcony + bedroom + total_sqfts, data = prices_train)
# summary(trail_model_202)

## random forest


##### PROBLEM STATEMENT: TO PREDICT THE PRICE OF A HOUSE #####
# Getting the current working Directory
getwd()
setwd("C:/Users/DELL/Desktop/R_projects/House_prices_Bengaluru")

# Reading the csv file
pt <- read.csv("Predicting-House-Prices-In-Bengaluru-Train-Data.csv")

#### Exploratory Data Analysis

#checking the dimension of train data
dim(pt)

#looking top 5 rows
head(pt)

# Checking the structure and the summary of the data
str(prices_train)
summary(prices_train)
sapply(prices_train, is.numeric)

pt$location <- as.numeric(factor(pt$location))

table(pt$location)
summary(pt$location)

pt$society <- as.numeric(factor(pt$society))

table(pt$society)
summary(pt$society)

str(pt)

# Imputing mean value to the missing values in the bath column
unique(pt$bath)
# As number of NAs are less so it can be replaced by mean value of bath
u_bath <- mean(pt$bath, na.rm = TRUE)
pt$bath[is.na(pt$bath)] <- u_bath
# We have removed all the na values from the bath column

# Imputing mean value to the missing values in the balcony column
unique(pt$balcony)
#As number of NAs are less so it can be replaced by mean value of balcony
u_balcony <- mean(pt$balcony, na.rm = TRUE)
pt$balcony[is.na(prices_train$balcony)] <- u_balcony
# We have removed all the na values from the balcony column

# Encoding categorical data

# area_type
unique(pt$area_type)
levels(pt$area_type)
pt$area_type <- factor(pt$area_type,
                                 levels = c("Built-up  Area", "Carpet  Area", "Plot  Area", "Super built-up  Area"),
                                 labels = c(1, 2, 3,4))
head(pt$area_type)

unique(pt$availability)
levels(pt$availability)

replace_availability <- function(availability){
  if (availability == 'Ready To Move') return(0)
  else if (availability == 'Immediate Possession') return(1)
  else 
    return(2)
}

pt$availability  <- sapply(pt$availability , replace_availability)

pt$availability <- as.factor(pt$availability)
head(pt$availability)

unique(pt$availability)
levels(pt$availability)
any(is.na(pt$availability))

unique(pt$size)
levels(pt$size)
table(pt$size)
nrow(pt)

pt$bedroom <- 0
for (i in c(1:13320)){
  pt$bedroom[i] = as.numeric(strsplit(as.character(pt$size)," ")[[i]][1])
}

any(is.na(pt$bedroom))
summary(pt$size)
summary(pt$bedroom)
table(pt$bedroom)

pt <-  pt[,-4]

unique(pt$bedroom)
u_bedroom <- mean(pt$bedroom, na.rm = TRUE)
pt$bedroom[is.na(pt$bedroom)] <- u_bedroom

# total_sqft
unique(pt$total_sqft)
#As some of the rows in the total_sqft shows the range, so taking the mean of them for calculation purpose.

pt$total_sqfts <- 0
for (i in c(1 : 13320)){
  pt$total_sqfts[i] <- mean(as.numeric(strsplit(as.character(pt$total_sqft), "-")[[i]]))
}

pt = pt[,-5]

any(is.na(pt$total_sqfts))
sum(is.na(pt$total_sqfts))

u_total_sqfts = mean(pt$total_sqfts, na.rm = TRUE)
pt$total_sqfts[is.na(pt$total_sqfts)] = u_total_sqfts

str(pt)
summary(pt)
summary(pt$total_sqfts)

any(is.na(pt))
library(Metrics)

# tm_0 <- lm(price ~ ., data = pt )
# summary(tm_0)
# pt$p0 <- predict(tm_0)
# rmse(pt$price, pt$p0)
# rmsle(pt$price, pt$p0)

tm_1 <- lm(log(price) ~ ., 
           data = pt )
summary(tm_1)
pt$p1 <- predict(tm_1)
rmse(pt$price, pt$p1)
rmsle(pt$price, pt$p1)

# tm_3 <- lm(log(price) ~ area_type + availability + location + society + bath + balcony + bedroom + log(total_sqfts), data = pt )
# summary(tm_3)
# pt$p3 <- predict(tm_3)
# rmse(pt$price, pt$p3)
# rmsle(pt$price, pt$p3)
# 
# ft<- pt
# sapply(ft, is.numeric)
# 
# ft[,9] <- scale(ft[,9])
# tm_4 <- lm(price ~ . , data = ft )
# summary(tm_4)
# ft$p4 <- predict(tm_4)
# rmse(ft$price, ft$p4)
# rmsle(ft$price, ft$p4)
# 
# tm_5 <- lm(log(price) ~ . , data = ft )
# summary(tm_5)
# ft$p5 <- predict(tm_5)
# rmse(ft$price, ft$p5)
# rmsle(ft$price, ft$p5)
# 
# ct <- pt
# ct[,3:9] <- scale(ct[, 3:9])
# 
# tm_6 <- lm(price ~ . , data = ct )
# summary(tm_6)
# ct$p6 <- predict(tm_6)
# rmse(ct$price, ct$p6)
# rmsle(ct$price, ct$p6)
# 
# tm_7 <- lm(log(price) ~ . , data = ct )
# summary(tm_7)
# ct$p7 <- predict(tm_7)
# rmse(ct$price, ct$p7)
# rmsle(ct$price, ct$p7)


library(randomForest)
set.seed(100)
r0 <-randomForest(log(price) ~ ., data = pt , ntree=500) 
print(r0)
summary(r0)
plot(r0)

pt$rpred <- predict(r0)
rmse(pt$price , pt$rpred)
rmsle(pt$price , pt$rpred)

# set.seed(100)
# r1 <-randomForest(price ~ ., data = pt , ntree=500) 
# print(r1)
# summary(r1)
# plot(r1)
# 
# pt$rpredf <- predict(r1)
# rmse(pt$price , pt$rpredf)
# rmsle(pt$price , pt$rpredf)

## decision tree

library(rpart)
dt0 = rpart(formula = log(price) ~ area_type + availability + location + society + bath + balcony + bedroom + total_sqfts, 
                       data = pt)
summary(dt0)
pt$dpred <- predict(dt0)
rmse(pt$price , pt$dpred)
rmsle(pt$price , pt$dpred)
plot(dt0)
text(dt0)
