
#### Problem Statement: 
# With the given 9 features(categorical and continuous) build a model to predict the price of houses in Bengaluru.

## Data :
# The train and test data will consist of various features that describe that property in Bengaluru. 
# This is an actual data set that is curated over months of primary & secondary research by our team. 
# Each row contains fixed size object of features. 
# There are 9 features and each feature can be accessed by its name.

# Features

# area_type - describes the area
# availability - when it can be possessed or when it is ready(categorical and time-series)
# location - where it is located in Bengaluru
# price - Value of the property in lakhs(INR)
# size - in BHK or Bedroom (1-10 or more)
# society - to which society it belongs
# total_sqft - size of the property in sq.ft
# bath - No. of bathrooms
# balcony - No. of the balcony

# Target variable:
# Price - Value of the property in lakhs(INR)

# Train dataset:
# Contains all the features and target variable.
# Contains 13,321 records.

# Test dataset:
# Contains all the features.
# Contains 1,481 records.

# Evaluation Metric
# Submissions are evaluated on Root-Mean-Squared-Error (RMSE) 
# between the logarithm of the predicted value and the logarithm of the observed sales price. 
# (Taking logs means that errors in predicting expensive houses and cheap houses will affect the result equally.)

# For more details:
# Link: https://www.machinehack.com/course/predicting-house-prices-in-bengaluru/