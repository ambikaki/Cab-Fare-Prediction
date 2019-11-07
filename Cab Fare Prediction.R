# remove environment
rm(list = ls())

# setting working directory
setwd("C:/Users/Radhika/Desktop") 
getwd()

# #loading Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "e1071","geosphere",
      "DataCombine", "pROC", "doSNOW", "class", "readxl","ROSE","dplyr", "plyr", 
      "reshape","xlsx", "pbapply", "unbalanced", "dummies", "MASS" , "gbm" ,
      "Information", "rpart", "tidyr", "miscTools")


# #install.packages if not 
lapply(x, install.packages)

# #load libraries
lapply(x, require, character.only = TRUE)
rm(x)

# loading data
train = read.csv("train_cab.csv")
test = read.csv("test.csv")

#### ----- Understanding the data using descriptive statistics -----###

# checking first few observations
head(train)

#Summary of data
summary(train)

#structure of data or data types 
str(train)

# chechking few observations in test data
head(test)

# summary of test data
summary(test)

# structure of test data
str(test)

###------ Exploratory Data Analysis ------####

# Changing the data types of variables
train$fare_amount = as.numeric(as.character(train$fare_amount), na.rm=TRUE)
train$passenger_count=round(train$passenger_count)

### Removing values which are not within desired range(outlier) depending upon basic understanding of dataset.

# 1.Fare amount has a negative value, which doesn't make sense. A price amount cannot be -ve and also cannot be 0. So we will remove these fields.
train[which(train$fare_amount < 1 ),]
nrow(train[which(train$fare_amount < 1 ),])
train = train[-which(train$fare_amount < 1 ),]

# Passenger_count variable
nrow(train[which(train$passenger_count > 6 ),])


# passenger_count is above 6, let's check them.
train[which(train$passenger_count > 6 ),]

# Also we need to see if there are any passenger_count==0
train[which(train$passenger_count < 1 ),]
nrow(train[which(train$passenger_count < 1 ),])

# We will remove these 58 observations and 20 observation which are above 6 value because a cab cannot hold these number of passengers.
train = train[-which(train$passenger_count < 1 ),]
train = train[-which(train$passenger_count > 6),]

# Latitudes range from -90 to 90.
#Longitudes range from -180 to 180.
#Removing which does not satisfy these ranges
nrow(train[which(train$pickup_longitude >180 ),])
nrow(train[which(train$pickup_longitude < -180 ),])
nrow(train[which(train$pickup_latitude > 90 ),])
nrow(train[which(train$pickup_latitude < -90 ),])
nrow(train[which(train$dropoff_longitude > 180 ),])
nrow(train[which(train$dropoff_longitude < -180 ),])
nrow(train[which(train$dropoff_latitude < -90 ),])
nrow(train[which(train$dropoff_latitude > 90 ),])

# There's only one outlier which is in variable pickup_latitude.
#So we will remove it.
# Also we will see if there are any values equal to 0.
nrow(train[which(train$pickup_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])
nrow(train[which(train$dropoff_longitude == 0 ),])
nrow(train[which(train$pickup_latitude == 0 ),])

# there are values which are equal to 0. we will remove them.
train = train[-which(train$pickup_latitude > 90),]
train = train[-which(train$pickup_longitude == 0),]
train = train[-which(train$dropoff_longitude == 0),]

# #there are -ve values in fare amount, so dropping values less than 2.5. 
# considering the minimum fare amount is 2.5
train = train[-which(train$pickup_latitude < 2.5),]

# fare amount has extreme values. the max amount we are considering here are 453, 
# anything beyond that, we are dropping
train = train[-which(train$fare_amount > 454),]

## ----- Missing Value Analysis ----- ##
sum(is.na(train$fare_amount))
sum(is.na(train$passenger_count))
sum(is.na(train$pickup_datetime))
sum(is.na(train))

# removing missing values from fare amount and passenger count
# there are only 77 missing values i.e., not even 1%, so dropping them
library(tidyr)
library(dplyr)
library(plyr)

train = DropNA(train)


###############     Feature Engineering    #######################

# Feature Engineering for timestamp variable
# we will derive new features from pickup_datetime variable
# new features will be year, month, day_of_week, hour
#Convert pickup_datetime from factor to date time
train$pickup_date = as.Date(as.character(train$pickup_datetime))
train$weekday = as.factor(format(train$pickup_date,"%u"))# Monday = 1
train$mnth = as.factor(format(train$pickup_date,"%m"))
train$yr = as.factor(format(train$pickup_date,"%Y"))
pickup_time = strptime(train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train$hour = as.factor(format(pickup_time,"%H"))

#deriving some features from test set
test$pickup_date = as.Date(as.character(test$pickup_datetime))
test$weekday = as.factor(format(test$pickup_date,"%u"))# Monday = 1
test$mnth = as.factor(format(test$pickup_date,"%m"))
test$yr = as.factor(format(test$pickup_date,"%Y"))
pickup_time = strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test$hour = as.factor(format(pickup_time,"%H"))

# there was 1 'na in pickup datetime after feature engineering
sum(is.na(train$pickup_datetime))

# we will remove that 1 row of na's
train = DropNA(train)
sum(is.na(train))

# removing some variables which are not important to the model
train = subset(train,select = -c(pickup_datetime,pickup_date))
test = subset(test,select = -c(pickup_datetime,pickup_date))

# Calculate the distance travelled using longitude and latitude
deg_to_rad = function(deg){
  (deg * pi) / 180
}
haversine = function(long1,lat1,long2,lat2){
  #long1rad = deg_to_rad(long1)
  phi1 = deg_to_rad(lat1)
  #long2rad = deg_to_rad(long2)
  phi2 = deg_to_rad(lat2)
  delphi = deg_to_rad(lat2 - lat1)
  dellamda = deg_to_rad(long2 - long1)
  
  a = sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * 
    sin(dellamda/2) * sin(dellamda/2)
  
  c = 2 * atan2(sqrt(a),sqrt(1-a))
  R = 6371e3
  R * c / 1000 #1000 is used to convert to meters
}

# Using haversine formula to calculate distance fr both train and test
train$dist = haversine(train$pickup_longitude,train$pickup_latitude,train$dropoff_longitude,train$dropoff_latitude)
test$dist = haversine(test$pickup_longitude,test$pickup_latitude,test$dropoff_longitude,test$dropoff_latitude)


# We will remove the variables which were used to feature engineer new variables
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
test = subset(test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

str(train)
summary(train)

#### --------- Feature selection  --------- #######
numeric_index = sapply(train,is.numeric) #selecting only numeric

numeric_data = train[,numeric_index]

cnames = colnames(numeric_data)
#Correlation analysis for numeric variables
corrgram(train[,numeric_index],upper.panel=panel.pie, main = "Correlation Plot")

#ANOVA for categorical variables with target numeric variable

#aov_results = aov(fare_amount ~ passenger_count * pickup_hour * pickup_weekday,data = train)
aov_results = aov(fare_amount ~ passenger_count + hour + weekday + mnth + yr,data = train)

summary(aov_results)

# pickup_weekdat has p value greater than 0.05 
#train = subset(train,select=-weekday)

#remove from test set
#test = subset(test,select=-weekday)

############## ------- Feature Scaling  -------- ##################

#Normalisation

print('dist')
train[,'dist'] = (train[,'dist'] - min(train[,'dist']))/
  (max(train[,'dist'] - min(train[,'dist'])))

# #check multicollearity
install.packages("usdm")
library(usdm)
vif(train[,-1])
vifcor(train[,-1], th = 0.9)

#######  Splitting train into train and validation subsets  ############
set.seed(1000)
tr.idx = createDataPartition(train$fare_amount,p=0.75,list = FALSE) # 75% in trainin and 25% in Validation Datasets
train_data = train[tr.idx,]
test_data = train[-tr.idx,]

rmExcept(c("test","train","df",'df1','df2','df3','test_data','train_data','test_pickup_datetime'))

#########    Model Building   ##########

####  Linear regression   #####
lm_model = lm(fare_amount ~.,data=train_data)

summary(lm_model)
str(train_data)
plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")

lm_predictions = predict(lm_model,test_data[,2:7])

qplot(x = test_data[,1], y = lm_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1],lm_predictions)

# mae        mse         rmse       mape 
# 5.9250805  84.1177311  9.1715719  0.6521778

#####   Decision Tree   #######

Dt_model = rpart(fare_amount ~ ., data = train_data, method = "anova")

summary(Dt_model)

#Predict for new test cases
predictions_DT = predict(Dt_model, test_data[,2:7])

qplot(x = test_data[,1], y = predictions_DT, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1],predictions_DT)

# mae       mse        rmse      mape 
# 2.703336  24.125931  4.911815  0.267605 

#####     Random forest   ######
rf_model = randomForest(fare_amount ~.,data=train_data)

summary(rf_model)

rf_predictions = predict(rf_model,test_data[,2:7])

qplot(x = test_data[,1], y = rf_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1],rf_predictions)

# mae        mse        rmse       mape 
# 2.3503473 18.2085090  4.2671430  0.2429453
     
####     Improving Accuracy by using Ensemble technique ---- XGBOOST   ###########################
install.packages("xgboost")
library(xgboost)

train_data_matrix = as.matrix(sapply(train_data[-1],as.numeric))
test_data_data_matrix = as.matrix(sapply(test_data[-1],as.numeric))

xgboost_model = xgboost(data = train_data_matrix,label = train_data$fare_amount,nrounds = 15,verbose = FALSE)

summary(xgboost_model)
xgb_predictions = predict(xgboost_model,test_data_data_matrix)

qplot(x = test_data[,1], y = xgb_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1],xgb_predictions)

# mae        mse        rmse       mape 
# 2.1799179 18.6693237  4.3208013  0.2204578 

#######     Finalizing and Saving Model for later use   #########
# In this step we will train our model on whole training Dataset and save that model for later use
train_data_matrix2 = as.matrix(sapply(train[-1],as.numeric))
test_data_matrix2 = as.matrix(sapply(test,as.numeric))

xgboost_model2 = xgboost(data = train_data_matrix2,label = train$fare_amount,nrounds = 15,verbose = FALSE)

# Let's now predict on test dataset
xgb = predict(xgboost_model2,test_data_matrix2)

xgb_pred = data.frame(test_data_matrix2,"predictions" = xgb)

# Now lets write(save) the predicted fare_amount in disk as .csv format 
write.csv(xgb_pred,"xgb_predictions_R.csv",row.names = FALSE)
