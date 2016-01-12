# Business Analytics
# Data Mining Cup 1 - Online sales prediction

# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)

setwd("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1")

# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


######################################################
# 1. Build a Team in the DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Found or join a team (size: 1-4 students)


######################################################
# 2. Load & Explore the Training Data Set
training_data = read.csv("dmc1_training.csv", sep=",",na.strings=c("","?","NA"))
test_data = read.csv("dmc1_test.csv", sep=",", na.strings=c("","?","NA"))

# Explore the data set...
#   https://www.youtube.com/watch?v=q5oPcgctQpk

dim(training_data)
str(training_data)
summary(training_data)
names(training_data)

#Remove id column, not needed for training
training_data = training_data[,2:21]

#availablity and online status dont seem to be important
training_data$availability<- NULL
training_data$onlineStatus<- NULL

test_data$availability<- NULL
test_data$onlineStatus<- NULL


# Nominal features
training_data$order = as.factor(training_data$order)
test_data$order = as.factor(test_data$order)

# 0 is a weekday, 1 is weekend...doesn't seem really good
training_data$startWeekday[(training_data$startWeekday==5)] = 0
training_data$startWeekday[(training_data$startWeekday==6)] = 1
training_data$startWeekday[(training_data$startWeekday==7)] = 1
training_data$startWeekday = factor(training_data$startWeekday)

test_data$startWeekday[(test_data$startWeekday==5)] = 0
test_data$startWeekday[(test_data$startWeekday==6)] = 1
test_data$startWeekday[(test_data$startWeekday==7)] = 1
test_data$startWeekday = factor(test_data$startWeekday)

training_data$addressType = as.factor(training_data$addressType, labels=c("Mr","Mrs","Company"))
test_data$addressType = as.factor(test_data$addressType, labels=c("Mr","Mrs","Company"))


str(training_data)


training_data[is.na(training_data)] <- -1
test_data[is.na(test_data)] <- -1
summary(training_data)

View(training_data)


dim(training_data)
names(training_data)

#Proportion of yes/no for order variable we need to make this even 
prop.table(table(training_data$order))


#Transform columns cols in log scale
cols <- c("duration","cartSumPrice", "clickMaxPrice","accountLifetime")
training_data[cols] <- log(training_data[cols])
names(training_data)


#Remove all rows with NaN values in
#http://nerds.airbnb.com/overcoming-missing-values-in-a-rfc/
dim(training_data)
training_data=training_data[complete.cases(training_data),]
dim(training_data)
summary(training_data)


boxplot(training_data$order,(training_data$startWeekday))
boxplot(training_data$availability,training_data$order)
boxplot(training_data$order,(training_data$clickMaxPrice))
boxplot(training_data$order,(training_data$clickCount))
table(training_data$availability, useNA="always")

#Proportion table between order and addressType variable 
prop.table(table(training_data$order,training_data$addressType),margin = 2)
barplot(prop.table(table(training_data$order)))



######################################################
# 4. Training & Evaluation

# Caret tutorial for model training and tuning  
#http://topepo.github.io/caret/training.html

#Partition training set for faster model training
InTrain<-createDataPartition(y=training_data$order,p=0.5,list=FALSE)
training_small<-training_data[InTrain,]
prop.table(table(training_small$order))
test_small<-training_data[-InTrain,]


# http://bigcomputing.blogspot.de/2014/10/an-example-of-using-random-forest-in.html
#duration+clickMaxPrice+cartMaxPrice+cartSumPrice+addressType+accountLifetime+clickCount
rf_model<-train(order~.,data=training_data,method="rf",
                trControl=trainControl(method="cv",number=10),
                prox=TRUE,allowParallel = TRUE, na.action = na.exclude)

print(rf_model)
print(rf_model$finalModel)

#Check importance of all features in dataset
importance <- rf_model$finalModel$importance
var_importance <-  varImp(rf_model, scale = FALSE)
plot(var_importance, top = 30)
plot(importance, top = 30)


######################################################
# 5. Predict Classes in Test Data
dim(test_data)
summary(test_data)
names(test_data)

#Remove id, availability and onlineStatus features
test_data <- test_data[,c(-1,-13,-21)]
dim(test_data)


#Replace all NA data with -1
#test_data[is.na(test_data)] <- -1
#test_data$onlineStatus[is.na(test_data$onlineStatus)] <- "y"

prediction_classes = predict.train(object=rf_model, newdata=test_data, na.action=na.pass)
prediction_classes

predictions = data.frame(id=test_data$id, prediction=prediction_classes)
predictions


######################################################
# 6. Export the Predictions
write.csv(predictions, file="predictions_dmc1.csv", row.names=FALSE)


######################################################
# 7. Upload the Predictions and the Corresponding R Script on DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Maxium number of submissions: 10
#
# Possible errors that could occur:
# - Wrong column names
# - Unknown IDs (if not in Test Data)
# - Missing IDs (if in Test Data but not in Predictions)
# - Wrong file format