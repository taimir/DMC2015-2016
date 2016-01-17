# Business Analytics
# Data Mining Cup 1 - Online sales prediction

# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)
library("DMwR")

# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed


######################################################
# 1. Build a Team in the DMC Manager
# https://dmc.dss.in.tum.de/dmc/
# Login with TUM login data ("TUM-Kennung")
#
# Found or join a team (size: 1-4 students)


######################################################
# 2. Load Training Data Set and Test Set
training_data = read.csv("dmc1_training.csv", sep=",",na.strings=c("","?","NA"))
test_data = read.csv("dmc1_test.csv", sep=",", na.strings=c("","?","NA"))

colSums(is.na(training_data))
colSums(is.na(test_data))

# Nominal features
training_data$order = as.factor(training_data$order)
test_data$order = as.factor(test_data$order)

training_data$startWeekday <- factor(training_data$startWeekday, labels=c("Fri","Sat","Sun"))
test_data$startWeekday <- factor(test_data$startWeekday, labels=c("Fri","Sat","Sun"))

training_data$addressType = factor(training_data$addressType, labels=c("Mr","Mrs","Company"))
test_data$addressType = factor(test_data$addressType, labels=c("Mr","Mrs","Company"))

# Convert startHour to a nominal feature (discretize)
library(arules)
# The gain is maximal if the hour is actually turned into a factor
training_data$startHour <- factor(training_data$startHour)
test_data$startHour <- factor(test_data$startHour)

# Add price "ranges"
training_data$clickPriceRange = training_data$clickMaxPrice - training_data$clickMinPrice
test_data$clickPriceRange = test_data$clickMaxPrice - test_data$clickMinPrice

training_data$cartPriceRange = training_data$cartMaxPrice - training_data$cartMinPrice
test_data$cartPriceRange = test_data$cartMaxPrice - test_data$cartMinPrice

# Add average item price in cart
training_data$AvgItemPrice = training_data$cartSumPrice / training_data$cartCount
test_data$AvgItemPrice = test_data$cartSumPrice / test_data$cartCount

# Count and save row indexes that have NaN value in either onlineStatus or availability coulmn
status_availability_train = training_data[,c("onlineStatus","availability")]
status_availability_test = test_data[,c("onlineStatus","availability")]
combined_status_availability <- rbind(status_availability_train,status_availability_test)
# Retunrs row indexes that have at least on NaN value in combined data frame
status_availability_NANs <- unique (unlist (lapply (combined_status_availability, function (x) which (is.na (x)))))

# Add another level - "unknown" for onlineStatus and replace NaN -> unknown"
training_data$onlineStatus = factor(training_data$onlineStatus, levels = c(levels(training_data$onlineStatus),"unknown"))
training_data$onlineStatus[is.na(training_data$onlineStatus)] <- "unknown"
# Do same as above for test data
test_data$onlineStatus = factor(test_data$onlineStatus, levels = c(levels(test_data$onlineStatus),"unknown"))
test_data$onlineStatus[is.na(test_data$onlineStatus)] <- "unknown"


# Add two new levels - "unknown" and "other"for availability and replace value 
# change NaN -> "unknown" and mixed, compl.not orderable, .... ->other
training_data$availability = factor(training_data$availability, levels = c(levels(training_data$availability),"unknown","other"))
training_data$availability[is.na(training_data$availability)] <- "unknown"
training_data$availability[(training_data$availability=="completely not determinable")] = "other"
training_data$availability[(training_data$availability=="completely not orderable")] = "other"
training_data$availability[(training_data$availability=="mainly not determinable")] = "other"
training_data$availability[(training_data$availability=="mixed")] = "other"
training_data$availability[(training_data$availability=="mainly orderable")] = "other"
training_data$availability = factor(training_data$availability)

# Do same as above for availability factor in test data
test_data$availability = factor(test_data$availability, levels = c(levels(test_data$availability),"unknown","other"))
test_data$availability[is.na(test_data$availability)] <- "unknown"

test_data$availability[(test_data$availability=="completely not determinable")] = "other"
test_data$availability[(test_data$availability=="completely not orderable")] = "other"
test_data$availability[(test_data$availability=="mainly not determinable")] = "other"
test_data$availability[(test_data$availability=="mixed")] = "other"
test_data$availability[(test_data$availability=="mainly orderable")] = "other"
test_data$availability = factor(test_data$availability)

################################
#   After the steps above we have 18 rows in test data that have NaN in some column
################################

# For other varibales: Since there is correlation between click and cart variables, try 
# try knn imputation on cominded_data (= training_data + test_data) which has 5000 instances
######## Not sure if this is a good idea because of curse of dimnesionality?
order_test = test_data[,"order"]
id_test = test_data[, "id"]

combined_data = rbind(training_data[,!names(training_data) %in% c("order")],test_data[, !names(test_data) %in% c("order")])

## Idea here: create additional column and if some row in combined_data has one or more  
## missing values, set appropriate row in this column to 1. 

# Retunrs row indexex that have at least on NaN value in combined data frame
rows_with_NaN <- unique (unlist (lapply (combined_data, function (x) which (is.na (x)))))
combined_data$NaN_values = 0
combined_data$NaN_values[rows_with_NaN] = 1
combined_data$NaN_values[status_availability_NANs] = 1

#Impute missing data with mean and kNN imputation
missing_age = which(is.na(combined_data$age))
combined_data$age[missing_age] <- mean(combined_data$age,na.rm = TRUE)
combined_data <- knnImputation(combined_data,k = 10)

################################
# Feature selection attempts
################################
library(FSelector)
# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(order ~ ., data=training_data)
weights_info_gain
 
weights_info_gain[order(-weights_info_gain$attr_importance), , drop = FALSE]

# those two are insignificant
# combined_data$lastOrder <- NULL
# combined_data$age <- NULL

# most_important_attributes <- cutoff.k.percent(weights_info_gain, 0.90)
# most_important_attributes
# 
# reduced_formula <- as.simple.formula(most_important_attributes, "order")
# 
# training_data$age <- NULL
# training_data$lastOrder <- NULL
# test_data$age <- NULL
# test_data$lastOrder <- NULL

pca <- preProcess(combined_data, method = "pca", thresh = 0.90)
combined_data <- predict(pca, newdata = combined_data)

training_data <- cbind(combined_data[1:4000, ], training_data$order)
colnames(training_data)[length(training_data)] = "order"

test_data <- cbind(combined_data[4001:5000,],order_test)
colnames(test_data)[length(test_data)] = "order"

#Remove id column, not needed for training
training_data = training_data[,!names(training_data) %in% c("id")]

colSums(is.na(training_data))
colSums(is.na(test_data))

######################################################
# 4. Training & Evaluation

# Caret tutorial for model training and tuning  
#http://topepo.github.io/caret/training.html

# Stratify to reach equal prop.
#library(ROSE)
# Instead of sample size (N=1100), one can specify the probability of rare class resampling, e.g. p=0.5
training_data = ovun.sample(order ~ ., data=training_data, method="over", p = 0.55, na.action="na.pass")$data
table(training_data$order)


#Partition training set for faster model training
InTrain<-createDataPartition(y=training_data$order,p=0.3,list=FALSE)
training_small<-training_data[InTrain,]
test_small<-training_data[-InTrain,]

# Train RandomForest model
# ,classProbs=TRUE,summaryFunction=twoClassSummary
# 
rf_model<-train(order ~ .,data=training_small,
                method="rf",
                trControl=trainControl(method="cv",number=10),
                ntree = 501,
                prox=TRUE, allowParallel = TRUE,na.action = na.exclude)


# Train AdaBoost Model
# ada_model<-ada(training_data[,!names(training_data) %in% c("order")], training_data$order,
#                loss=c("exponential","logistic"),
#                type=c("discrete","real","gentle"),
#                iter=200,
#                nu=0.2,
#                bag.frac=0.1,
#                model.coef=FALSE,
#                bag.shift=FALSE,
#                max.iter=20,
#                delta=10^(-10),
#                verbose=FALSE,
#                na.action=na.rpart)


#Train Gradient Boosting Machines model
# gbmGrid <-  expand.grid(interaction.depth = c(1, 3, 5, 9),
#                         n.trees = (1:20)*50,
#                         shrinkage = 0.01,
#                         n.minobsinnode = 10)
# gbm_model<-train(order~.,data=training_data,
#                  method="gbm",
#                  trControl=trainControl(method="cv",number=10), 
#                  tuneGrid=gbmGrid,  
#                  verbose = FALSE)
# 
# 
# print(rf_model)
# print(rf_model)
# var_importance <-  varImp(gbm_model, scale = FALSE)
# plot(var_importance)
######################################################
# 5. Predict Classes in Test Data

#Make RF predictions
 prediction_classes_rf = predict.train(object=rf_model, newdata=test_data)
 predictions_rf = data.frame(id=test_data$id, prediction=prediction_classes_rf)
# 
# #Make Ada predictions
# prediction_classes_ada = predict(ada_model, newdata=test_data, type = c("vector", "probs", "both", "F"))
# predictions_ada = data.frame(id=test_data$id, prediction=prediction_classes_ada)
#  
# #Make GBM predictions
# prediction_classes_gbm = predict.train(object=gbm_model, newdata=test_data)
# predictions_gbm = data.frame(id=test_data$id, prediction=prediction_classes_gbm)
#  
# # Check the proportion of y/n values of dependent variable order in train and test set
# prop.table(table(training_data$order))
# prop.table(table(prediction_classes_rf))

#
# ######################################################
# # 6. Export the Predictions
# 
# write.csv(predictions_ada, file="predictions_dmc1_ada.csv", row.names=FALSE)
  write.csv(predictions_rf, file="predictions_dmc1_rf_PCA_noSampling.csv", row.names=FALSE)
# write.csv(predictions1, file="predictions_dmc1_gbm.csv", row.names=FALSE)

print("Done. Files saved. ")
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