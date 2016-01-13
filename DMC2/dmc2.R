# Business Analytics
# Data Mining Cup Introduction
#
# Please note, that this script only has the nature of proposal. It provides useful functions for the steps of data mining but does not cover all possibilities.

# The caret package is used (http://topepo.github.io/caret/index.html)
#install.packages("caret")
library(caret)
library(plyr)

# For reasons of traceability you must use a fixed seed
set.seed(42) # do NOT CHANGE this seed

######################################################
# 2. Load 
training_data = read.csv("training.csv", sep=",")
test_data = read.csv("test.csv", sep=",")

######################################################
# 3. Data Preparation
# (using both training and test data)
# do NOT DELETE any instances in the test data

# Rename columns
names(training_data)[names(training_data) == 'ProperyDamage'] <- 'PropertyDamage'
names(test_data)[names(test_data) == 'ProperyDamage'] <- 'PropertyDamage'

names(training_data)[names(training_data) == 'WorkZOne'] <- 'WorkZone'
names(test_data)[names(test_data) == 'WorkZOne'] <- 'WorkZone'

# Nominal attributes
setLevels <- function (data, attr, levels) {
  data[,attr] <- factor(data[,attr], levels=levels)
  return(data)
}


# Reduce charge
training_data$Charge <- as.factor(sub("-.*", "", training_data$Charge))
test_data$Charge <- as.factor(sub("-.*", "", test_data$Charge))

# Unify Yes/ No fields
# --------------------------------------------------
levels <- c("No", "Yes")
# training data
training_data <- setLevels(training_data, "Accident", levels)
training_data <- setLevels(training_data, "Belts", levels)
training_data <- setLevels(training_data, "PersonalInjury", levels)
training_data <- setLevels(training_data, "PropertyDamage", levels)
training_data <- setLevels(training_data, "Fatal", levels)
training_data <- setLevels(training_data, "CommercialLicense", levels)
training_data <- setLevels(training_data, "HAZMAT", levels)
training_data <- setLevels(training_data, "CommercialVehicle", levels)
training_data <- setLevels(training_data, "Alcohol", levels)
training_data <- setLevels(training_data, "WorkZone", levels)
training_data <- setLevels(training_data, "ContributedToAccident", levels)

# test data
test_data <- setLevels(test_data, "Accident", levels)
test_data <- setLevels(test_data, "Belts", levels)
test_data <- setLevels(test_data, "PersonalInjury", levels)
test_data <- setLevels(test_data, "PropertyDamage", levels)
test_data <- setLevels(test_data, "Fatal", levels)
test_data <- setLevels(test_data, "CommercialLicense", levels)
test_data <- setLevels(test_data, "HAZMAT", levels)
test_data <- setLevels(test_data, "CommercialVehicle", levels)
test_data <- setLevels(test_data, "Alcohol", levels)
test_data <- setLevels(test_data, "WorkZone", levels)
test_data <- setLevels(test_data, "ContributedToAccident", levels)

unifyLevels <- function(data, attr) {
  levels <- union(levels(training_data[, attr]), levels(test_data[, attr]))
  data[,attr] <- factor(data[,attr], levels=levels)
  return(data)
}

# State
training_data <- unifyLevels(training_data, "State")
test_data <- unifyLevels(test_data, "State")

# VehicleType
training_data <- unifyLevels(training_data, "VehicleType")
test_data <- unifyLevels(test_data, "VehicleType")

# Make
training_data <- unifyLevels(training_data, "Make")
test_data <- unifyLevels(test_data, "Make")

# Model
training_data <- unifyLevels(training_data, "Model")
test_data <- unifyLevels(test_data, "Model")

# Color
training_data <- unifyLevels(training_data, "Color")
test_data <- unifyLevels(test_data, "Color")

# Charge
training_data <- unifyLevels(training_data, "Charge")
test_data <- unifyLevels(test_data, "Charge")

# Race
training_data <- unifyLevels(training_data, "Race")
test_data <- unifyLevels(test_data, "Race")

# Gender
training_data <- unifyLevels(training_data, "Gender")
test_data <- unifyLevels(test_data, "Gender")

# DriverCity
training_data <- unifyLevels(training_data, "DriverCity")
test_data <- unifyLevels(test_data, "DriverCity")

# DriverState
training_data <- unifyLevels(training_data, "DriverState")
test_data <- unifyLevels(test_data, "DriverState")

# DLState
training_data <- unifyLevels(training_data, "DLState")
test_data <- unifyLevels(test_data, "DLState")

# ArrestType
training_data <- unifyLevels(training_data, "ArrestType")
test_data <- unifyLevels(test_data, "ArrestType")

# Remove missing features
# ----------------------------------------------------
drop <- c("TimeOfStop", "Agency", "SubAgency", "Geolocation", "Article", "Location")
training_data <- training_data[,!(names(training_data) %in% drop)]
test_data <- test_data[,!(names(test_data) %in% drop)]

# Fix errors
# ---------------------------------------------------
training_data$Year[training_data$Year == 2077] = 2007
training_data$Year[training_data$Year == 0] = median(training_data$Year)
training_data$Year <- as.integer(training_data$Year)

# Discretize Longitude and Latitude
# ----------------------------------------------------
# TODO: do this by considering longitude and lat. at the same time (squares in 2D space)
# So far I've seen no obvious "separation" in the locations, so
# discretization with a single rule seems meaningless
# install.packages("arules")
library(arules)
# Equal frequency binning
equiFreqLatitude = discretize(training_data$Latitude, categories=18, method="cluster", onlycuts=TRUE)
training_data$LatitudeDiscr = cut(training_data$Latitude, breaks=equiFreqLatitude, ordered_result=TRUE, right=FALSE)
test_data$LatitudeDiscr = cut(test_data$Latitude, breaks=equiFreqLatitude, ordered_result=TRUE, right=FALSE)
# table(training_data$LatitudeDiscr, useNA="ifany")
# str(training_data)

equiFreqLongitude = discretize(training_data$Longitude, categories=18, method="cluster", onlycuts=TRUE)
training_data$LongitudeDiscr = cut(training_data$Longitude, breaks=equiFreqLongitude, ordered_result=TRUE, right=FALSE)
test_data$LongitudeDiscr = cut(test_data$Longitude, breaks=equiFreqLongitude, ordered_result=TRUE, right=FALSE)
# table(training_data$LongitudeDiscr, useNA="ifany")
# str(training_data)



# Handle NA values
# ----------------------------------------------------
# Only for longitude and latitude, replace with median
# remove NA Year and NA color
naCols = c("Longitude", "Latitude")
pp<- preProcess(training_data[naCols], method = c("medianImpute"))
preprocessed <- predict(pp, newdata = training_data[naCols])
training_data$Latitude = preprocessed$Latitude
training_data$Longitude = preprocessed$Longitude

# remove other NA values
training_data <- training_data[complete.cases(training_data),]
colSums(is.na(training_data))



# New fields
# ----------------------------------------------------

extractField <- function(data, searches, fieldName) {
  filter <- 1:length(data$Description)
  # find all entries matching all of the searched words
  for (word in searches) {
    filter <- intersect(filter, grep(word, data$Description, ignore.case = TRUE))
  }
  if(!fieldName %in% colnames(data)) {
    data[,fieldName] <- factor("No", levels = c("No", "Yes"))
  }
  data[filter,fieldName] <- factor("Yes", levels = c("No", "Yes"))
  return(data)
}

# Alcohol
training_data <- extractField(training_data, c("ALCOHOL"), "Alcohol")
test_data <- extractField(test_data, c("ALCOHOL"), "Alcohol")

# Speed
training_data <- extractField(training_data, c("SPEED"), "Speed")
test_data <- extractField(test_data, c("SPEED"), "Speed")

training_data <- extractField(training_data, c("EXCEEDING"), "Speed")
test_data <- extractField(test_data, c("EXCEEDING"), "Speed")

# Accident
training_data <- extractField(training_data, c("ACCIDENT"), "Accident")
test_data <- extractField(test_data, c("ACCIDENT"), "Accident")

# Belts
training_data <- extractField(training_data, c("SEATBELT"), "Belts")
test_data <- extractField(test_data, c("SEATBELT"), "Belts")

# Injury
training_data <- extractField(training_data, c("INJUR"), "PersonalInjury")
test_data <- extractField(test_data, c("INJUR"), "PersonalInjury")

# Damage
training_data <- extractField(training_data, c("DAMAGE"), "PropertyDamage")
test_data <- extractField(test_data, c("DAMAGE"), "PropertyDamage")

# Fatal
training_data <- extractField(training_data, c("FATAL"), "Fatal")
test_data <- extractField(test_data, c("FATAL"), "Fatal")

training_data <- extractField(training_data, c("DEATH"), "Fatal")
test_data <- extractField(test_data, c("DEATH"), "Fatal")

training_data <- extractField(training_data, c("DEAD"), "Fatal")
test_data <- extractField(test_data, c("DEAD"), "Fatal")

# Commercial License
training_data <- extractField(training_data, c("COMMERCIAL LICENSE"), "CommercialLicense")
test_data <- extractField(test_data, c("COMMERCIAL LICENSE"), "CommercialLicense")

# License
training_data <- extractField(training_data, c("LICENSE"), "License")
test_data <- extractField(test_data, c("LICENSE"), "License")

# HAZMAT
training_data <- extractField(training_data, c("HAZARD"), "HAZMAT")
test_data <- extractField(test_data, c("HAZARD"), "HAZMAT")

# CommercialVehicle
training_data <- extractField(training_data, c("COMMERCIAL VEHICLE"), "CommercialVehicle")
test_data <- extractField(test_data, c("COMMERCIAL VEHICLE"), "CommercialVehicle")

# WorkZone
training_data <- extractField(training_data, c("WORKZONE"), "WorkZone")
test_data <- extractField(test_data, c("WORKZONE"), "WorkZone")

# Accident
training_data <- extractField(training_data, c("ACCIDENT"), "ContributedToAccident")
test_data <- extractField(test_data, c("ACCIDENT"), "ContributedToAccident")

# Life
training_data <- extractField(training_data, c("LIFE"), "Life")
test_data <- extractField(test_data, c("LIFE"), "Life")

# Danger
training_data <- extractField(training_data, c("DANGER"), "Danger")
test_data <- extractField(test_data, c("DANGER"), "Danger")

# Drug
training_data <- extractField(training_data, c("DRUG"), "Drug")
test_data <- extractField(test_data, c("DRUG"), "Drug")

training_data <- extractField(training_data, c("SUBSTANCE"), "Drug")
test_data <- extractField(test_data, c("SUBSTANCE"), "Drug")

# Crosswalk
training_data <- extractField(training_data, c("CROSSWALK"), "Crosswalk")
test_data <- extractField(test_data, c("CROSSWALK"), "Crosswalk")

# Registration
training_data <- extractField(training_data, c("REG."), "Registration")
test_data <- extractField(test_data, c("REG."), "Registration")

training_data <- extractField(training_data, c("REGIST"), "Registration")
test_data <- extractField(test_data, c("REGIST"), "Registration")

# Lights
training_data <- extractField(training_data, c("LIGHTS"), "Lights")
test_data <- extractField(test_data, c("LIGHTS"), "Lights")

training_data <- extractField(training_data, c("LAMP"), "Lights")
test_data <- extractField(test_data, c("LAMP"), "Lights")

# PHONE
training_data <- extractField(training_data, c("PHONE"), "Phone")
test_data <- extractField(test_data, c("PHONE"), "Phone")

# Red signal
training_data <- extractField(training_data, c("RED", "SIGNAL"), "RedSignal")
test_data <- extractField(test_data, c("RED", "SIGNAL"), "RedSignal")

# Medical certificate
training_data <- extractField(training_data, c("MEDICAL", "CERTIFICATE"), "MedCert")
test_data <- extractField(test_data, c("MEDICAL", "CERTIFICATE"), "MedCert")

# Right of way
training_data <- extractField(training_data, c("RIGHT OF WAY"), "RightOfWay")
test_data <- extractField(test_data, c("RIGHT OF WAY"), "RightOfWay")

training_data <- extractField(training_data, c("RIGHT-OF-WAY"), "RightOfWay")
test_data <- extractField(test_data, c("RIGHT-OF-WAY"), "RightOfWay")

# Highway
training_data <- extractField(training_data, c("HIGHWAY"), "Highway")
test_data <- extractField(test_data, c("HIGHWAY"), "Highway")

# No passing
training_data <- extractField(training_data, c("NO PASSING"), "NoPassing")
test_data <- extractField(test_data, c("NO PASSING"), "NoPassing")

# Insurence
training_data <- extractField(training_data, c("INSURE"), "Insurence")
test_data <- extractField(test_data, c("INSURE"), "Insurence")

# Turn
training_data <- extractField(training_data, c("TURN"), "Turn")
test_data <- extractField(test_data, c("TURN"), "Turn")

# Pedestrian
training_data <- extractField(training_data, c("PEDESTRIAN"), "Pedestrian")
test_data <- extractField(test_data, c("PEDESTRIAN"), "Pedestrian")

# Child
training_data <- extractField(training_data, c("CHILD"), "Child")
test_data <- extractField(test_data, c("CHILD"), "Child")

# Passanger
training_data <- extractField(training_data, c("PASSENGER"), "Passenger")
test_data <- extractField(test_data, c("PASSENGER"), "Passenger")

training_data <- extractField(training_data, c("OCCUPANT"), "Passenger")
test_data <- extractField(test_data, c("OCCUPANT"), "Passenger")

# STOP 
training_data <- extractField(training_data, c("STOP"), "Stop")
test_data <- extractField(test_data, c("STOP"), "Stop")

# Tire
training_data <- extractField(training_data, c("TIRE"), "Tire")
test_data <- extractField(test_data, c("TIRE"), "Tire")

# SIGNS
training_data <- extractField(training_data, c("SIGNS"), "Signs")
test_data <- extractField(test_data, c("SIGNS"), "Signs")

# Police
training_data <- extractField(training_data, c("POLICE"), "Police")
test_data <- extractField(test_data, c("POLICE"), "Police")

training_data <- extractField(training_data, c("OFFICER"), "Police")
test_data <- extractField(test_data, c("OFFICER"), "Police")

# Lane
training_data <- extractField(training_data, c("LANE"), "Lane")
test_data <- extractField(test_data, c("LANE"), "Lane")




# Multicolinearity
# ----------------------------------------------------
# * Check columns
# * remove ones that are collinear


# Feature selection
# ----------------------------------------------------

# Remove some excess fields
training_data$Latitude <- NULL
test_data$Latitude <- NULL
training_data$Longitude <- NULL
test_data&Longitude <- NULL
training_data$Description <- NULL
test_data$Description <- NULL

# TODO: decide whether to remove description

# Drop the id column
training_data$id <- NULL
# Drop driverCity and model
training_data$DriverCity <- NULL
training_data$Model <- NULL
test_data$DriverCity <- NULL
test_data$Model <- NULL

#install.packages("FSelector")
library(FSelector)
# Calculate weights for the attributes using Info Gain and Gain Ratio
weights_info_gain = information.gain(Citation ~ ., data=training_data)
weights_info_gain

weights_gain_ratio = gain.ratio(Citation ~ ., data=training_data)
weights_gain_ratio

most_important_attributes <- cutoff.k(weights_info_gain, 25)
most_important_attributes

reduced_formula <- as.simple.formula(most_important_attributes, "Citation")

# COMMERCIAL VEHICLE and YEAR seem to be of no importance, remove them
training_data$Year <- NULL
test_data$Year <- NULL
training_data$CommercialVehicle <- NULL
test_data$CommercialVehicle <- NULL

# check the proportions of the labels
prop.table(table(training_data$Citation)) # close enough


######################################################
# 4. Training & Evaluation
# List of classifiers in Caret: http://topepo.github.io/caret/modelList.html

# Caret tutorial for model training and tuning  
#http://topepo.github.io/caret/training.html

#Partition training set for faster model training
InTrain<-createDataPartition(y=training_data$Citation,p=0.5,list=FALSE)
training_small<-training_data[InTrain,]
test_small<-training_data[-InTrain,]


# http://bigcomputing.blogspot.de/2014/10/an-example-of-using-random-forest-in.html
# State + VehicleType + Charge + Race + ArrestType + Alcohol + Speed + Accident + Belts + PersonalInjury + PropertyDamage + Fatal + License + HAZMAT + CommercialLicense + WorkZone + Accident + Life + Danger + Drug + Crosswalk + Registration + Lights + Phone + RedSignal + MedCert + RightOfWay + Highway + NoPassing + Insurence + Turn + Pedestrian + Child + Passenger + Stop + Tire + Signs + Police + Lane
rf_model<-train(Citation ~ Alcohol + Speed + Accident + Belts + PersonalInjury + PropertyDamage + Fatal + License + HAZMAT + CommercialLicense + WorkZone + Accident + Life + Danger + Drug + Crosswalk + Registration + Lights + Phone + RedSignal + MedCert + RightOfWay + Highway + NoPassing + Insurence + Turn + Pedestrian + Child + Passenger + Stop + Tire + Signs + Police + Lane,data=training_small,method="rf",
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
test_data <- test_data[,c(-1)]

prediction_classes = predict.train(object=rf_model, newdata=test_data, na.action=na.pass)
prediction_classes

predictions = data.frame(id=test_data$id, prediction=prediction_classes)
predictions


