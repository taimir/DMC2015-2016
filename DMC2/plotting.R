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

# Yes/ No fields
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

# Remove fields that have the very same value (Feature selection)
drop <- c("TimeOfStop", "Agency", "SubAgency", "Geolocation", "Article", "Location")

training_data <- training_data[,!(names(training_data) %in% drop)]
test_data <- test_data[,!(names(test_data) %in% drop)]

# Plotting
# Coordinates
west <- subset(training_data, Latitude < (-60) & Latitude > (-77.6))
plot(west$Latitude, west$Longitude, col = west$Citation)

east <- subset(training_data, Latitude > 38.8 & Latitude < 39.3)
plot(east$Latitude, east$Longitude, col = east$Citation)

# Car type - seems like there are some dependencies after all
plot(training_data$Make, training_data$Model, col = training_data$Citation)

# Race and gender - there seems to be major discrimination against race
# Conclusion: it seems ASIAN and HISPANIC and OTHER tend to get a Citation a bit more
# the rest are perfectly unbiased
plot(training_data$Gender, training_data$Race, col  = training_data$Citation)
asian <- subset(training_data, Race == "ASIAN")
table(asian$Citation) # leaning towards a Citation
black <- subset(training_data, Race == "BLACK")
table(black$Citation) # relatively unbiased here
hispanic <- subset(training_data, Race == "HISPANIC")
table(hispanic$Citation) # leaning towards a Citation
nat_american <- subset(training_data, Race == "NATIVE AMERICAN")
table(nat_american$Citation) # unbiased
white <- subset(training_data, Race == "WHITE")
table(white$Citation) # unbiased
other <- subset(training_data, Race == "OTHER")
table(other$Citation) # leaning towards yes

# Year
# there is a some wrong years and a typo - 
# fix 2077 to 2007 at this type
pos_year <- subset(training_data, Year > 0 & Year < 2070)
plot(pos_year$Year, pos_year$Citation)

# Charge

plot(training_data$Charge, training_data$Citation)
reduced_charge <- training_data
reduced_charge$Charge <- as.factor(sub("-.*", "", training_data$Charge))
plot(reduced_charge$Charge, training_data$Citation)
# ArrestType
plot(training_data$ArrestType, training_data$Citation)
