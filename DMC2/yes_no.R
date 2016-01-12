library(plyr)

# Exploring data


# Show columns with missing values
colSums(is.na(training_data))
# geolocation is missing everywhere, remove it

# Explore the violation types
# !!!! IMPORTANT: Almost all of them are set to No (214 yeses throughout) !!!!
# NOTE: fix typos in the attribute names, Poper--Y--Damage and WorkZ--O--ne
nrow(!is.na(subset(training_data, Belts == 'Yes' | Alcohol == 'Yes' | Accident == 'Yes' | PersonalInjury == 'Yes' | ProperyDamage == 'Yes' | Fatal == 'Yes' | CommercialLicense == 'Yes' | HAZMAT == 'Yes' | CommercialVehicle == 'Yes' | WorkZOne == 'Yes')))

# ----------------- Accident -------------------------------
# Check the correlation of the Accident field and the description
View(training_data[grep('ACCIDENT', training_data$Description), ])
# there are no records of accidents, hmmmmm... ------> Fields don't match again
table(training_data$Accident)
# Check the correlation of Accidents and Citations ---> 100% correlation as expected
table(training_data[grep('ACCIDENT', training_data$Description), ]$Citation)

count(training_data, vars=c('Accident', 'Citation'))

# ----------------- Alcohol --------------------------------
#
# Immense correlation between ALCOHOL and Citation, but Alcohol field is wrong
table(training_data$Alcohol)
View(training_data[grep('ALCOHOL', training_data$Description), ])
table(training_data[grep('ALCOHOL', training_data$Description), ]$Citation)
count(training_data, vars=c('Alcohol', 'Citation'))

# ----------------- Belts ----------------------------------
# First, check if belts in description match the belts field ----> Not at all
# The field is definitely wrong
View(training_data[grep('BELT', training_data$Description), ])
table(training_data$Belts)
table(training_data[grep('BELT', training_data$Description), ]$Belts)
# Next, check the ones with checked Belt field and no Belts in the description
View(subset(training_data, Belts == 'Yes'))
# Next, check the correlation of belts and Citation ---> There is a correlation in 75% of the cases
table(training_data[grep('BELT', training_data$Description), ]$Citation)
count(training_data, vars=c('Belts', 'Citation'))

# ---------------- Injury ----------------------------------
View(training_data[grep('INJUR.*', training_data$Description), ])
table(training_data$PersonalInjury)
View(subset(training_data, PersonalInjury == 'Yes'))
count(training_data, vars=c('PersonalInjury', 'Citation'))


# ------------ Fatal ----------------------------------
table(training_data$Fatal)
# Very few data examples, Fatal does not match the description in some cases
# One of the data samples is fatal, but has no citation => I think I'll remove this one
table(subset(training_data, Fatal == 'Yes')$Citation)
View(subset(training_data, Fatal == 'Yes'))
count(training_data, vars=c('Fatal', 'Citation'))


# ------------ PropertyDamage -------------------------
# ------------ Strong correlation between those two as well ---
table(training_data$ProperyDamage)
View(training_data[grep('DAMAGE', training_data$Description), ])
count(training_data, vars=c('ProperyDamage', 'Citation'))


# ------------CommercialLicense -----------------------
# FIRST: no need to grep here, those are set properly
# Second: this field is weakly correlated to the Citation
table(training_data$CommercialLicense)
View(training_data[grep('COMMERCIAL', training_data$Description), ])
count(training_data, vars=c('CommercialLicense', 'Citation'))

# ------------ HAZMAT --------------------------------
table(training_data$HAZMAT)
# Be very careful with this one, you can't just grep it. It sometimes is something
# really stupid, or contains "NONHAZARDOUS" in the message
# Maybe: grep for those that contain HAZARDOUS, but bot "NONHAZARDOUS"
View(training_data[grep('HAZARD', training_data$Description), ])
count(training_data, vars=c('CommercialLicense', 'Citation'))

# ------------ CommercialVehicle ---------------------
# ? Need to grep here: no inputs in the CommercialVehicle field
table(training_data$CommercialVehicle)
View(training_data[grep('COMMERCIAL', training_data$Description), ])

# ------------ WorkZone ------------------------------
# Very weak correlation here, almost negative. Not a lot of instances though ... maybe
# disregard this feature as well.
table(training_data$WorkZOne)
View(training_data[grep('WORK', training_data$Description), ])
count(training_data, vars=c('WorkZOne', 'Citation'))

# ------------ Special field: ContributedToAccident -------------
# a 100% correlation between citation and caused accidents
table(training_data$ContributedToAccident)
table(subset(training_data, ContributedToAccident == 'Yes')$Citation)
count(training_data, vars=c('ContributedToAccident', 'Citation'))



# ------------ Gender ------------------------
# ---- Gender is irrelevant for the model, I should probably cut this attribute off----
table(training_data$Gender)
table(subset(training_data, Gender == 'M')$Citation)
table(subset(training_data, Gender == 'F')$Citation)
count(training_data, vars=c('Gender', 'Citation'))


# TODO: Examine the counts of the above attributes in the test data, to know which ones are dangerous
test_data = read.csv("test.csv", sep=",")
View(test_data)
table(training_data$Accident)
table(training_data$Belts)
table(training_data$Alcohol)
table(training_data$PersonalInjury)
table(training_data$ProperyDamage)
table(training_data$Fatal)
table(training_data$CommercialVehicle)
table(training_data$CommercialLicense)
table(training_data$HAZMAT)
table(training_data$WorkZOne)


# Yes / No attributes are done



