training_data = read.csv("training.csv", sep=",")
test_data = read.csv("test.csv", sep=",")

# almost all of them
training_data$State
training_data$VehicleType
training_data$Make
training_data$Color
training_data$Charge
training_data$Race
training_data$Gender
training_data$DriverState
training_data$DLState
training_data$ArrestType


# numeric attributes
training_data$Year
training_data$Longitude
training_data$Latitude

# Considerations:

# DONE:
# --------------------------------------------------------------------------------------
# Remove the Agency, SubAgency, TimeOfStop and Geolocation columns, since those do not contain different values
# Remove the Location column use Longitude and Latitude instead
# The field Article contains just 1 value, and "Maryland rules" <- (total bull), therefore remove it
# Think about impossible values
# Discretization: discretize Location
# Extract some of the yes / no fields from the descriptions.
# 1. Does it contain "SPEED" ---> 20% of all unique ones do
#   A lot of them are in the form: EXCEEDING MAXIMUM SPEED 82 MPH IN A POSTED 55 MPH ZONE
#   Extract a new field from this: "speed over limit"
# 1. Check for : EXCEEDING
# 2. Does it contain any of the already available fields (the predefined ones)
# 3. Does it contain LIFE (as in endangered life)
# 4. Does it contain DANGER
# 5. Besides alcohol, also check: SUBSTANCE, DRUG
# 6. Check for: LICENSE (as in Driver's license)
# 7. Check for: CROSSWALK
# 8. Check for: REGIST (of the vehicle) or REG.
# 9. Check for: LIGHTS or LAMP
# 10. Check for: PHONE
# 11. Check for: RED & SIGNAL
# 12. Check for: MEDICAL & CERTIFICATE
# 13. Check for: RIGHT OF WAY or RIGHT-OF-WAY
# 14. Check for: HIGHWAY
# 15. Check for: NO PASSING ZONE
# 16. Check for: UNINSURED
# 17. Check for: TURN
# 18. Check for: PEDESTRIAN
# 19. Check for: CHILD
# 20. Check for: PASSANGER or OCCUPANT
# 21. Check for: STOP
# 22. Check for: TIRE
# 23. Check for: SIGNS
# 24. Check for: POLICE or OFFICER
# 25. Check for: LANE

# TODO:
# ----------------------------------------------------------------------
# Consider merging the Make and Model into 1 field (car)
# Consider stripping off the first number in "Charge" field into a separate field
# Consider making a field: driverstate <is_equal> DLState


# LOW PRIO:
# Consider merging longitude and latitude to one field: Geolocation
# Numeric to factor: consider factorizing year


# Kinda intersting: overlaps in description based on CITATION
length(unique(subset(training_data, Citation == 'Yes')$Description))
length(unique(subset(training_data, Citation == 'No')$Description))

# intersection is 107 long
intersect(unique(subset(training_data, Citation == 'Yes')$Description), unique(subset(training_data, Citation == 'No')$Description))

# explore the different words (shingles) in the DESCRIPTION
words <- character(0)

for (descr in training_data$Description) {
  words <- append(words, strsplit(descr, " ")[[1]])
}

for (descr in test_data$Description) {
  words <- append(words, strsplit(descr, " ")[[1]])
}
words <- as.data.frame(table(words))
words <- words[order(words$Freq),]





