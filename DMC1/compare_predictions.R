# Use this script to compare model predictions on test data

# Load predictions with as dataframe with (id,prediction) columns
first_submission = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/predictions_dmc1_first_submission.csv", sep=",",na.strings=c("","?","NA"))
second_submission = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/predictions_dmc1_second_submission.csv", sep=",",na.strings=c("","?","NA"))
ada_NN = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Jan Ada/predictions_dmc1_ada_no_pca_no_sampl.csv", sep=",",na.strings=c("","?","NA"))
ada_NY = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Jan Ada/predictions_dmc1_ada_no_pca_with_sampl.csv", sep=",",na.strings=c("","?","NA"))
ada_YN = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Jan Ada/predictions_dmc1_ada_with_pca_no_sampl.csv", sep=",",na.strings=c("","?","NA"))
ada_YY = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Jan Ada/predictions_dmc1_ada_with_pca_with_sampl.csv", sep=",",na.strings=c("","?","NA"))
rf_NN = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Nasko RF/predictions_dmc1_rf_noPCA_noSampling.csv", sep=",",na.strings=c("","?","NA"))
rf_NY = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Nasko RF/predictions_dmc1_rf_noPCA_withSampling.csv", sep=",",na.strings=c("","?","NA"))
rf_YN = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Nasko RF/predictions_dmc1_rf_withPCA_noSampling.csv", sep=",",na.strings=c("","?","NA"))
rf_YY = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Nasko RF/predictions_dmc1_rf_withPCA_withSampling.csv", sep=",",na.strings=c("","?","NA"))
gbm_NN = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Aleks GBM/predictions_dmc1_gbm_NoPCA_NoSampling.csv", sep=",",na.strings=c("","?","NA"))
gbm_NY = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Aleks GBM/predictions_dmc1_gbm_NoPCA_withSampling.csv", sep=",",na.strings=c("","?","NA"))
gbm_YN = read.csv("C:/Users/user/Dropbox/Fakulteti/TUM/Business Analytics/DMC 2015/DMC1/Aleks GBM/predictions_dmc1_gbm_withPCA_noSampling.csv", sep=",",na.strings=c("","?","NA"))


# rows with different predictions are marked as FALSE
same_prediction = rf_YN$prediction ==gbm_YN$prediction
table(same)

# Proportion of "yes/no" predictions
prop.table(table(rf_YN$prediction))


# Row numbers predicted as 'yes' by gbm_YN model
true_pre = which(gbm_YN$prediction == 'y')
# Row numbers predicted as 'no' by gbm_YN model
false_pre = which(gbm_YN$prediction == 'n')

# How many of yes and no predictions by model above are differently predicted by rf_YN
table(rf_YN$prediction[true_pre])
table(rf_YN$prediction[false_pre])
