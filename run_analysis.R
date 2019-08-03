## Getting and Cleaning Data - Course Project

# By David Knaack

# Project Objective - demonstrate that I can collect, clean, and work with a data set.

# This R script (run_analysis) does these following tasks:

#   1. Merges the training and test sets to create a data set.
#   2. Extracts the mean and standard deviation for each measurement.
#   3. Uses descriptive activity names for the names of the columns in the data set.
#   4. Appropriately labels the data set with descriptive variable names.
#   5. Uses the data set from step 4 and creates another tidy data set with the average
#      of each variable for each activity.

# Saving current workind directory. This is just in case you want to go back to 
# your original directory after running this in your UCI HAR Dataset directory.
OldPath<- getwd()

# This sets the directory to the "UCI HAR Dataset" Directory. If your directory is
# different please change the code below according to your UCI HAR Dataset file.
setwd("~/Coursera/UCI HAR Dataset")

# This dataset assumes the following files are in your working directory
# ------------------------------------------------------------------------------------

#   1. `activity_labels.txt`   => Connects the class labels with their activity name.
#   2. `features_info.txt` => Shows info. on variables used on the feature vector.
#   3. `features.txt`   => Lists all features.
#   4. `README.txt`
#   5. `test/X_test.txt` => Test set
#   6. `test/subject_test.txt` => Test subject numbers
#   7. `test/y_test.txt` => Test labels
#   8. `train/X_train.txt` => Training set
#   9. `train/subject_train.txt` => Training subject numbers
#   10. `train/y_train.txt` => Training labels

## This R package reads the test and training data tables for the 


# load R packages

library(dplyr)
library(tidyr)

# read the 'test' dataset files

X_test <- read.table("test/X_test.txt")
Y_test <- read.table("test/y_test.txt")
S_test <- read.table("test/subject_test.txt")

# read the 'training' dataset files

X_train <- read.table("train/X_train.txt")
Y_train <- read.table("train/y_train.txt")
S_train <- read.table("train/subject_train.txt")

# read the 'label' files

f_labs <- read.table("features.txt")
a_labs <- read.table("activity_labels.txt")

# add new column to indicate data group

S_train <- mutate(S_train, Class = "train")
S_test <- mutate(S_test, Class = "test")

# Merge the train and test datasets for X(set), y(labels), and subject

All_X <- bind_rows(X_train, X_test)
All_Y <- bind_rows(Y_train, Y_test)
All_S <- bind_rows(S_train, S_test)

# Assign descriptive column names to merged datasets

colnames(All_X) <- f_labs$V2
colnames(All_Y) <- c("Activity")
colnames(All_S) <- c("Subject", "Class")

# combine all dataset columns so they are within a single dataset

all <- bind_cols(All_X, All_Y, All_S)
all_t <- tbl_df(all)
rm(all)

# ensure column names are unique

valid_column_names <- make.names(names=names(all_t), unique=TRUE, allow_ = TRUE)
names(all_t) <- valid_column_names

# Extract only mean or standard deviation variable values

all_t <- all_t %>%
    select(Class, Subject, Activity, contains(".mean."), contains(".std.")) %>%
    mutate(Activity = factor(Activity, labels = a_labs$V2)) %>%
    gather(Measurement, Meas_val, -(Class:Activity)) %>%
    separate(Measurement, c("Feature_Variable", "Stat_Type", "Axis"))

# Create a second dataset that contains the average of each variable for each 
# activity and each subject. 

tidy_dataset <- all_t %>%
    group_by(Subject, Activity, Feature_Variable, Stat_Type, Axis) %>%
    summarize(Avg_val = mean(Meas_val)) %>%
    spread(Stat_Type, Avg_val) %>%
    rename("Mean" = mean, "Std" = std) # Also Task 4

# Write output of summary dataset to a csv file

write.csv(tidy_dataset, "UCI-HAR-Summary-Dataset.csv", row.names = FALSE)