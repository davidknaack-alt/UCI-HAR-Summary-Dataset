## README.md
##### David Knaack
##### Aug 3, 2019

Summarized Dataset of a Human Activity Recognition Using Smartphones Experiment
Version 1.0

This repository contains an R script to process and summarize a dataset of created from a set of experiments carried out with 
a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, 
WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded 
accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the 
training data and 30% the test data. 

### About the Source Dataset
The original source data represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full 
description is available at the site where the data was obtained: 

<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones> 

#### For each record in the source dataset it is provided:

  - Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
  - Triaxial Angular velocity from the gyroscope. 
  - A 561-feature vector with time and frequency domain variables. 
  - Its activity label. 
  - An identifier of the subject who carried out the experiment.


### About the Target Dataset

The run_analysis.R script R package uses the dplyr and tidyr R packages to process the source data as follows:

1. Loads the dependent R packages: **dplyr**  and **tidyr**.
2. Combines the test and training datasets in the 'UCI HAR Dataset/train' and 'UCI HAR Dataset/test' folders. 
      *Note: code does not read the files in the subfolders within these two directories*
3. Adds a new column to indicate data group (test or train).
4. Assigns descriptive column names to merged datasets from reading the features.txt in as a vector.
5. Assigns descriptive activity labels to merged datasets from reading the activity_labels.txt in as a vector.
6. Extract only mean or standard deviation variable values
7. Reshapes the data according to the following 'tidy dataset' principles:
  - Each variable forms a column
  - Each observation forms a row
  - Each table table stores data about one kind of observation.
8. Generates a second dataset that contains the average of each variable for each activity and each subject.  This is 
   written out to as a text file called, 'UCI-HAR-Summary-Dataset.txt'

#### To regenerate the target dataset:
1. Clone a local copy of this repository without making any modifications to the file structure.
2. Install the dependent dplyr and tidyr R packages.
3. Source the run_analysis.R script in RStudio or R (v3.2.1 or higher)
4. The target dataset will be written to the 'UCI-HAR-Summary-Dataset.csv' file and the 'UCI-HAR-Summary-Dataset.txt' file. 

### This repository includes the following files:

  - 'README.md'

  - 'CodeBook.md': Shows information about the variables used on the feature vector.

  - 'run_analysis.R' : R script that processes the source datasets to write out a smaller, summarized version.

#### Specific source files in 'UCI HAR Dataset' used by the R Script:

  - 'features.txt': List of all features.

  - 'activity_labels.txt': Links the class labels with their activity name.

  - 'features.txt': List of all features.

  - 'train/X_train.txt': Training set.

  - 'train/y_train.txt': Training labels.

  - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. 
    Its range is from 1 to 30. 

  - 'test/X_test.txt': Test set.

  - 'test/y_test.txt': Test labels.

  - 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window 
    sample. Its range is from 1 to 30. 
