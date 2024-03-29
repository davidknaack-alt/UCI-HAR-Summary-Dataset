## Code Book
##### David Knaack
##### Aug 3, 2019

This document describes the fields and values in the target dataset, ‘UCI-HAR-Summary-Dataset.txt’,
created from the run_analysis.R script.

The target dataset includes the following fields:

- **Subject** : ID of person wearing the Samsung device.  Range of 1-30 which includes all 
                participants in the study.

- **Activity**: activity the subject was doing when the measurement was taken. 

    + WALKING
    + WALKING_UPSTAIRS
    + WALKING_DOWNSTAIRS
    + SITTING
    + STANDING
    + LAYING
    
- **Axis** : indicates the axial direction of the measurement value. Values:

    + X
    + Y
    + Z 
    + NA
    
- **Mean** : average mean value of the respective variable by each subject and activity.  See 
             the note on the 'Feature_Variable' prefix below that indicates the measurement unit.

- **Std** : average standard deviation of the respective variable by each subject and activity.

- **Feature_Variable** : **t** pre-fix denotes a **time** (normalized and bound to absolute value 
                         range of 0:1) variable; **f** pre-fix denotes a **frequency** (normalized 
                         and bound to absolute value range of 0:1)  variable.

    + tBodyAcc
    + tGravityAcc
    + tBodyAccJerk
    + tBodyGyro
    + tBodyGyroJerk
    + tBodyAccMag
    + tGravityAccMag
    + tBodyAccJerkMag
    + tBodyGyroMag
    + tBodyGyroJerkMag
    + fBodyAcc-XYZ
    + fBodyAccJerk-XYZ
    + fBodyGyro-XYZ
    + fBodyAccMag
    + fBodyAccJerkMag
    + fBodyGyroMag
    + fBodyGyroJerkMag

#### Notes on Feature Selection from the Source Dataset documentation

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals 
tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a 
constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass 
Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal 
was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using 
another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk 
signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals 
were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, 
tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, 
fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to 
indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.


The set of variables that were created from these signals that are extracted and output in the target dataset are: 

  + mean(): Mean value
  + std(): Standard deviation
