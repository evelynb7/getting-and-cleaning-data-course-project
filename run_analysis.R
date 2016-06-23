################################################################################

## Getting and Cleaning Data Assignment
## Evelyn B.
## 23 June 2016

## Assignement Requirements:
 #  R1. Merges the training and the test sets to create one data set.
 #  R2. Extracts only the measurements on the mean and standard deviation
 #      for each measurement.
 #  R3. Uses descriptive activity names to name the activities in the data set
 #  R4. Appropriately labels the data set with descriptive variable names.
 #  R5. From the data set in step 4, creates a second, independent tidy data set
 #      with the average of each variable for each activity and each subject.

################################################################################


run_analysis <- function(workingdir = getwd())
{
  setwd(workingdir)
  
  ## Load library
  library(reshape2)
  
  # Data source
  fileURL   <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI HAR Dataset.zip"
  filename  <- "UCI HAR Dataset.zip"
  
  ## Common files
  activitytxt       <- "UCI HAR Dataset/activity_labels.txt"
  featurestxt       <- "UCI HAR Dataset/features.txt"
  
  ## TEST files
  testSubjecttxt    <- "UCI HAR Dataset/test/subject_test.txt"
  testSettxt        <- "UCI HAR Dataset/test/X_test.txt"
  testActivitytxt   <- "UCI HAR Dataset/test/y_test.txt"
  
  ## TRAIN files
  trainSubjecttxt   <- "UCI HAR Dataset/train/subject_train.txt"
  trainSettxt       <- "UCI HAR Dataset/train/X_train.txt"
  trainActivitytxt  <- "UCI HAR Dataset/train/y_train.txt"
  
  neededFiles <- c(activitytxt, featurestxt, 
                   testSubjecttxt, testSettxt, testActivitytxt, 
                   trainSubjecttxt, trainSettxt, trainActivitytxt)
  allFiles    <- list.files(recursive = TRUE)
  
  ## Check if files exist before downloading
  if(!all(neededFiles %in% allFiles))
  {
    if(!file.exists(filename))
    {
      download.file(fileURL, filename)
    }
    unzip(filename)
  }
  
  
  ## Load Activity and Features into R
  activity      <- read.table(activitytxt)
  activity[,2]  <- as.character(activity[,2])
  
  features      <- read.table(featurestxt)
  features[,2]  <- as.character(features[,2])
  
  #################### Start working with Features data ###############
  
  # Get index of mean & std (including meanFreq) columns
  xFeatureIdx   <- grep(".*mean.*|.*std.*", features[,2])
  
  # Get feature names for mean & std (including meanFreq)
  xFeatureNames <- features[xFeatureIdx,2]
  
  # Tidy up the names
  xFeatureNames <- gsub("-mean", "-Mean", xFeatureNames)
  xFeatureNames <- gsub("-std", "-Std", xFeatureNames)
  xFeatureNames <- gsub("[:():]", "", xFeatureNames)
  xFeatureNames <- gsub("tBody", "time-Body", xFeatureNames)
  xFeatureNames <- gsub("tGravity", "time-Gravity", xFeatureNames)
  xFeatureNames <- gsub("fBody", "freq-Body", xFeatureNames)
  xFeatureNames <- gsub("Acc", "Accel", xFeatureNames)
  xFeatureNames <- gsub("Mag", "Magnitude", xFeatureNames)
  xFeatureNames <- gsub("BodyBody", "Body", xFeatureNames)
  
  #################### End working with features data #################
  
  
  #################### Start working with TEST data ###################
  
  ## Load TEST data into R
  testSubect    <- read.table(testSubjecttxt)
  testActivity  <- read.table(testActivitytxt)
  testSet       <- read.table(testSettxt)
  # R2. Extract mean and std columns
  testSet       <- testSet[, xFeatureIdx] 
  
  ## Create TEST data table
  testDT <- cbind(testSubect, testActivity, testSet)
  
  #################### End working with TEST data #####################
  
  #################### Start working with TRAIN data ##################
  
  ## Load TRAIN data into R
  trainSubect    <- read.table(trainSubjecttxt)
  trainActivity  <- read.table(trainActivitytxt)
  trainSet       <- read.table(trainSettxt)
  # R2. Extract mean and std columns
  trainSet       <- trainSet[, xFeatureIdx] 
  
  ## Create TRAIN data table
  trainDT <- cbind(trainSubect, trainActivity, trainSet)
  
  #################### End working with TRAIN data ####################
  
  
  ## R1. Merge TEST and TRAIN data tables
  mergedDT <- rbind(testDT, trainDT)
  
  ## Make variables with character values into factor variables
  mergedDT[,1] <- as.factor(mergedDT[,1])
  # R3. Also setting descriptive activity names
  mergedDT[,2] <- factor(mergedDT[,2], levels = activity[,1], labels = activity[,2])
  
  ## R4. Rename variables
  colnames(mergedDT) <- c("subject", "activity", xFeatureNames)
  
  ## R5a. Average each variable for each activity and subject
  mergedDT <- melt(mergedDT, id = c("subject", "activity"))
  mergedDT <- dcast(mergedDT, subject + activity ~ variable, mean)
  
  ## R5b. Export to .csv
  write.table(mergedDT, file = "tidydata.txt", sep = "\t", row.names = FALSE, quote = FALSE)
}