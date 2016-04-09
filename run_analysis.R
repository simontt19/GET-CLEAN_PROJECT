setwd("D:/01-PRIVATE/workspace/r/getclean")
rm(list=ls())

# ==================================================
#   Description:
#   Human Activity Recognition Using Smartphones Dataset Cleaning
# 
# History:
#   1.00  2016-04-09  Simon  Creation
# ==================================================
  

# 1. Merges the training and the test sets to create one data set.

## 1.1 download and unzip file

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","c2w4a1.zip")
c2w4a1<- unzip("c2w4a1.zip", exdir = "D:/01-PRIVATE/workspace/r/getclean/data")

## 1.2 load data
train_x <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
train_y <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
train_subject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
test_x <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
test_y <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
test_subject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")

## 1.4: merges the training and the test sets
train_Data <- cbind(train_subject, train_y, train_x)
test_Data <- cbind(test_subject, test_y, test_x)
Data <- rbind(train_Data, test_Data)


# ====================================================================================================

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

## 2.1 load feature name into R
featureName <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]

## 2.2  extract mean and standard deviation of each measurements
featureIndex <- grep(("mean\\(\\)|std\\(\\)"), featureName)
head(Data)
finalData <- Data[, c(1, 2, featureIndex+2)]
colnames(finalData) <- c("subject", "activity", featureName[featureIndex])

# ====================================================================================================
# 3. Uses descriptive activity names to name the activities in the data set

## 3.1 load activity data into R
activityName <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

## 3.2 replace 1 to 6 with activity names
finalData$activity <- factor(finalData$activity, levels = activityName[,1], labels = activityName[,2])


# ====================================================================================================
# 4. Appropriately labels the data set with descriptive variable names.

names(finalData) <- gsub("\\()", "", names(finalData))
names(finalData) <- gsub("^t", "time", names(finalData))
names(finalData) <- gsub("^f", "frequence", names(finalData))
names(finalData) <- gsub("-mean", "Mean", names(finalData))
names(finalData) <- gsub("-std", "Std", names(finalData))

# ====================================================================================================
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(dplyr)
groupData <- finalData %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(groupData, "./MeanData.txt", row.names = FALSE)
