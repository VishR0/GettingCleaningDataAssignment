##M3 Assignment


library(dplyr)


### Part 0.1 - Obtain Dataset

# S0.1.1 - Download zip file containing data, contingent on already file not being downloaded
zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipname <- "UCI HAR Dataset.zip"

if (!file.exists(zipname)) {
  download.file(zipurl, zipname, mode = "wb")
}

# S0.1.2 - Unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipname)
}

### Part 0.2 - Read data into R

# S0.2.1 - Read training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# S0.2.2 - Read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# S0.2.3 - Read features
features <- read.table("UCI HAR Dataset/features.txt")
features[,2] <- as.character(features[,2])

# S0.2.4 - Read activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


### Part 1.1 - Merge training and test datasets into one

# S1.1.1 - Bind each data tables to make single data table

humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# remove individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# assign column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")

### Part 2.1 - Extract only the mean and standard deviation

# S2.1.1 - find and keep columns with mean, std
columnsKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsKeep]


### Part 3.1 - Use descriptive activity names to name the activities in the dataset

# S3.1.1 - Replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])


# Part 4 - Label dataset with descriptive variable names

# S4.1.1 - Get column names
humanActivityCols <- colnames(humanActivity)

# S4.2.1 - Clean column names
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# S4.3.1 - Rename column names
colnames(humanActivity) <- humanActivityCols


# Part 5 - Create dataset with the average of each variable, per activity, per subject

# S5.1.1 - Group by subject and activity and average each variable

humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_all(.funs = mean)


# S5.1.2 write to "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)