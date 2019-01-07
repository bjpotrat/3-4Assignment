##Activity 1: Merges the training and the test sets to create one data set.
#Step 1: Read in data files
pathdata <- 'UCI HAR Dataset'
#Reading training tables - xtrain / ytrain, subject train
xtrain <- read.table(file.path(pathdata, "train", "X_train.txt"),header = FALSE)
ytrain <- read.table(file.path(pathdata, "train", "y_train.txt"),header = FALSE)
subject_train <- read.table(file.path(pathdata, "train", "subject_train.txt"),header = FALSE)
xtest <- read.table(file.path(pathdata, "test", "X_test.txt"),header = FALSE)
ytest <- read.table(file.path(pathdata, "test", "y_test.txt"),header = FALSE)
subject_test <- read.table(file.path(pathdata, "test", "subject_test.txt"),header = FALSE)
features <- read.table(file.path(pathdata, "features.txt"),header = FALSE)
activityLabels <- read.table(file.path(pathdata, "activity_labels.txt"),header = FALSE)

#Step 2: Create Column Labels Data
colnames(xtrain) = features[,2]
colnames(ytrain) = "activityId"
colnames(subject_train) = "subjectId"
colnames(xtest) = features[,2]
colnames(ytest) = "activityId"
colnames(subject_test) = "subjectId"
colnames(activityLabels) <- c('activityId','activityType')

#Step 3: Merging Everything together into mrg_result
mrg_train = cbind(ytrain, subject_train, xtrain)
mrg_test = cbind(ytest, subject_test, xtest)
mrg_result = rbind(mrg_train, mrg_test)

##Activity 2: Extracts only the measurements on the mean and standard deviation for each measurement.
#Step 1: Get the column names
colNames = colnames(mrg_result)
#Step 2: Select all the mean and standards and the correspondonin activityID and subjectID 
mean_and_std =   (grepl("activityId" , colNames) | 
                  grepl("subjectId" , colNames) | 
                  grepl("mean.." , colNames) | 
                  grepl("std.." , colNames))
#Step 3: Create the subset for the result MeanAndStd_result
MeanAndStd_result <- mrg_result[ , mean_and_std == TRUE]

##Activity 3: Uses descriptive activity names to name the activities in the data set.
MeanAndStdWithActivityNames <- merge(MeanAndStd_result, activityLabels, by='activityId', all.x=TRUE)

##Activity 4: Appropriately labels the data set with descriptive variable names.
#Already Completed

##Activity 5: From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.
TidyDataSet <- aggregate(. ~subjectId + activityId, MeanAndStdWithActivityNames, mean)
TidyDataSet <- TidyDataSet[order(TidyDataSet$subjectId, TidyDataSet$activityId),]
write.table(TidyDataSet, "Assignment3-4TidyDataSet.txt", row.name=FALSE)