#######################################################################################################
## 1. Merges the training and the test sets to create on data set.
#######################################################################################################




train_set <- read.table("./project/UCI HAR Dataset/train/X_train.txt")
dim(train_set)  ##[1] 7352  561
test_set <- read.table("./project/UCI HAR Dataset/test/X_test.txt")
dim(test_set)   ##[1] 2947  561

one.data.set <- rbind (train_set, test_set)
dim(one.data.set) ##[1] 10299   561


#######################################################################################################
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
#######################################################################################################

features <- read.table("./project/UCI HAR Dataset/features.txt")
dim(features) ## [1] 561   2

# list of features
list.features <- features [,2]; list.features <- as.character(list.features)

# list of features including "mean()|std()"
extract <- features[grepl("mean()|std()", list.features),1]
length(extract) ## [1] 79

# extract only the measurements on mean and std from one.data.set
mean.std <- one.data.set[,extract]
dim(mean.std) ## [1] 10299    79

# labeling of each field of feature after extraction
colnames(mean.std) <- features[extract,2]

#######################################################################################################
## 3. Uses descriptive activity names to name the activity in the data set. 
## 4. Appropriately labels the data set with descriptive activity names.
#######################################################################################################

# descriptive activity names loading

activity_names <- read.table("./project/UCI HAR Dataset/activity_labels.txt")
colnames(activity_names) <- c("activity_code", "activity_name")


# activity label extract and merge from training and test data sets

activity_train <- read.table("./project/UCI HAR Dataset/train/y_train.txt")
dim(activity_train) ## [1] 7352    1
activity_test <- read.table("./project/UCI HAR Dataset/test/y_test.txt") 
dim(activity_test)  ##  [1] 2947    1

activity_total <- rbind(activity_train, activity_test)
dim(activity_total)  ##  [1] 10299     1
colnames(activity_total) <- "activity_code"

# labeling with descriptive activity with activity code

descrip.activity.total <- merge (x = activity_total, y = activity_names, by = "activity_code", all.x = TRUE)
dim(descrip.activity.total)

# combine with mean.std datset

mean.std.activity <- cbind (descrip.activity.total, mean.std)
dim(mean.std.activity) ##[1] 10299    81


#######################################################################################################
## 5. Create a second, independent tidy data set with the average of each variable for each activity 
##     and each subject. 
#######################################################################################################

# extract of subject id

subject_train <- read.table("./project/UCI HAR Dataset/train/subject_train.txt")
dim(subject_train) ## [1] 7352    1
subject_test <- read.table("./project/UCI HAR Dataset/test/subject_test.txt") 
dim(activity_test)  ##  [1] 2947    1

# binding suject id

subject_total <- rbind(subject_train, subject_test)
dim(subject_total)  ##  [1] 10299     1
names(subject_total) <- "subject_id"

head(subject_total)

# merge subject id with mean.std.activity data set

final <- cbind(subject_total, mean.std.activity)
dim(final)  ##cbind(subject_total, mean.std.activity)
dim(final) ##  [1] 10299    82

# independent tidy data set with the avarage of each variable for each activity and each subject.

library(reshape2)

final.melt <- melt(final, id = c("subject_id", "activity_name", "activity_code"), measure.vars = features[extract,2] )

tidy.data <- dcast(final.melt, subject_id + activity_name ~ variable, mean)

write.table(tidy.data, file = "./tidy_final.txt", row.names = FALSE)

