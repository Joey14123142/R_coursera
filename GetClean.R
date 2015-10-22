library(reshape2)
alab <- read.table("UCI HAR Dataset/activity_labels.txt") # 6*2
fea <- read.table("UCI HAR Dataset/features.txt") #561*2
s_test <- read.table("UCI HAR Dataset/test/subject_test.txt") # 2947*1
x_test <- read.table("UCI HAR Dataset/test/X_test.txt") # 2947*561
y_test <- read.table("UCI HAR Dataset/test/Y_test.txt") # 2947*1
s_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("UCI HAR Dataset/train/Y_train.txt")

#Merges the training and the test sets to create one data set.
test <- cbind(x_test,s_test, y_test)
train <- cbind(x_train,s_train, y_train)
pretotal <- rbind(test, train)

#Appropriately labels the data set with descriptive variable names. 
colnames(pretotal) <- c(tolower(fea$V2), "subjectId", "activity")

#Extracts only the measurements on the mean and standard deviation for each measurement.
filt <- grepl("mean|std", fea$V2)
total <- pretotal[,filt]

#Uses descriptive activity names to name the activities in the data set
total$activity <- alab[total$activity,2]


#From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy <- dcast(melt(total, id=c("subjectId","activity")), subjectId+activity ~ variable, mean)
write.table(tidy, file = "tidy.txt")             


