# IMPORTANT NOTE: This R script is not a standalone script.

# The following dataset must first be downloaded and extracted
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# Please run this script in the extracted directory in order for it to perform correctly.

# CRITERIA:
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Good luck! - copy that!

# suppress warnings
options(warn=-1)
cat("Please be patient, this may take a few minutes. Processing... ")
cat("")
# progressbar code found on stackoverflow
progressbar <- function(x = sort(runif(20)), ...)
{
    pb <- txtProgressBar(...)
    for(i in c(0, x, 1)) {Sys.sleep(1); setTxtProgressBar(pb, i)}
    Sys.sleep(10)
    close(pb)
}
progressbar()

# test data: x=set, y=labels
test_set <- read.table("./test/X_test.txt",header=FALSE)
test_labels <- read.table("./test/y_test.txt",header=FALSE)
test_subject <- read.table("./test/subject_test.txt",header=FALSE)

# train data: x=set, y=labels
train_set <- read.table("./train/X_train.txt",header=FALSE)
train_labels <- read.table("./train/y_train.txt",header=FALSE)
train_subject <- read.table("./train/subject_train.txt",header=FALSE)

# merge x, y, subject
x <- rbind(test_set, train_set)
colnames(x) <- "feature"
y <- rbind(test_labels, train_labels)

# colnames(y) <- "activity_id"
subject <- rbind(test_subject, train_subject)
colnames(subject) <- "subject"

# factor: features
features <- read.table("features.txt")[,2]

# column names: x
names(x) <- features

# extract only mean and std deviation
extract_only <- grepl("mean|std", features)
x = x[,extract_only]

# activity labels
activities <- read.table("activity_labels.txt", header=FALSE, colClasses="character")

# column names
y$V1 <- factor(y$V1, levels=activities$V1, labels=activities$V2)
colnames(y) <- "activity"

# full merge
data <- cbind(x, y, subject)

data_mean <- sapply(data, mean, na.rm=TRUE)
data_sd <- sapply(data, sd, na.rm=TRUE)

# requires reshape2 to melt
library(reshape2)
data_melt <- melt(data, id = c("activity", "subject"))
data_tidy <- dcast(data_melt, activity + subject ~ variable, mean)

# finsh the task by saving uci_har_tidy.csv
write.csv(data_tidy, file='uci_har_tidy.csv')
cat("Complete.  Your new output file is named uci_har_tidy.csv")

# unsupress warnings
options(warn=0)
