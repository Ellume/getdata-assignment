###########################################
##                                       ##
## Coursera Data Science: Get/Clean Data ##
##                                       ##
##       Assignement: Clean Data         ##
##                                       ##
###########################################
##                                       ##
##          Created By Ellume            ##
##           August 19, 2015             ##
##                                       ##
###########################################


# Change to my working directory (commented out for assignment submission)
# setwd("C:/Users/Ellume/Git/datasciencecoursera/getdata/Assignment")

# Require dplyr package
require("dplyr")

# Read in features and activity labels
features = read.table("./UCI HAR Dataset/features.txt")
activity.labels = read.table("./UCI HAR Dataset/activity_labels.txt")

# Use only Mean and Standard Deviation features as required by assignment
features.mean=grep("(mean\\(\\)|gravityMean|AccMean)",features[,2],ignore.case=T)
features.sd=grep("std\\(\\)",features[,2],ignore.case=T)
features.used=c(features.mean, features.sd)

#######

# Read in data for test subjects
test.subject = read.table("./UCI HAR Dataset/test/subject_test.txt")
colnames(test.subject)="Subject"
test.subject=as.factor(test.subject[[1]])
test.count=nrow(test.subject)

# Read in, select used features, and set readable variable names for test subjects
test.measure = read.table("./UCI HAR Dataset/test/X_test.txt")
test.measure.used=test.measure[,features.used]
colnames(test.measure.used)=features[features.used,2]

# Read and convert activity data into a readable format
test.activity = read.table("./UCI HAR Dataset/test/Y_test.txt")
for (i in activity.labels[,1]) {
  test.activity[test.activity==i]=as.character(activity.labels[i,2])
}
test.activity=as.factor(test.activity[[1]])

# create table for test subjects and create data set variable
test.dataset=as.factor(rep("TEST",n=test.count))
test=cbind(subject=test.subject,
           dataset=test.dataset,
           activity=test.activity,
           test.measure.used)

#######

# Read in data for train subjects
train.subject = read.table("./UCI HAR Dataset/train/subject_train.txt")
colnames(train.subject)="Subject"
train.subject=as.factor(train.subject[[1]])
train.count=nrow(train.subject)

# Read in, select used features, and set readable variable names for train subjects
train.measure = read.table("./UCI HAR Dataset/train/X_train.txt")
train.measure.used=train.measure[,features.used]
colnames(train.measure.used)=features[features.used,2]

# Read and convert activity data into a readable format
train.activity = read.table("./UCI HAR Dataset/train/Y_train.txt")
for (i in activity.labels[,1]) {
  train.activity[train.activity==i]=as.character(activity.labels[i,2])
}
train.activity=as.factor(train.activity[[1]])

# create table for train subjects and create data set variable
train.dataset=as.factor(rep("TRAIN",n=train.count))
train=cbind(subject=train.subject,
           dataset=train.dataset,
           activity=train.activity,
           train.measure.used)

########

# Combine the test and train tables into completed data set
cleaned.data=rbind(test,train)

# Create a tidy summary table
tidy.data=aggregate(cleaned.data[,-3:-1],
                    by=list(subject=cleaned.data$subject,
                            activity=cleaned.data$activity,
                            dataset=cleaned.data$dataset),
                    mean)

# Write the table to txt file
write.table(tidy.data, "./tidy.data.txt", row.names=F)