# This R Script loads all the relevat data for this project and return a tidy an 
# clean set of data following the steps:

# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.


# First of all we read all the .txt files

setwd("~/R/Data Science JH University/Getting and ceaning data/Assignment Week 4/UCI HAR Dataset")

features<-read.table("features.txt",sep = "",header = F)
activity_labels<-read.table("activity_labels.txt",sep = "",header = F)

setwd("~/R/Data Science JH University/Getting and ceaning data/Assignment Week 4/UCI HAR Dataset/train")

x_train<-read.table("X_train.txt",sep = "",header = F)
subject_train<-read.table("subject_train.txt",sep = "",header = F)
y_train<-read.table("y_train.txt",sep = "",header = F)

setwd("~/R/Data Science JH University/Getting and ceaning data/Assignment Week 4/UCI HAR Dataset/test")

x_test<-read.table("X_test.txt",sep = "",header = F)
subject_test<-read.table("subject_test.txt",sep = "",header = F)
y_test<-read.table("y_test.txt",sep = "",header = F)

# 1. We merge the training and the test sets to create one data set called x_train_test.
#    Before that, we merge the columns y_train and subject_train fo both test and train:  

x_train<-cbind(y_train,x_train)
x_train<-cbind(subject_train,x_train)
x_test<-cbind(y_test,x_test )
x_test<-cbind(subject_test,x_test)

x_train_test<-rbind(x_train,x_test)

# 4. Appropriately labels the data set with descriptive variable names.

names(x_train_test)[1]<- "Subject" 
names(x_train_test)[2]<- "Activity"
names(x_train_test)[3:563]<- as.character(features[,2])

# 2. We extract only the measurements on the mean and standard deviation for each measurement.

x_train_mean_std<-x_train_test[,grepl("mean|std|Activity|Subject",names(x_train_test))]

# 3. We use descriptive activity names to name the activities in the data set and arrange
#    the factors properly with factor() so that the order is 1WALKING 2....

x_train_mean_std[,2]<- activity_labels[match(x_train_mean_std$Activity,activity_labels[,1]),2]

x_train_mean_std$Activity<- factor(x_train_mean_std$Activity, levels = (activity_labels[,2]))

# We put the columns Subject and activity in order so that "Subjects" is 1-30 in order
# and "Activity"  is 1.WALKING-6.LAYING in order

x_train_test_ordered<-arrange(x_train_mean_std,Subject,Activity)

# 5. Data set with the average of each variable for each activity and each subject.

x_train_mean_std %>%
  
  group_by(Subject,Activity) %>%
  
  summarise_all(mean)
