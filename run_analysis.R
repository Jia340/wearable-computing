library(reshape2)
##Read data
#Read the measured data. Merge the test and training datasets as one.
X_test <- read.table("UCI HAR Dataset/test/X_test.txt", quote="\"", stringsAsFactors=FALSE)
X_train <- read.table("UCI HAR Dataset/train/X_train.txt", quote="\"", stringsAsFactors=FALSE)
X_data <- rbind(X_test, X_train)
rm(X_test, X_train)
#Read the activity labels. Merge the test and training datasets as one.
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", quote="\"", stringsAsFactors=FALSE)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", quote="\"", stringsAsFactors=FALSE)
y_data <- rbind(y_test,y_train)
rm(y_test, y_train)
#Read the subject labels. Merge the test and training datasets as one.
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", quote="\"", stringsAsFactors=FALSE)
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", quote="\"", stringsAsFactors=FALSE)
subject_data <- rbind(subject_test,subject_train)
rm(subject_test,subject_train)

##Filter measurements on mean and standard deviation
features <- read.table("UCI HAR Dataset/features.txt", quote="\"", stringsAsFactors=FALSE)
f <- features[grepl("mean\\(\\)|std\\(\\)",features$V2),]
X_data <- X_data[,f$V1]

##Merge the three datasets
mdf <- cbind(X_data, y_data, subject_data)

##Modify the column names
colnames(mdf) <- c(f$V2,c("activities","subjects"))

##Annotate activities
activ <- read.table("UCI HAR Dataset/activity_labels.txt", quote="\"", stringsAsFactors=FALSE)
act_list <- as.list(activ$V2)
act_vec <- sapply(mdf$activities,function(x) act_list[[x]])
mdf$activities <- act_vec

##Generate the tidy dataset
tidy <- as.data.frame(t(sapply(split(mdf,list(mdf$activities,mdf$subjects)),function(x) apply(x[,c(1:nrow(f))],2,mean))))
tidy$activities <- as.character(sapply(row.names(tidy),function(x) strsplit(x,"\\.")[[1]][1]))
tidy$subjects <- as.character(sapply(row.names(tidy),function(x) strsplit(x,"\\.")[[1]][2]))

##Save the dataset
write.table(tidy, file="wearable_computing_tidy.txt",sep="\t",row.names=F,col.names=T,quote=F)
