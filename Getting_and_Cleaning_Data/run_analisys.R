library(data.table)
# we set the working path in which we have the data
myPath <- getwd()
pathWithData <- file.path(myPath, "Getting&Cleaning_Project","UCI HAR Dataset")

#we read the data from  data path
subjectTrain <- fread(file.path(pathWithData, "train", "subject_train.txt"))
subjectTest  <- fread(file.path(pathWithData, "test" , "subject_test.txt" ))

labelTrain <- fread(file.path(pathWithData, "train", "Y_train.txt"))
labelTest  <- fread(file.path(pathWithData, "test" , "Y_test.txt" ))

dataTrain <- read.table(file.path(pathWithData, "train", "X_train.txt"))
dataTest  <- read.table(file.path(pathWithData, "test" , "X_test.txt" ))
       
#merge data                 
allDataSubject <- rbind(subjectTrain, subjectTest)
setnames(allDataSubject, "V1", "subject")

allDataActivity <- rbind(labelTrain, labelTest)
setnames(allDataActivity, "V1", "activityNum")

allData <- rbind(dataTrain, dataTest)


allDataSubject <- cbind(allDataSubject, allDataActivity)
allData <- cbind(allDataSubject, allData)

#set key
setkey(allData, subject, activityNum)

#now we extract the mean and std

features <- fread(file.path(pathWithData, "features.txt"))
setnames(features, names(features), c("featureNum", "featureName"))


features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]

features$featureCode <- features[, paste0("V", featureNum)]


select <- c(key(allData), features$featureCode)
allData <- allData[, select, with=FALSE]

#use descriptive names
allActivityNames <- fread(file.path(pathWithData, "activity_labels.txt"))
setnames(allActivityNames, names(allActivityNames), c("activityNum", "activityName"))

#labels with descriptive names
allData <- merge(allData, allActivityNames, by="activityNum", all.x=TRUE)
setkey(allData, subject, activityNum, activityName)
allData <- data.table(melt(allData, key(allData), variable.name="featureCode"))
allData <- merge(allData, features[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)
allData$activity <- factor(allData$activityName)
allData$feature <- factor(allData$featureName)

#separate feature from feature name
grepthis <- function (regex) {
    grepl(regex, allData$feature)
}


n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
allData$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
allData$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
allData$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
allData$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))


allData$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
allData$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))

n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
allData$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))

#create tidy data with mean value

setkey(allData, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
allDataTidy <- allData[, list(count = .N, average = mean(value)), by=key(allData)]

