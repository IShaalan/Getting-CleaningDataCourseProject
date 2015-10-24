nicenames <- function(x, pattern, replacement,...){
  for(i in 1:length(pattern)){
    x <- gsub(pattern[i], replacement[i], x, ignore.case = TRUE,...)
  }
  x
}

library(dplyr)

testSet <- tbl_df(read.table("GettingNCleaning/Course Project/test/X_test.txt"))
trainSet <- tbl_df(read.table("GettingNCleaning/Course Project/train/X_train.txt"))
data  <- rbind(testSet, trainSet) ##1
featuresTable <- tbl_df(read.table("GettingNCleaning/Course Project/features.txt"))
columnsOfInterest <- featuresTable[grepl("mean", featuresTable$V2) | 
                                     grepl("std", featuresTable$V2),]
dataOfInterest <- data[,columnsOfInterest$V1] ##2

activityLabels <- tbl_df(read.table("GettingNCleaning/Course Project/activity_labels.txt"))

trainLabel <- tbl_df(read.table("GettingNCleaning/Course Project/train/y_train.txt"))
testLabel <- tbl_df(read.table("GettingNCleaning/Course Project/test/y_test.txt"))
dataLabels <- rbind(testLabel,trainLabel)

for(i in 1:length(dataLabels$V1)){
  dataLabels$V1[i] <- as.character(activityLabels$V2[as.integer(dataLabels$V1[i])])
}

dataOfInterest <- cbind(dataLabels,dataOfInterest)##3

from <- c("\\(","\\)","-","acc","std","bodybody","mag","freq","gyro")
to <- c("","","","acceleration","standarddeviation","body","magnitude","frequency","gyroscope")


columnsOfInterest$V2 <- tolower(sapply(columnsOfInterest$V2, nicenames,from,to))

columnsOfInterest <- rbind(data.frame(V1 = 562, V2= "activity"),columnsOfInterest)  ##4 Create a regular expression to adjust names
colnames(dataOfInterest) <- columnsOfInterest$V2

testSubjects <- tbl_df(read.table("GettingNCleaning/Course Project/test/subject_test.txt"))
trainSubjects <- tbl_df(read.table("GettingNCleaning/Course Project/train//subject_train.txt"))
subjectsSet <- rbind(testSubjects,trainSubjects)
colnames(subjectsSet) <- "subject"
dataOfInterest <- cbind(subjectsSet,dataOfInterest)


subjectActivityMeanSet<- dataOfInterest %>% group_by(subject,activity) %>% summarise_each(funs(mean))

write.table(subjectActivityMeanSet,"tidydata.txt", row.names = FALSE)