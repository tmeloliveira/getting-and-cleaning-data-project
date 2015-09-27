runAnalysis <- function() {
  # Reading the labels
  activityLabels = read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  
  # Reading test data
  testingData = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
  testingData[,562] = read.csv("UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
  testingData[,563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  
  # Reading train data
  trainingData = read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
  trainingData[,562] = read.csv("UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
  trainingData[,563] = read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
  
  # Read features data
  features = read.csv("UCI HAR Dataset/features.txt", sep="", header=FALSE)
  
  # Change the name of some features
  features[,2] = gsub('-mean', 'Mean', features[,2])
  features[,2] = gsub('-std', 'Std', features[,2])
  features[,2] = gsub('[-()]', '', features[,2])
  
  # Merge training and testing data
  dataset = rbind(trainingData, testingData)
  
  # Get mean and std data
  cols <- grep(".*Mean.*|.*Std.*", features[,2])
  
  # Reduce features table
  features <- features[cols,]
  
  # Add two last columns
  cols <- c(cols, 562, 563)
  
  # Remove others columns
  dataset <- dataset[,cols]
  
  # Add the column names
  colnames(dataset) <- c(features$V2, "Activity", "Subject")
  colnames(dataset) <- tolower(colnames(dataset))
  
  currentActivity = 1
  for (currentActivityLabel in activityLabels$V2) {
    dataset$activity <- gsub(currentActivity, currentActivityLabel, dataset$activity)
    currentActivity <- currentActivity + 1
  }
  
  dataset$activity <- as.factor(dataset$activity)
  dataset$subject <- as.factor(dataset$subject)
  
  # Put tidy data together
  tidy = aggregate(dataset, by=list(activity = dataset$activity, subject=dataset$subject), mean)
  
  # Remove unused columns 
  tidy[,90] = NULL
  tidy[,89] = NULL
  
  # Write output file
  write.table(tidy, "tidy.txt", sep="\t", row.name=FALSE)

}