## This is a file for Project Assignment of the course
## Getting and Cleaning Data started in June, 2014.
## Usage
## =====
## -- getnclean() - gets all the data and generates one dataset with train and test data
## -- getavgdata(alldf) - creates second dataset with average for each ariable for each activity and each subject
## -- savecleandata() - cleans and saves all the data to csv files

## The function reads a file of 561-feature vectors
## The vectors are written as strings of the file
## Parameters:
## -- filename - the name of the file
## -- cols - column numbers of interest (we are not going to extract all features)
## Returns:
## -- list of list of values
readfeature561 <- function(filename, cols=c()) {
  ## Helper function to process one line of the file
  linex <- function(x) {
    res <- c()
    for (i in unlist(strsplit(x, " "))) {
      if (i != "") res <- c(res, as.numeric(i))
    }
    return(if (length(cols)==0) res else res[cols])
  }
  features <- lapply(readLines(filename), linex)
  features
}

## The function reads a file of 1-feature vectors
## The vectors are written as strings of the file
## Parameters:
## -- filename - the name of the file
## -- cols - column numbers of interest (we are not going to extract all features)
## Returns:
## -- one vector of values
readfeature1 <- function(filename) {
  test <- unlist(lapply(readLines(filename), as.numeric))
  test
}

## The function that reads and cleans on dataset - train or test.
## Params:
## -- type - "train"/"test" character string.
## -- colnames - column names vector
## -- actnames - activity names vector
## Returns:
## -- properly organized data frame which contains the data
getncleanportion <- function(type="test", colnames=c(), actnames=c()) {
  ## Reading measurements from columns defined in colnames
  dx <- readfeature561(filename=gsub("%", type, "%/X_%.txt"), cols=which(!is.na(colnames)))
  df <- data.frame(matrix(unlist(dx), nrow=length(dx), byrow=T))
  names(df) <- colnames[!is.na(colnames)]  ## Adding meaningful names to data frame
  ## Reading activity data
  ad <- factor(readfeature1(filename=gsub("%", type, "%/y_%.txt")), labels=actnames)
  df$Activity <- ad  ## Adding activity factor
  ## Reading subject data
  subj <- readfeature1(filename=gsub("%", type, "%/subject_%.txt"))
  df$Subject <- subj
  return(df)
}

## The main function which cleans the data
getnclean <- function() {
  ## Read features.txt file to get features which deals with means or standard deviations
  ## The columns vector contains the names of parameters
  columns <- unlist(lapply(readLines("features.txt"), 
                           function(x) return( if (!identical(grep("mean|std", x), integer(0))) unlist(strsplit(x, " ", fixed=TRUE))[2] else NA)))
  ## Reading activity labels
  actlab <- lapply(readLines("activity_labels.txt"), function(x) unlist(strsplit(x, " ", fixed=TRUE))[2])
  ## Reading test data set
  testdf <- getncleanportion(type="test", colnames=columns, actnames=actlab)
  ## Reading train data set
  traindf <- getncleanportion(type="train", colnames=columns, actnames=actlab)
  
  return(rbind(testdf, traindf))
}

## The function which returns average data frame
## Params:
## -- alldf - data frame which contains clean data
getavgdata <- function(alldf) {
  return(aggregate(.~Subject+Activity , data=alldf, mean))
}

## The function which generates and saves all the data
savecleandata <- function() {
  alldf <- getnclean()
  write.table(alldf, file="activity_data.csv", row.names=FALSE)
  avgdf <- getavgdata(alldf)
  write.table(avgdf, file="activity_data_avg.csv", row.names=FALSE)  
}
