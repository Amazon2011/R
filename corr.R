corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  completedatanum <- complete(directory)
  datafilenames <- paste(directory, "/", list.files(directory), sep = "")
  satisfactorydatafilenames <- datafilenames[completedatanum[, 2] >= threshold & completedatanum[, 2] > 0]
  
  corVector <- numeric(0)
  for (file in satisfactorydatafilenames) {
    completedata <- na.omit(read.csv(file, header = T))
    corVector <- c(corVector, cor(completedata$sulfate, completedata$nitrate))
  }
  
  corVector
}