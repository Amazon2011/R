pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  datafilenames <- paste(directory, "/", list.files(directory), sep = "")
  
  merge.data <-data.frame()
  for(i in id) {
    newdata <- read.csv(datafilenames[i], header = T)
    merge.data <- rbind(merge.data,newdata)
  }
  
  round(mean(merge.data[, pollutant], na.rm = TRUE), digits = 3)
}