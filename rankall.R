getrankhospital <- function(outcomeofcaremeasures, state, outcome) {
  prefix <- "Hospital 30-Day Death \\(Mortality\\) Rates from"
  colName <- paste(prefix, outcome, sep = " ")
  columnIndex <- grep(paste("^", colName, "$", sep = ""), colnames(outcomeofcaremeasures), ignore.case = TRUE)
  wantedData <- outcomeofcaremeasures[outcomeofcaremeasures[, "State"] == state & outcomeofcaremeasures[, columnIndex] != "Not Available", c(2, columnIndex)]
  wantedData[, 2] <- sapply(wantedData[, 2], as.numeric)
  
  wantedData <- wantedData[order(wantedData[, 2], wantedData[, 1]),]
  wantedData$Rank <- 1:nrow(wantedData)
  
  wantedData
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeofcaremeasures <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",
                                    colClasses = "character", header = TRUE, check.names = FALSE)
  ## Check that outcome are valid
  if (length(grep(paste("from", outcome, sep=" "), colnames(outcomeofcaremeasures), ignore.case = TRUE)) == 0) {
    stop("invalid outcome")
  }
  ## For each state, find the hospital of the given rank
  orderedStates <- sort(unique(outcomeofcaremeasures$State))
  
  result <- data.frame(hospital = character(), state = character(), stringsAsFactors=FALSE)
  for (state in orderedStates) {
    wantedData <- getrankhospital(outcomeofcaremeasures, state, outcome)
    hospitalName <- ""
    
    if (num == "best") {
      hospitalName <- wantedData[1, 1]
    } else if (num == "worst") {
      hospitalName <- wantedData[nrow(wantedData), 1]
    } else if (is.numeric(num) && num == as.integer(num)) {
      if (num <= nrow(wantedData)) {
        hospitalName <- wantedData[num, 1]
      } else {
        hospitalName <- NA
      }
    } else {
      stop("invalid num")
    }
    
    df <- data.frame(row.names = state, hospital = hospitalName, state = state, stringsAsFactors=FALSE)
    result <- rbind(result, df)
  }
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result
}