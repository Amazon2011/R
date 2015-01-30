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

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeofcaremeasures <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",
                                    colClasses = "character", header = TRUE, check.names = FALSE)
  ## Check that state and outcome are valid
  if (length(grep(state, outcomeofcaremeasures$State, ignore.case = TRUE)) == 0 ) {
    stop("invalid state")
  }
  if (length(grep(paste("from", outcome, sep=" "), colnames(outcomeofcaremeasures), ignore.case = TRUE)) == 0) {
    stop("invalid outcome")
  }
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  wantedData <- getrankhospital(outcomeofcaremeasures, state, outcome)
  
  if (num == "best") {
    wantedData[1, 1]
  } else if (num == "worst") {
    wantedData[nrow(wantedData), 1]
  } else if (is.numeric(num) && num == as.integer(num)) {
    if (num <= nrow(wantedData)) {
      wantedData[num, 1]
    } else {
      NA
    }
  } else {
    stop("invalid num")
  }
  
}