best <- function(state, outcome) {
  ## Read outcome data
  outcomeofcaremeasures <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv",
                                    colClasses = "character", header = TRUE,check.names = FALSE)
  
  ## Check that state and outcome are valid
  
  if (length(grep(state, outcomeofcaremeasures$State, ignore.case = TRUE)) == 0 ) {
    stop("invalid state")
  }
  if (length(grep(paste("from", outcome, sep=" "), colnames(outcomeofcaremeasures), ignore.case = TRUE)) == 0) {
    stop("invalid outcome")
  }
  
  prefix <- "Hospital 30-Day Death \\(Mortality\\) Rates from"
  colName <- paste(prefix, outcome, sep = " ")
  columnIndex <- grep(paste("^", colName, "$", sep = ""), colnames(outcomeofcaremeasures), ignore.case = TRUE)
  wantedData <- outcomeofcaremeasures[outcomeofcaremeasures[, "State"] == state & outcomeofcaremeasures[, columnIndex] != "Not Available", c(2, columnIndex)]
  wantedData[, 2] <- sapply(wantedData[, 2], as.numeric)
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  wantedData[order(wantedData[, 2], wantedData[, 1]),][1, 1]
}