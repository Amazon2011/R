best <- function(state, outcome) {
  ## Read outcome data
  outcomeofcaremeasures <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  
  if (!(state %in% outcomeofcaremeasures$State)) {
    stop("invalid state")
  }
  if (!(outcome %in% names(outcomeofcaremeasures))) {
    stop("invalid outcome")
  }
  
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}