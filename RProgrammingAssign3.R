best <- function(state, outcome) {
  ## Read outcome data
  dt <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% levels(factor(dt$State)))){
    stop("invalid state")
  }
  
  if (!(outcome == "heart attack" || outcome == "heart failure" || outcome == "pneumonia")){
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  col <- ifelse(outcome=="heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
  sub <- subset(dt, dt$State==state)
  sub <- subset(sub, sub[,col]!="Not Available")
  return (sub[,2][order(sub[,col], decreasing = T)][1])
}