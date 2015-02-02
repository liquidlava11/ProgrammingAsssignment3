rankhospital <- function(state, outcome, num) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  # check state
  if (state %in% state.abb) {
    
  } else {
    stop("invalid state")
  }
  
  l <- c("heart attack", "heart failure", "pneumonia")
  
  if (outcome %in% l) {
    n = 0
  } else {
    stop("invalid outcome")
  }
  
  if (outcome == "heart attack") {
    n = 11
  }
  if (outcome == "heart failure") {
    n = 17
  }
  if (outcome == "pneumonia") {
    n = 23 
  }
  
  #subsetting data 
  data1 <- subset(data, data["State"]==state)
  data1 <- na.omit(data1)
  
  data1[, n] <- suppressWarnings(as.numeric(data1[, n]))
  # sorting data
  data_new <- data1[order(data1[,2]),]
  data_new <- na.omit(data_new)
  data_new <- data_new[order(data_new[,n], data_new[,2]), ]
  
  # get first in column
  if (grepl(num,"worst")) {
    hosp <- data_new[nrow(data_new),2]
    print(hosp)
  } else {
    hosp <- data_new[num,2]
    print(hosp)
  }
  
}
