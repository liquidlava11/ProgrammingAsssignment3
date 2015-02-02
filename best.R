best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # check state
  if (state %in% state.abb) {
    
  } else {
    print("Error state")
  }
  
  l <- c("heart attack", "heart failure", "pneumonia")
  
  if (outcome %in% l) {
    n = 0
  } else {
    print("Error disease")
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
  
  data1[, n] <- as.numeric(data1[, n])
  # sorting data
  data_new <- data1[order(data1[,2]),]
  data_new <- data_new[order(data_new[,n], data_new[,2]), ]
  
  # get first in column
  hosp <- data_new[1,2]
  
  # print it
  print(hosp)
}
