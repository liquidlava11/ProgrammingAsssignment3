rankall <- function(outcome, num) {
  data <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
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
  
  data[, n] <- suppressWarnings(as.numeric(data[, n]))
  # sorting data
  data_new <- data[order(data[,2]),]
  data_new <- na.omit(data_new)
  data_new <- data_new[order(data_new[,n], data_new[,2]), ]
  
  new <- data.frame()
  
  stat <- state.abb
  for (each in stat) {
    st <- subset(data_new, data_new[,7]==each)
    if (grepl(num,"worst")) {
      hosp <- st[nrow(st),2]
    } else {
      hosp <- st[num,2]
      
    }
    result <- cbind(hosp, each)
    new <- rbind(new, result)
  }
  names(new) <- c('hospital', 'state')
  return(new)
}
