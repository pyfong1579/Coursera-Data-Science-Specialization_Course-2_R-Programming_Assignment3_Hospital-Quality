rankhospital <- function(state, outcome, num= "best"){

  ##Read data
  data <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  
  ##Check that State and outcome are valid
  if (!state %in% unique(data$State)) stop("invalid state")
  if (!outcome %in% c("heart attack","heart failure", "pneumonia")) stop("invalid outcome")
  
  #Select variables of interest'
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  data <- data[data$State == state, c(2,7,outcomes[[outcome]])]
  
  # rename columns
  colnames(data)<- c("hospital", "state", "outcome")
  
  # remove NA
  data <- data[complete.cases(data),]
  
  #re order by outcome 
  state_rank <- order(data$outcome, data$hospital)
  data <- data[state_rank,]
  
  data$hospital
  
  if(num == "best"){
    return(data$hospital[1])
  } else if(num == "worst"){
    return(data$hospital[length(data$hospital)])
  } else if(num > length(data$hospital) | num < 0){
    return(NA)
  } else {
    
    ##Return hospital name in that state with the given rank 30-day death rate
      return(data$hospital[num])
  }
  
}
