best <- function(state, outcome){

  ##Read data'
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
  
  #select best hospitals with lowest outcome
  best <- min(data$outcome)
  data <- data[data$outcome == best,]
  
  #sort alphabetical hospital name if tie
  sort(data$hospital)[1]

  ## Return hospital name in that state with lowest 30-day death rate

}