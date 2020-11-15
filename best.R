best <- function(state, outcome){

  ##Read data'
  data <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  
  ##Check that State and outcome are valid
  if (!state %in% unique(data$State)) stop("invalid state")
  if (!outcome %in% c("heart attack","heart failure", "pneumonia")) stop("invalid outcome")
  
  #Select variables of interest'
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  sel_data <- data[data$State == state, c(2,7,outcomes[[outcome]])]
  
  # rename columns
  colnames(sel_data)<- c("hospital", "state", "outcome")
  
  # remove NA
  sel_data <- sel_data[complete.cases(sel_data),]
  
  #select best hospitals with lowest outcome
  best <- min(sel_data$outcome)
  sel_data <- sel_data[sel_data$outcome == best,]
  
  #sort alphabetical hospital name if tie
  sort(sel_data$hospital)[1]

  ## Return hospital name in that state with lowest 30-day death rate

}