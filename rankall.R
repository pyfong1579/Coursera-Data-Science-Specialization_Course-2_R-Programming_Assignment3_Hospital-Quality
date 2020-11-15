rankall <- function(outcome, num= "best"){
  
  ##Read data
  data <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available",stringsAsFactors=FALSE)
  
  ##Check that outcome is valid
  if (!outcome %in% c("heart attack","heart failure", "pneumonia")) stop("invalid outcome")
  
  #Select & extract variables to be filtered
  outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
  
  data <- data[, c(2,7,outcomes[[outcome]])]
  
  # rename columns
  colnames(data)<- c("hospital", "state", "outcome")
  
  # remove NA
  data <- data[complete.cases(data),]
  
  # rank data by state, outcome and hospital to handle ties
  ranking <- order(data$state, data$outcome, data$hospital)
  data <- data[ranking,]
 
  # split data by state
  data <- split(data, data$state)
  
  # Defining result logic 
  fun <- function(x, num){
    if(num == "best") return(x$hospital[1])
    if(num =="worst") return(x$hospital[nrow(x)])
    else return(x$hospital[num])
  }
  data <- sapply(data, fun, num = num)
  
  ## Return a data frame with the hospital names and the abbreviated state name
  data <- data.frame("hospital" = data, "state" = names(data))
}