rankall <- function(outcome, num = "best") {
  
  ## Read the outcome of data
  hospital_data <- read.csv("outcome-of-care-measures.csv", sep = ",")
  
  ## Check that the states and outcome are valid
  valid_states <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY")
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!is.element(outcome, valid_outcomes)) stop("invalid outcome")
  
  header_name <- NULL
  if (outcome == "heart attack") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  else if (outcome == "heart failure") header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  else header_name <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  hospital <- c()
  states <- c()
  
  ## Finds the hospital ranking for each given state.
  for (state in valid_states) {
    hospital_ranking <- c()
    data <- hospital_data[hospital_data$State == state,]        
    sorted_data <- data[order(as.numeric(as.character(data[,header_name])), as.character(data[,"Hospital.Name"])),]
    sorted_data <- sorted_data[!sorted_data[,header_name] == "Not Available",]
    if (num == "best") {
      hospital_ranking <- best(state, outcome)
    } else if (num == "worst") {
      hospital_ranking <- as.character(tail(sorted_data[,"Hospital.Name"], n = 1))
    } else {
      hospital_ranking <- as.character(sorted_data[,"Hospital.Name"][num])
    }
    hospital <- c(hospital, hospital_ranking)
  }
  result <- data.frame(hospital, valid_states)
  colnames(result) <- c("hospital", "state")
  return(result)
}