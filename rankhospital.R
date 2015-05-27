rankhospital <- function(state, outcome, num = "best") { 
  ## Read the outcome of hospital data 
  hospital_data <- read.csv("outcome-of-care-measures.csv", colClasses="character") 
  
  ## Filter the state 
  hospital_data <- hospital_data[hospital_data$State == state, ] 
  
  ## Check the invalid state 
  if(nrow(hospital_data) == 0) { 
    stop("invalid state") 
  } 
  
  ## Get the column number 
  col_num <- if(outcome == "heart attack") { 
    11 
  } else if(outcome == "heart failure") { 
    17 
  } else if(outcome == "pneumonia") { 
    23 
  } else { 
    stop("invalid outcome")         
  } 
  
  ## Convert the character to numeric 
  hospital_data[, col_num] <- suppressWarnings(as.numeric(hospital_data[, col_num])) 
  
  ## Remove NA's 
  hospital_data <- hospital_data[complete.cases(hospital_data[,col_num]),] 
  
  ## Sort the hospital data by rate and then by name 
  hospital_data <- hospital_data[order(hospital_data[col_num], hospital_data[2]), ] 
  
  ## Return the desired results
  if(num == "best"){ 
    hospital_data[1, 2] 
  } else if(num == "worst"){ 
    tail(hospital_data, 1)[1, 2] 
  } else { 
    hospital_data[num, 2] 
  } 
} 