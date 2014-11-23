rankhospital <- function(state, outcome, num = "best") {
  # Find hospital with specified ranking for 30-day mortality for given
  # outcome and state
  #
  # Args: 
  #   - state: character, 2 letter State code
  #   - outcome: character, type of disease, either "heart attack",
  #              "heart failure", or "pneumonia"
  #   - num: numeric, ranking
  #
  # Returns:
  #   - a character vector with the name of the hospital whose ranking is "num"
  #     for 30-day mortality rate for the specied outcome and state
  
  # Read outcome data
  
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Check arguments
  outcome.list  <- c("heart attack","heart failure","pneumonia") # possible outcomes
  state.list    <- unique(data[,"State"])                        # possible state codes
  
  if (!any(outcome==outcome.list))  stop("invalid outcome")
  if (!any(state==state.list))      stop("invalid state")
  if (!is.numeric(num) & !(num=="best") & !(num=="worst")) stop("invalid ranking")
  
  
  # get variable corresponding to the mortality rate for specified outcome
  pattern <- paste("^Hospital.30.Day.Death..Mortality..Rates(.*)",
                   sub(" ",".",outcome),
                   sep="")
  
  mortality.rate <- grep(pattern,names(data),ignore.case = TRUE, value = TRUE)
  
  # subset data to specified outcome and state, excluding hospitals that do
  # not have data on the particular outcome 

  outcome.data <- data[data$State==state,c("Hospital.Name",mortality.rate)]
  names(outcome.data) <- c("Hospital.Name","Mortality.Rate")

  outcome.data$Mortality.Rate <- as.numeric(outcome.data$Mortality.Rate)
  outcome.data <- outcome.data[!is.na(outcome.data$Mortality.Rate),]
  
  # sort by mortality rate, then by name
  
  outcome.data <- outcome.data[order(outcome.data$Mortality.Rate,
                                     outcome.data$Hospital.Name),]
  
  # select the specified rank
  
  if (is.numeric(num)) {
    if (num<=nrow(outcome.data)) outcome.data$Hospital.Name[num]
    else NA
  }  else if (num=="best") {
    outcome.data$Hospital.Name[1]
  }  else {
    outcome.data$Hospital.Name[nrow(outcome.data)]
  }
  
  
}
