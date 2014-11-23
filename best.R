best <- function(state, outcome) {
  # Find hospital with the best (i.e. lowest) 30-day mortality for the specied
  # outcome in the specified state
  #
  # Args: 
  #   - state: character, 2 letter State code
  #   - outcome: character, type of disease, either "heart attack",
  #              "heart failure", or "pneumonia"
  #
  # Returns:
  #   - a character vector with the name of the hospital that has the best
  #     (i.e. lowest) 30-day mortality for the specied outcome and state

  # Read outcome data
  
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Check that arguments state and outcome are valid
  diseases   <- c("heart attack","heart failure","pneumonia")
  state.list <- unique(data[,"State"])
  
  if (!any(outcome==diseases)) stop("invalid outcome")
  if (!any(state==state.list)) stop("invalid state")
  
  # Return hospital name in that state with lowest 30-day death
  
    # get the variable corresponding to the 30-day death for specified outcome
    pattern <- paste("^Hospital.30.Day.Death..Mortality..Rates(.*)",
                    sub(" ",".",outcome),
                    sep="")
    mortality.rate.variable <- grep(pattern,names(data),ignore.case = TRUE)
  
    # subset data to specified outcome and state, excluding hospitals that do
    # not have data on the particular outcome 
    outcome.data <- data[,c(2,7,mortality.rate.variable)]
    outcome.data[,3] <- as.numeric(outcome.data[,3])
    outcome.data <- outcome.data[outcome.data$State==state & !is.na(outcome.data[,3]),]
  
    # rate: sort by mortality rate, then by name, pick the first
  
    outcome.data <- outcome.data[order(outcome.data[,3],outcome.data[,1]),]
    outcome.data[1,1]
}
