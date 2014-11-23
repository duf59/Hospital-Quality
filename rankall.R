rankall <- function(outcome, num = "best") {
  # Find hospital with specified ranking for 30-day mortality for given
  # outcome and all states
  #
  # Args: 
  #   - outcome: character, type of disease, either "heart attack",
  #              "heart failure", or "pneumonia"
  #   - num: numeric, ranking
  #
  # Returns:
  #   - 2-column data frame containing the hospital in each state that has the
  #   ranking specied in num
  
  # Read outcome data
  data <- read.csv("data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Check arguments
  outcome.list  <- c("heart attack","heart failure","pneumonia") # possible outcomes
  
  if (!any(outcome==outcome.list))  stop("invalid outcome")
  if (!is.numeric(num) & !(num=="best") & !(num=="worst")) stop("invalid ranking")
  
  # get variable corresponding to the mortality rate for specified outcome
  pattern <- paste("^Hospital.30.Day.Death..Mortality..Rates(.*)",
                   sub(" ",".",outcome),
                   sep="")
  mortality.rate <- grep(pattern,names(data),ignore.case = TRUE, value = TRUE)
  
  # subset data for specified outcome, exclude hospitals that do not have data
  data[,mortality.rate] <- as.numeric(data[,mortality.rate])
  outcome.data <- data[!is.na(data[,mortality.rate]),
                       c("Hospital.Name","State",mortality.rate)]
  names(outcome.data) <- c("hospital","state","mortality.rate")  
  
  
  # CONTINUE HERE - COMPUTE RANKING BY STATE CODE...
  
  # Get state codes
  state.list    <- unique(data[,"State"])
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
