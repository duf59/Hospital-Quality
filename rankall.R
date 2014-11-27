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
  outcome.var <- grep(pattern,names(data),ignore.case = TRUE, value = TRUE)
  
  # subset data for specified outcome
  data[,outcome.var] <- as.numeric(data[,outcome.var])
  
  # exclude hospitals that do not have data, keep only variables of interest
  outcome.data <- data[!is.na(data[,outcome.var]),
                       c("Hospital.Name","State",outcome.var)]
  # Rename variables
  names(outcome.data) <- c("hospital","state","mortality.rate")  
  
  # Order by State, then mortality rate, then hospital name
  outcome.data <- outcome.data[order(outcome.data$state,
                                     outcome.data$mortality.rate,
                                     outcome.data$hospital),]
  
  # Set the rank to 1 if  num = "best"
  if (num=="best") {
    num <- 1
  }
  
  # get the hospital with specified ranking for each state

  final <- sapply(split(outcome.data,outcome.data$state,drop = FALSE),
                  function(x) { if (num=="worst") c(x$hospital[nrow(x)], x$state[1])
                                else c(x$hospital[num], x$state[1])
                              })
  final <- as.data.frame(t(final), stringsAsFactors = FALSE)
  names(final) <- c("hospital","state")
  
  final
  
}
