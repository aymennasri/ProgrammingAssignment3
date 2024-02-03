# Loading tidyverse library to clean the dataset
library(tidyverse)

# Loading the dataset
outcome <- read.csv("outcome-of-care-measures.csv",
                    colClasses = "character")

# Extracting the useful columns only
hospitals <- outcome %>% 
  mutate(heart_attack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
         heart_failure = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
         pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
         State, hospital_name = Hospital.Name, .keep = "none")

# Inspecting the dataset for potential badly registered data
is.numeric(hospitals$heart_attack)
is.numeric(hospitals$heart_failure)
is.numeric(hospitals$pneumonia)

# The rows aren't all numeric, we have to inspect them
sample(hospitals$heart_failure, 20, replace = T)
sample(hospitals$pneumonia, 20, replace = T)

# The rows are of type string so we have to convert them back to numeric
# ATTENTION : Using lapply() can complicate things here, converting the rows into
# lists instead of simple numeric.
hospitals$heart_attack <- sapply(hospitals$heart_attack, as.numeric)
hospitals$heart_failure <- sapply(hospitals$heart_failure, as.numeric)
hospitals$pneumonia <- sapply(hospitals$pneumonia, as.numeric)

# Sorting the hospital names to handle ties for the best hospital
hospitals_df <- hospitals %>%
  arrange(hospital_name)

# The function responsible for finding the best hospital in a state
best <- function(state, outcome){
  if(!(state %in% hospitals_df[,"State"])){
    print("invalid state")
    stop(best)
  }
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    print("invalid outcome")
    stop(best)
  }
  result_state <- hospitals_df %>%
    filter(State==state)
  if(outcome=="heart attack"){
    hospitals_df <- na.omit(hospitals_df$heart_attack)
    result <- result_state[which.min(result_state$heart_attack),]
    return(result[,"hospital_name"])
  }
  if(outcome=="heart failure"){
    hospitals_df <- na.omit(hospitals_df$heart_failure)
    result <- result_state[which.min(result_state$heart_failure),]
    return(result[,"hospital_name"])
  }
  if(outcome=="pneumonia"){
    hospitals_df <- na.omit(hospitals_df$pneumonia)
    result <- result_state[which.min(result_state$pneumonia),]
    return(result[,"hospital_name"])
  }
}

# Testing the function

best("SC", "heart attack")
best("NY", "pneumonia")
best("AK","pneumonia")