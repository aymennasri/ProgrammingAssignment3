library(dplyr)

# Loading the dataset
data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

# Extracting the useful columns only
hospitals <- data |>
  mutate(
    heart_attack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
    heart_failure = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
    pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
    State,
    hospital_name = Hospital.Name,
    .keep = "none"
  )

# The rows are of type string so we have to convert them back to numeric
outcomes <- c("heart_attack", "heart_failure", "pneumonia")

hospitals[outcomes] <- lapply(hospitals[outcomes], function(x) {
  as.numeric(ifelse(x == "Not Available", NA, x))
})
  

# The function responsible for finding the best hospital in a state
best <- function(state, outcome) {
  if (!(state %in% hospitals$State)) {
    stop("invalid state")
  }
  
  outcome <- tolower(outcome)
  outcome_col <- switch(outcome,
                        "heart attack" = "heart_attack",
                        "heart failure" = "heart_failure",
                        "pneumonia" = "pneumonia",
                        stop("invalid outcome"))
  
  hospitals |>
    arrange(hospital_name) |>
    filter(State == state) |>
    filter(!is.na(!!sym(outcome_col))) |>
    slice_min(!!sym(outcome_col), with_ties = FALSE) |> 
    pull(hospital_name)
  
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")