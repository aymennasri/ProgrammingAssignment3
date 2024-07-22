# The function responsible for ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
  
  if(!(state %in% hospitals$State)) {
    stop("invalid state")
  }
  
  outcome <- tolower(outcome)
  outcome_col <- switch(outcome,
                        "heart attack" = "heart_attack",
                        "heart failure" = "heart_failure",
                        "pneumonia" = "pneumonia",
                        stop("invalid outcome"))
  
  state_df <- hospitals %>%
    filter(State == state) %>%
    arrange(!!sym(outcome_col), hospital_name)
  
  if(num == "best") {
    state_df |>
      slice_min(!!sym(outcome_col), with_ties = FALSE, na_rm = T) |> 
      pull(hospital_name)
  }
  
  else if(num == "worst") {
    state_df |>
      slice_max(!!sym(outcome_col), with_ties = FALSE, na_rm = T) |> 
      pull(hospital_name)
  }
  
  # Handle numeric ranking
  else if(is.numeric(num)) {
    if(num > 0 && num <= nrow(state_df)) {
      return(state_df$hospital_name[num])
    }
    else {
      return(NA)
    }
  }
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)
rankhospital("TX", "pneumonia", 10)