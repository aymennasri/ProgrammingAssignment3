# The function responsible of ranking hospitals in all states
rankall <- function(outcome, num = "best"){
  
  outcome <- tolower(outcome)
  outcome_col <- switch(outcome,
                        "heart attack" = "heart_attack",
                        "heart failure" = "heart_failure",
                        "pneumonia" = "pneumonia",
                        stop("invalid outcome"))
  
  state_df <- hospitals |>
    arrange(!!sym(outcome_col), hospital_name) |> 
    group_by(State)
  
  if(num == "best"){
    state_df |> 
      slice_min(!!sym(outcome_col), with_ties = FALSE, na_rm = T) |> 
      select(hospital = hospital_name, state = State)
  }
  
  else if(num == "worst"){
    state_df |> 
      slice_max(!!sym(outcome_col), with_ties = FALSE, na_rm = T) |> 
      select(hospital = hospital_name, state = State)
  }
  
  else if(is.numeric(num)) {
    if(num > 0) {
      state_df |> 
        slice_min(!!sym(outcome_col), n = num, with_ties = FALSE, na_rm = T) |> 
        slice(num) |> 
        select(hospital = hospital_name, state = State) |> 
        right_join(data.frame(state = unique(hospitals$State))) |> 
        arrange(state)
    }
    else{
      stop("Invalid num")
    }
  }
}

tail(rankall("heart failure"), 10)
tail(rankall("pneumonia", "worst"), 3)
head(rankall("heart attack", 20), 10)
