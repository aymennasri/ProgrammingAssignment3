# The function responsible for ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best"){
  result_state <- hospitals_df %>%
    filter(State==state)
  if(num=="best"){
    return(best(state,outcome))
  }
  if(num=="worst"){
    if(!(state %in% hospitals_df[,"State"])){
      print("invalid state")
      stop(rankhospital)
    }
    if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
      print("invalid outcome")
      stop(rankhospital)
    }
    if(outcome=="heart attack"){
      result <- result_state[which.max(result_state$heart_attack),]
      return(result[,"hospital_name"])
    }
    if(outcome=="heart failure"){
      result <- result_state[which.max(result_state$heart_failure),]
      return(result[,"hospital_name"])
    }
    if(outcome=="pneumonia"){
      result <- result_state[which.max(result_state$pneumonia),]
      return(result[,"hospital_name"])
    }
  }
  if(is.numeric(num)){
    if(outcome=="heart attack"){
      result_state <- result_state %>%
        arrange(heart_attack)
      return(result_state[num,"hospital_name"])
    }
    if(outcome=="heart failure"){
      result_state <- result_state %>%
        arrange(heart_failure)
      return(result_state[num,"hospital_name"])
    }
    if(outcome=="pneumonia"){
      result_state <- result_state %>%
        arrange(pneumonia)
      return(result_state[num,"hospital_name"])
    }
  }
}

rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX","pneumonia",10)
rankhospital("NY", "heart attack", 7)
rankhospital("MN", "heart attack", 5000)