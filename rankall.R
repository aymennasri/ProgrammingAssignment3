# The function responsible of ranking hospitals in all states
rankall <- function(outcome, num = "best"){
  if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
    print("invalid outcome")
    stop(rankall)
  }
  if(num=="best"){
    if(outcome=="heart attack"){
      result <- hospitals_df %>%
        group_by(State)%>%
        mutate(heart_attack,
               .keep = "none")%>%
        summarize(rate=min(heart_attack, na.rm = T))
    }
  }
  return(result)
}
r <- rankall("heart attack")
r
as.character(subset(r, State=="HI"))
