Warnings <- function(DR,ID,PB){
  
  if(DR <= .2 | ID <= 0 | PB <= .2){
  cat("{\\bf Warning:}")
  cat("\\\\")
  }
  
  if(DR <= .2 & ID > 0 & PB > .2){
    cat("\\\\")
    cat("The calculated difficulty rating is below acceptable levels. Please see the appendix for suggestions on how to improve this question.")
  }
  
  if(ID <= 0 & DR > .2 & PB > .2){
    cat("\\\\")
    cat("The calculated item discrimination index is below acceptable standards. Please see the appendix for suggestions on how to improve this question.")
  }
  
  if(PB <= 0.2 & DR > .2 & ID > 0){
    cat("\\\\")
    cat("The calculated point biserial index is below acceptable standards. Please see the appendix for suggestions on how to improve this question.")
  }
  
  if(DR <= .2 & ID <= 0 & PB > .2){
    cat("\\\\")
    cat("Both the calculated difficulty and item discrimination are below acceptable standards. Please see the appendix for suggestions on how to improve this question.")
  }
  
  if(DR <= .2 & ID > 0 & PB <= .2){
    cat("\\\\")
    cat("Both the calculated difficulty and point biserial statistic are below acceptable standards. Please see the appendix for suggestionc on how to improve this question.")
  }
  
  if(DR > .2 & ID <= 0 & PB <= .2){
    cat("\\\\")
    cat("Both the calculated item discrimination and point biserial statistics are below accpetable standards. Please see the appendix for suggestions on how to improve this question.")
  }
  
  if(DR <= .2 & ID <= 0 & PB <= .2){
    cat("\\\\")
    cat("All three calculated statistics are below acceptable standards. Please see the appendix for suggestions on how to improve this question.")
  }
  
  if(DR <= .2 | ID <= 0 | PB <= .2){
    cat("\\\\")
    cat("\\\\")
    cat("\\\\")
  }
  
}