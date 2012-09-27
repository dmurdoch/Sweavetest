Warnings <- function(DR,ID,PB){
  
  if(DR <= .2 || ID <= 0 || PB <= .2){
  cat("{\\bf Warning:}")
  cat("\\\\")
  }
  
  if(DR <= .2 && ID > 0 && PB > .2){
    cat("\\\\")
    cat(sprintf("The calculated difficulty rating of %.2f is below 0.2. 
    Please see the appendix for suggestions on how to improve this question.",
        DR))
  }
  
  if(ID <= 0 && DR > .2 && PB > .2){
    cat("\\\\")
    cat(sprintf("The calculated item discrimination index of %.2f is negative. 
    Please see the appendix for suggestions on how to improve this question.",
        ID))
  }
  
  if(PB <= 0.2 && DR > .2 && ID > 0){
    cat("\\\\")
    cat(sprintf("The calculated point biserial index of %.2f is below 0.2. 
    Please see the appendix for suggestions on how to improve this question.", 
        PB))
  }
  
  if(DR <= .2 && ID <= 0 && PB > .2){
    cat("\\\\")
    cat(sprintf("Both the calculated difficulty %.2f and item discrimination %.2f 
    are below acceptable standards. 
    Please see the appendix for suggestions on how to improve this question.",
        DR, ID))
  }
  
  if(DR <= .2 && ID > 0 && PB <= .2){
    cat("\\\\")
    cat(sprintf("Both the calculated difficulty %.2f and point biserial statistic %.2f 
    are below acceptable standards. 
    Please see the appendix for suggestionc on how to improve this question.",
        DR, PB))
  }
  
  if(DR > .2 && ID <= 0 && PB <= .2){
    cat("\\\\")
    cat(sprintf("Both the calculated item discrimination %.2f and point biserial statistic %.2f 
    are below accpetable standards. 
    Please see the appendix for suggestions on how to improve this question.",
        ID, PB))
  }
  
  if(DR <= .2 && ID <= 0 && PB <= .2){
    cat("\\\\")
    cat(sprintf("All three calculated statistics (DR=%.2f, ID=%.2f, PB=%.2f) 
    are below acceptable standards. 
    Please see the appendix for suggestions on how to improve this question.",
        DR, ID, PB))
  }
  
  if(DR <= .2 || ID <= 0 || PB <= .2){
    cat("\\\\")
    cat("\\\\")
    cat("\\\\")
  }
  
}