DistractorDiscrimination <- function(
   GradedTests = getglobal(GradedTests, NULL),
   QuestionCounter = getglobal(QuestionCounter, 0),
   Index = getglobal(Index, NULL)){
  
  Quantile75 <- quantile(GradedTests$Grade, .75)
  Quantile25 <- quantile(GradedTests$Grade, .25)
  Bottom25 <- which(GradedTests$Grade <= Quantile25)
  Top25 <- which(GradedTests$Grade >= Quantile75)
  NumBottom25 <- length(Bottom25)
  NumTop25 <- length(Top25)
  
  Qlength <- 5
  if (is.na(Index[QuestionCounter, "E"]))
    Qlength <- 4
  columns <- paste0("A", 1:Qlength)
  
  topSummary <- CreateIndex(Index, GradedTests[Top25,])
  topSummary <- topSummary[topSummary$Question == QuestionCounter, columns]
    
  botSummary <- CreateIndex(Index, GradedTests[Bottom25,])
  botSummary <- botSummary[botSummary$Question == QuestionCounter, columns]
    
  Top25Percentage <- topSummary/NumTop25
  Bottom25Percentage <- botSummary/NumBottom25
    
  Distractor <- Top25Percentage-Bottom25Percentage
  return(Distractor)
}