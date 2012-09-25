DistractorDiscrimination <- function(
   GradedTests = getglobal(GradedTests, NULL),
   Index = getglobal(Index, NULL)){
  
  Quantile75 <- quantile(GradedTests$Grade, .75)
  Quantile25 <- quantile(GradedTests$Grade, .25)
  Bottom25 <- which(GradedTests$Grade <= Quantile25)
  Top25 <- which(GradedTests$Grade >= Quantile75)
  NumBottom25 <- length(Bottom25)
  NumTop25 <- length(Top25)
  
  Qlength <- 5
  if (is.na(Index[QuestionCounter(), "E"]))
    Qlength <- 4
  columns <- paste0("A", 1:Qlength)
  
  topSummary <- CreateIndex(Index, GradedTests[Top25,])
  topSummary <- apply(topSummary[topSummary$Question == QuestionCounter(),columns], 2, sum)
  
  botSummary <- CreateIndex(Index, GradedTests[Bottom25,])
  botSummary <- apply(botSummary[botSummary$Question == QuestionCounter(),columns], 2, sum)
  
  Top25Percentage <- topSummary/NumTop25
  Bottom25Percentage <- botSummary/NumBottom25
    
  result <- data.frame(Option = 1:Qlength,
                       Discrimination = as.numeric(Top25Percentage - Bottom25Percentage))
  
  result
}