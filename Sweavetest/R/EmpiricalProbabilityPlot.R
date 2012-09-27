EmpiricalProbabilityPlot <- function(GradedTests, QuestionCount){
  
  qs<-seq_len(max(nchar(GradedTests$Correct)))
  StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
  CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)
  QuestionScores <- StudentAnswers == CorrectAnswers
  StudentTotal <- GradedTests$Grade
  
  TestScores <- c(0:ncol(QuestionScores))
  
  ScoreDistribution <- rep(0,ncol(QuestionScores)+1)
  
  par(mfrow=c(1,1))
  
  for(j in QuestionCount){
    
    ScorePattern <- seq(0,ncol(QuestionScores), by=1)
    ProbabilityCorrect <- rep(0, ncol(QuestionScores))
  }
  
  for(i in 1:ncol(QuestionScores)+1){
    ScoreDistribution[i] <- length(which(StudentTotal == (i-1)))
  }
    
    
  for(i in 1:ncol(QuestionScores)){
    
    Question <- QuestionScores[,j]
    
    Student0 <- which(StudentTotal == 0)
    StudentQuestion0 <- Question[Student0]
    CorrectCount0 <- sum(StudentQuestion0)
    ProbabilityCorrect0 <- CorrectCount0/ScoreDistribution[1]
    
    Student <- which(StudentTotal == i)
    StudentQuestion <- Question[Student]
    CorrectCount <- sum(StudentQuestion)
    ProbabilityCorrect[i] <- CorrectCount/ScoreDistribution[i+1]
  }
  
  ProbabilityCorrectTotal <- c(ProbabilityCorrect0, ProbabilityCorrect)
  
  drop <- which(!is.finite(ProbabilityCorrectTotal))
  
  ScorePattern <- ScorePattern[-drop]
  ProbabilityCorrectTotal <- ProbabilityCorrectTotal[-drop]
  
  KernelDensityEstimate <- ksmooth(ScorePattern,ProbabilityCorrectTotal,kernel="normal", bandwidth=2)
  
  Title <- paste("Question",j)  
  plot(ScorePattern,ProbabilityCorrectTotal, xlab="Test Score", ylab = "Pr(Correct)",main=Title)
  lines(KernelDensityEstimate) 

}
