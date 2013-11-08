EmpiricalProbabilityPlot <- function(GradedTests, QuestionCount, LongQuestionGrades, LongQuestionCount, GradeTotal=TotalGrade()){
  
  if(missing(LongQuestionCount)==TRUE){
  qs<-seq_len(max(nchar(GradedTests$Correct)))
  StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
  CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)
  QuestionScores <- StudentAnswers == CorrectAnswers
  StudentTotal <- GradeTotal
  
  LQGr <- LongQuestionGrades[,-1]
  MaxLQGr <- c()
  for(k in 1:ncol(LQGr)){
    NewMax <- max(LQGr[,k])
    MaxLQGr <- c(MaxLQGr,NewMax)
  }
  
  MaxScore <- sum(MaxLQGr)
  
  TestScores <- c(0:(ncol(QuestionScores)+MaxScore))
  
  ScoreDistribution <- rep(0,(ncol(QuestionScores)+MaxScore)+1)
  
  par(mfrow=c(1,1))
  
  for(j in QuestionCount){
    
    ScorePattern <- seq(0,ncol(QuestionScores)+MaxScore, by=1)
    ProbabilityCorrect <- rep(0, ncol(QuestionScores)+MaxScore)
  }
  
  for(i in 1:ncol(QuestionScores)+MaxScore+1){
    ScoreDistribution[i] <- length(which(StudentTotal == (i-1)))
  }
    
    
  for(i in 1:(ncol(QuestionScores)+MaxScore)){
    
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
  
  if(missing(LongQuestionCount)==FALSE){
    qs<-seq_len(max(nchar(GradedTests$Correct)))
    StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
    CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)
    QuestionScores <- StudentAnswers == CorrectAnswers
    StudentTotal <- GradeTotal
    
    LQGr <- LongQuestionGrades[,-1]
    MaxLQGr <- c()
    for(k in 1:ncol(LQGr)){
      NewMax <- max(LQGr[,k])
      MaxLQGr <- c(MaxLQGr,NewMax)
    }
    
    MaxScore <- sum(MaxLQGr)
    
    TestScores <- c(0:(ncol(QuestionScores)+MaxScore))
    
    ScoreDistribution <- rep(0,ncol(QuestionScores)+MaxScore+1)
    
    par(mfrow=c(1,1))
    
    for(j in LongQuestionCount){
      
      ScorePattern <- seq(0,ncol(QuestionScores)+MaxScore, by=1)
      ProbabilityCorrect <- rep(0, ncol(QuestionScores)+MaxScore)
    }
    
    for(i in 1:ncol(QuestionScores)+MaxScore+1){
      ScoreDistribution[i] <- length(which(StudentTotal == (i-1)))
    }
    
    
    for(i in 1:(ncol(QuestionScores)+MaxScore)){
      
      Question <- LongQuestionGrades[,j+1]
      
      Student0 <- which(StudentTotal == 0)
      StudentQuestion0 <- Question[Student0]
      CorrectCount0 <- sum(StudentQuestion0)
      ProbabilityCorrect0 <- CorrectCount0/ScoreDistribution[1]
      
      Student <- which(StudentTotal == i)
      StudentQuestion <- Question[Student]
      CorrectCount <- sum(StudentQuestion)
      ProbabilityCorrect[i] <- CorrectCount/length(Student)
    }
    
    ProbabilityCorrectTotal <- c(ProbabilityCorrect0, ProbabilityCorrect)
    
    drop <- which(!is.finite(ProbabilityCorrectTotal))
    
    ScorePattern <- ScorePattern[-drop]
    ProbabilityCorrectTotal <- ProbabilityCorrectTotal[-drop]
    
    KernelDensityEstimate <- ksmooth(ScorePattern,ProbabilityCorrectTotal,kernel="normal", bandwidth=2)
    
    #Title <- paste("Question",j)  
    plot(ScorePattern,ProbabilityCorrectTotal, xlab="Test Score", ylab = "Average Score", main="Empirical Probability Plot")
    lines(KernelDensityEstimate) 
  }
  
}
