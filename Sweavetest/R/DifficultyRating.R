DifficultyRating <- function(GradedTests){

  qs<-seq_len(max(nchar(GradedTests$Correct)))
  StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
  CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)
  QuestionScores <- StudentAnswers == CorrectAnswers

  DiffRate <- 0

  for(i in 1:ncol(QuestionScores)){

    DiffRate[i] <- sum(QuestionScores[,i])/nrow(QuestionScores)

  } 

  return(DiffRate)

}
