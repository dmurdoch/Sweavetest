PointBiserial <- function(GradedTests, Questions=qs){

  qs<-seq_len(max(nchar(GradedTests$Correct)))
  StudentAnswers <- answerMatrix(GradedTests$Answers,qs)[,Questions,drop=FALSE]
  CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)[,Questions,drop=FALSE]
  QuestionScores <- StudentAnswers == CorrectAnswers
  StudentTotal <- GradedTests$Grade

  Mp <- rep(0,ncol(QuestionScores))
  Mq <- rep(0,ncol(QuestionScores))

  for(i in 1:ncol(QuestionScores)){
	StudentsRight <- 0
	StudentsWrong <- 0
	for (j in 1:nrow(QuestionScores)){
		if (QuestionScores[j,i] == TRUE){
			StudentsRight <- c(StudentsRight, StudentTotal[j])
			}
		else StudentsWrong <- c(StudentsWrong, StudentTotal[j])
	}
    StudentsRight <- StudentsRight[-1]
    StudentsWrong <- StudentsWrong[-1]

    if (length(StudentsRight))
      Mp[i] <- mean(StudentsRight)
    else 
      Mp[i] <- mean(StudentTotal)
    if (length(StudentsWrong))
      Mq[i] <- mean(StudentsWrong)
    else
      Mq[i] <- mean(StudentTotal)
  }
	
  P <- colSums(QuestionScores)/nrow(QuestionScores)
  Q <- 1 - P
  sd <- sqrt(var(GradedTests$Grade))

  pointbiserial <- (Mp-Mq)*sqrt(P*Q)/sd

  return(pointbiserial)

}
