KR20 <- function(GradedTests){

qs<-seq_len(max(nchar(GradedTests$Correct)))
StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)
QuestionScores <- StudentAnswers == CorrectAnswers

P <- 0

for (i in 1:ncol(QuestionScores)){

	P[i] <- sum(QuestionScores[,i])/nrow(QuestionScores)

}

K <- ncol(QuestionScores)

StudentScores <- rowSums(QuestionScores)
VarScore <-var(StudentScores)

KR20Stat <- (K/(K-1))*(1-sum(P*(1-P))/VarScore)

return(KR20Stat)
}
