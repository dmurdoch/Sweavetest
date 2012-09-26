PointBiserial <- function(graded.tests, Questions=qs){

qs<-seq_len(max(nchar(graded.tests$Correct)))
StudentAnswers <- answerMatrix(graded.tests$Answers,qs)[,Questions,drop=FALSE]
CorrectAnswers <- answerMatrix(graded.tests$Correct,qs)[,Questions,drop=FALSE]
QuestionScores <- StudentAnswers == CorrectAnswers
StudentTotal <- rowSums(QuestionScores)

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

Mp[i] <- mean(StudentsRight)
Mq[i] <- mean(StudentsWrong)
}
	
P <- colSums(QuestionScores)/nrow(QuestionScores)
Q <- 1 - P
sd <- sqrt(var(graded.tests$Grade))

pointbiserial <- (Mp-Mq)*sqrt(P*Q)/sd

return(pointbiserial)

}
