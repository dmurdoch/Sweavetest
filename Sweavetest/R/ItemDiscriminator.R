ItemDiscriminator <- function(GradedTests, Questions=qs){

qs<-seq_len(max(nchar(GradedTests$Correct)))
StudentAnswers <- answerMatrix(GradedTests$Answers,qs)[,Questions,drop=FALSE]
CorrectAnswers <- answerMatrix(GradedTests$Correct,qs)[,Questions,drop=FALSE]
QuestionScores <- StudentAnswers == CorrectAnswers
StudentTotal <- GradedTests$Grade

Upper25 <- which(StudentTotal >= quantile(StudentTotal, .75))
Lower25 <- which(StudentTotal <= quantile(StudentTotal, .25))

UpperScore <- 0
LowerScore <- 0

for (i in 1:ncol(QuestionScores)){
	UpperScore[i] <- sum(QuestionScores[Upper25,i])/length(Upper25)
	LowerScore[i] <- sum(QuestionScores[Lower25,i])/length(Lower25)
}

ItemDisc <- UpperScore - LowerScore

return(ItemDisc)

}
