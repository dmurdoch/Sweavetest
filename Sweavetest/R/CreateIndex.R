CreateIndex <- function(Index = getglobal(Index, c()), GradedTests){
  
Index <- Index[,c("ExamCode", LETTERS[1:5]),drop=FALSE]
GradedTests <- getglobal(GradedTests,c())
NumberOfVersions <- getglobal(NumberOfVersions, 1)

qs <- seq_len(max(nchar(GradedTests$Correct)))
StudentAnswers <- answerMatrix(GradedTests$Answers,qs)
NumQ <- max(qs)

MatrixSize <- (ncol(Index)-1) * nrow(Index)

Index <- as.matrix(Index)

AnswerCountMatrix <- matrix(rep(0,MatrixSize), nrow=nrow(Index), ncol=ncol(Index)-1)

for(i in 1:NumberOfVersions){
  for(j in 1:NumQ){
    Q <- Index[j+NumQ*(i-1),]
    ExamCode <- Q[1]
    NumOpt <- ncol(Index)-1
    if(is.na(Q[6])){
      NumOpt <- NumOpt-1
    }
    for(k in 1:NumOpt){
      Position <- which(Q == k)
      Position <- Position - 1
      
      if(Position == 1){
        Correct <- "A"
      }
      
      if(Position == 2){
        Correct <- "B"
      }
      
      if(Position == 3){
        Correct <- "C"
      }
      
      if(Position == 4){
        Correct <- "D"
      }
      
      if(Position == 5){
        Correct <- "E"
      }
    StudentsInVersion <- which(GradedTests[,3]==ExamCode)
    StudentVersionAnswers <- StudentAnswers[StudentsInVersion,j]
    NumCorrAns <- length(which(StudentVersionAnswers == Correct))
    AnswerCountMatrix[j,k] <- AnswerCountMatrix[j,k] + NumCorrAns
    }
    if(NumOpt < 5){
      AnswerCountMatrix[j,5] <- NA
    }
  }
}

return(AnswerCountMatrix)
}