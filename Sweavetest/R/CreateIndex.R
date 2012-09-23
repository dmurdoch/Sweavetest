answerMatrix <- function(answers, qs) {
  answerS <- strsplit(answers, "")
  len <- length(qs)
  answerS <- lapply(answerS,  
                function(x) {
                  if (length(x) < len) 
                    x <- c(x, rep(" ", len-length(x)))
                  x[qs]
                }
             )       
  result <- do.call(rbind, answerS)
  colnames(result) <- qs
  result
}

AnswerCounts <- function(StudentAnswers, qs=seq_len(max(nchar(Answers)))) {
  NumS <- nrow(StudentAnswers)
  NumQ <- length(qs)
  
  Answers <- StudentAnswers$Answers
  
  student <- answerMatrix(Answers, qs)
  
  versions <- as.character(unique(StudentAnswers$ExamCode))
  NumV <- length(versions)
  
  result <- array(0, c(NumV, NumQ, 7))
  dimnames(result) <- list(versions, qs, c(LETTERS[1:5], " ", "Bad"))
  
  for(i in seq_len(NumS)){
    ExamCode <- as.character(GradedTests$ExamCode[i])
    answers <- student[i, qs]
    answers[ ! (answers %in% c(LETTERS[1:5], " ")) ] <- "Bad"
    indices <- cbind(ExamCode, as.character(qs), answers)
    result[indices] <- result[indices] + 1
  }
  result
}

CreateIndex <- function(Index = getglobal(Index, c()), GradedTests){
  qs <- seq_len(max(nchar(GradedTests$Correct)))
  Counts <- AnswerCounts(GradedTests, qs)
  
  Versions <- dimnames(Counts)[[1]]
  result <- data.frame(ExamCode = character(0), Question=character(0), 
                       A=numeric(0), B=numeric(0), C=numeric(0), D=numeric(0), E=numeric(0),
                       Blank=numeric(0), Bad=numeric(0))
  for (v in seq_along(Versions))
    result <- rbind(result, data.frame(ExamCode=v, Question=qs, Counts[v,,]))
  result
}