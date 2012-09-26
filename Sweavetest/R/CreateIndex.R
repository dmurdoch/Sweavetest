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
  dimnames(result) <- list(versions, qs, c(LETTERS[1:5], "Blank", "Bad"))
  
  for(i in seq_len(NumS)){
    ExamCode <- as.character(StudentAnswers$ExamCode[i])
    answers <- student[i, qs]
    answers[ ! (answers %in% c(LETTERS[1:5], " ")) ] <- "Bad"
    answers[ answers == " " ] <- "Blank"
    indices <- cbind(ExamCode, as.character(qs), answers)
    result[indices] <- result[indices] + 1
  }
  result
}

CreateIndex <- function(fromGradedTests=GradedTests(), mainIndex=Index()){
  qs <- seq_len(max(nchar(fromGradedTests$Correct)))
  Counts <- AnswerCounts(fromGradedTests, qs)
  
  Versions <- dimnames(Counts)[[1]]
  result <- data.frame(ExamCode = character(0), Question=character(0), 
  		       Correct=numeric(0), Score=numeric(0),
                       A1=numeric(0), A2=numeric(0), A3=numeric(0), 
                       A4=numeric(0), A5=numeric(0),
                       Blank=numeric(0), Bad=numeric(0),
                       R1=character(0), R2=character(0), R3=character(0),
                       R4=character(0), R5=character(0))
  for (v in Versions) {
    counts <- Counts[v,,]
    index <- mainIndex[mainIndex$ExamCode == v,]
    if (nrow(index)) {
      for (q in qs) {
        A <- rep(NA, 5)
        R <- rep(NA, 5)
        thisq <- index[index$Question == q,,drop=FALSE]
        if (!is.na(thisq$A)) {
          A[thisq$A] <- counts[q, "A"]
          R[thisq$A] <- "A"
        }
        if (!is.na(thisq$B)) {
          A[thisq$B] <- counts[q, "B"]
          R[thisq$B] <- "B"
        }
        if (!is.na(thisq$C)) {
          A[thisq$C] <- counts[q, "C"]
          R[thisq$C] <- "C"
        }
        if (!is.na(thisq$D)) {
          A[thisq$D] <- counts[q, "D"]
          R[thisq$D] <- "D"
        }
        if (!is.na(thisq$E)) {
          A[thisq$E] <- counts[q, "E"]
          R[thisq$E] <- "E"
        }
        Correct <- thisq$Correct
        result <- rbind(result, data.frame(ExamCode=v, 
                      Question=q, 
                      Correct=Correct, Score=A[Correct],
                      A1=A[1], A2=A[2], A3=A[3], A4=A[4], A5=A[5], 
                      Blank=counts[q,"Blank"],
                      Bad=counts[q,"Bad"], 
                      R1=R[1], R2=R[2], R3=R[3], R4=R[4], R5=R[5]))
      }
    }
  }
  result
}