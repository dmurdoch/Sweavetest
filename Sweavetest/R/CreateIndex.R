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

CreateIndex <- function(gradedTests=GradedTests()){
  numQ <- max(nchar(gradedTests$Correct))
  qs <- seq_len(numQ)
  
  Counts <- AnswerCounts(gradedTests, qs)
  
  Versions <- dimnames(Counts)[[1]]
  Aname <- paste0("A", 1:5)
  Rname <- paste0("R", 1:5)
  
  cresult <- matrix(NA_character_, nrow=1, ncol=7)
  colnames(cresult) <- c("ExamCode", "Question", Rname)
  nresult <- matrix(NA_real_, nrow=1, ncol=9)
  colnames(nresult) <- c("Correct", "Score", Aname, "Blank", "Bad")
  mainIndex <- Index()
  if (is.null(mainIndex)) {
    message("No Index() found, creating dummy one.")
    keep <- integer(length(Versions))
    for (i in seq_along(Versions))
      keep[i] <- which(gradedTests$ExamCode == Versions[i])[1]
      
    correctMatrix <- answerMatrix(gradedTests$Correct[keep], qs)
    
    letterToNumber <- c(A=1, B=2, C=3, D=4, E=5)
    
    num <- numQ*length(Versions)
    
    mainIndex <- data.frame(Question=rep(qs,length(Versions)),
      ExamCode=rep(Versions, each=numQ),
      Correct=letterToNumber[t(correctMatrix)],
      A=rep(1, num),
      B=rep(2, num),
      C=rep(3, num),
      D=rep(4, num),
      E=rep(5, num))
      
    Index(mainIndex)
  }

  for (v in Versions) {
    counts <- Counts[v,,]
    index <- mainIndex[mainIndex$ExamCode == v,]
    if (nrow(index)) {
      thisrow <- nrow(cresult)
      newrows <- length(qs)
      cresult <- cresult[c(seq_len(thisrow), rep(1, newrows)),]
      cresult[thisrow + 1:newrows, "ExamCode"] <- v
      cresult[thisrow + 1:newrows, "Question"] <- qs
      nresult <- nresult[c(seq_len(thisrow), rep(1, newrows)),]
      for (q in qs) {
        thisrow <- thisrow + 1
        
        thisq <- index[index$Question == q,,drop=FALSE]
        if (!is.na(thisq$A)) {
          nresult[thisrow, Aname[thisq$A]] <- counts[q, "A"]
          cresult[thisrow, Rname[thisq$A]] <- "A"
        }
        if (!is.na(thisq$B)) {
          nresult[thisrow, Aname[thisq$B]] <- counts[q, "B"]
          cresult[thisrow, Rname[thisq$B]] <- "B"
        }
        if (!is.na(thisq$C)) {
          nresult[thisrow, Aname[thisq$C]] <- counts[q, "C"]
          cresult[thisrow, Rname[thisq$C]] <- "C"
        }
        if (!is.na(thisq$D)) {
          nresult[thisrow, Aname[thisq$D]] <- counts[q, "D"]
          cresult[thisrow, Rname[thisq$D]] <- "D"
        }
        if (!is.na(thisq$E)) {
          nresult[thisrow, Aname[thisq$E]] <- counts[q, "E"]
          cresult[thisrow, Rname[thisq$E]] <- "E"
        }
        nresult[thisrow, "Correct"] <- thisq$Correct
        nresult[thisrow, "Score"] <- nresult[thisrow, Aname[thisq$Correct]]
        nresult[thisrow, "Blank"] <- counts[q, "Blank"]
        nresult[thisrow, "Bad"] <- counts[q, "Bad"]
      }
    }
  }
  data.frame(cresult[-1,1:2,drop=FALSE], nresult[-1,,drop=FALSE], cresult[-1,3:7,drop=FALSE])
}