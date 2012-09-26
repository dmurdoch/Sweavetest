items <-
function(..., Correct=1, KeepLast=0, report=FALSE) {
  
  if (report) 
    stop("items() cannot produce a report; you need to call QReport()")
    
  CheckDups <- getglobal(CheckDups, TRUE)
  randomize <- getglobal(randomize, FALSE)
  
  QuestionCounter(QuestionCounter() + 1)
  
  correct(c(correct(), Correct))
  
  x <- unlist(list(...))
  if (CheckDups && any(duplicated(format(x))))  
    stop("Duplicated answers in Q", length(Answers())+1, ": ", 
         paste(format(x), collapse=" "))
  
  n <- length(x) - KeepLast
  rand <- sample(n)
  
  if (!randomize) rand <- 1:n  
  indices <- c(if (n) perms[[n]][[testversion()]][rand], n+seq_len(KeepLast))
  
  if (!is.na(Correct)) {
    x[Correct] <- paste("\\Correct", x[Correct])
    Answers(c(Answers(), which(indices == Correct)))
  }
  
  x <- x[indices]
  QuestionIndex(c(QuestionIndex(),QuestionCounter()))
  
  y <- paste("\\item",x,"\n", sep=" ")
  cat(y)
  
  if(length(indices)<5){
    indices <- c(indices,rep(NA,5-length(indices)))
  }
  
  Index(rbind(Index(),data.frame(Question=QuestionCounter(), 
  			       ExamCode=versioncode(),
  			       Correct=Correct,
  			       A=indices[1],
  			       B=indices[2],
  			       C=indices[3],
  			       D=indices[4],
  			       E=indices[5])))	
}

QReport <- function() {
  if (Version() != "Report") return(invisible())
  
  CheckDups <- getglobal(CheckDups, TRUE)
  randomize <- getglobal(randomize, FALSE)
  
  DR <- getglobal(DR,c())
  ID <- getglobal(ID,c())
  PB <- getglobal(PB,c())
  AnswerCountMatrix <- getglobal(AnswerCountMatrix,c())
  GradedTests <- getglobal(GradedTests,c())
  student <- GradedTests$Answers
  correct <- GradedTests$Correct
  DR <- DR[QuestionCounter()]
  ID <- ID[QuestionCounter()]
  PB <- PB[QuestionCounter()]
  CorrectIndex <- getglobal(CorrectIndex, C())

  if(testversion() > 4){
    testversion(1)
  }
    
  cat("\\ \\\\")
  
  AnswerCounts <- AnswerCountMatrix[QuestionCounter(),]
  drop <- which(is.na(AnswerCounts))
  if(length(drop) > 0){
    AnswerCounts <- AnswerCounts[-drop]
  }
  Options <- paste(" ", c(1:length(AnswerCounts)), sep="")
  Resp <- 100*AnswerCounts/nrow(GradedTests)
  Dis <- DistractorDiscrimination(GradedTests,QuestionCounter())
  CountFrame <- data.frame(Options,AnswerCounts,Resp,Dis)
  
  StatNames <- c("Difficulty Rating", "Item Discriminator", "Point Biserial")
  Stats <- c(DR, ID, PB)
  StatFrame <- data.frame(StatNames,Stats)
  
  Values <- function(x){x}
  Frequency <- function(x){x}
  Percentage <- function(x){x}
  Discrimination <- function(x){x}
  
  cat("\\begin{table}[h]")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\hspace{.2in}")
  latex(tabular(Heading("Option")*Options~Heading()*Format(digits=2)*AnswerCounts*Frequency+Heading()*Format(digits=2)*Resp*Percentage+Heading()*Format(digits=2)*Dis*Discrimination, data=CountFrame))
  cat("\\end{subtable}")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\centering")
  cat("\\hfill")
  latex(tabular(Heading("Item Analysis")*StatNames ~ Heading()*Format(digits=2)*Stats*Values, data=StatFrame))
  cat("\\end{subtable}")
  cat("\\end{table}")
  
  cat("\\ \\\\")
  
  Warnings(DR,ID,PB)
  
  fignum(fignum() + 1)
  
  cat("\\begin{figure}[h]")
  dir.create("Sweavetest", showWarnings=FALSE)
  filename <- file.path( "Sweavetest", paste("fig", fignum(), ".pdf", sep=""))
  
  pdf(filename, width=8, height=4)
  answerPlots(student,correct,GradedTests$ExamCode, QuestionCount = QuestionCounter())
  dev.off()
  cat("\\hspace{.05in}")
  cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
  
  fignum(fignum() + 1)
  
  filename <- file.path( "Sweavetest", paste("fig", fignum(), ".pdf", sep=""))
  
  pdf(filename, width=8, height=4)
  EmpiricalProbabilityPlot(GradedTests, QuestionCounter())
  dev.off()
  cat("\\hspace{.08in}")
  cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
  cat("\\end{figure}")
  cat("\\newpage")    
  invisible()
}

enumerate <- function(..., Correct = 1, KeepLast = 0, report = TRUE) {
  cat("\\begin{enumerate}\n")
  items(..., Correct=Correct, KeepLast=KeepLast)
  cat("\\end{enumerate}\n")

  if (report)
    QReport()
}