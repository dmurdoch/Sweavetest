horiz <-
function(..., Correct=1, KeepLast=0, CheckDups, randomize, itemlabels, Answers, Index, QuestionIndex,DR,ID,PB,AnswerCountMatrix, GradedTests, fignum, correct) {
  
  randomize <- getglobal(randomize, FALSE)
  itemlabels <- getglobal(itemlabels, paste("(", letters, ")", sep=""))
  Answers <- getglobal(Answers, c())
  CheckDups <- getglobal(CheckDups, TRUE)
  Index <- getglobal(Index, c())
  QuestionIndex <- getglobal(QuestionIndex,c())
  correct <- getglobal(correct,c())
  QuestionCounter(QuestionCounter()+1)
  
  if(Version() != "Report"){
    correct <- c(correct, Correct)
    .STEnv$correct <- correct
  }
  
  if(Version() == "Report"){
    DR <- getglobal(DR,c())
    ID <- getglobal(ID,c())
    PB <- getglobal(PB,c())
    fignum <- getglobal(fignum,0)
    AnswerCountMatrix <- getglobal(AnswerCountMatrix,c())
    GradedTests <- getglobal(GradedTests,c())
    student <- GradedTests$Answers
    correct <- GradedTests$Correct
    DR <- DR[QuestionCounter()]
    ID <- ID[QuestionCounter()]
    PB <- PB[QuestionCounter()]
    Index <- getglobal(Index, c())
    CorrectIndex <- getglobal(CorrectIndex, c())
  }
   
  if(Version() != "Report"){
    x <- unlist(list(...))
    if (CheckDups && any(duplicated(format(x))))  
      stop("Duplicated answers in Q", length(Answers)+1, ": ", 
         paste(format(x), collapse=" ")) 
    n <- length(x)-KeepLast  
    rand <- sample(n)  
    if (!randomize) rand <- 1:n  
    indices <- c(if (n) perms[[n]][[testversion()]][rand], n+seq_len(KeepLast))
    if (!is.na(Correct)) {
      x[Correct] <- paste("\\Correct", x[Correct])
      .STEnv$Answers <- c(Answers, which(indices == Correct))
    }		 				  
		 
    .STEnv$LastIndices <- indices		 		 
			 
    labels <- itemlabels[1:(n+KeepLast)]		 	 
    x <- x[indices]		 				  
					  
    .STEnv$QuestionIndex <- c(QuestionIndex,QuestionCounter())	 
					  
    cat( paste(labels, "\\hspace{1ex}", x, "\\hfill") )	 
	 
    if(length(indices)<5){		 			 
	indices <- c(indices,rep(NA,5-length(indices)))						 
    }		 				  
			 
    .STEnv$Index <- rbind(Index,indices)		 	 
  }
  
  ###NEW ADDITIONS IF REPORT IS WANTED###
  
  if(Version() == "Report"){
    
    if(testversion() > 4){
      testversion(1)
    }
    
    x <- unlist(list(...))
    if (CheckDups && any(duplicated(format(x))))  
      stop("Duplicated answers in Q", length(Answers)+1, ": ", 
           paste(format(x), collapse=" ")) 
    n <- length(x)-KeepLast  
    rand <- sample(n)  
    if (!randomize) rand <- 1:n  
    indices <- c(if (n) perms[[n]][[testversion()]][rand], n+seq_len(KeepLast))
    if (!is.na(Correct)) {
      x[Correct] <- paste("\\Correct", x[Correct])
      .GlobalEnv$Answers <- c(Answers, which(indices == Correct))
    }
    .GlobalEnv$LastIndices <- indices
    
    labels <- itemlabels[1:(n+KeepLast)]
    x <- x[indices]
    
    cat("\\ \\\\")
    
    cat( paste(labels, "\\hspace{1ex}", x, "\\hfill") )
    
    AnswerCounts <- AnswerCountMatrix[QuestionCounter(),]
    drop <- which(is.na(AnswerCounts))
    if(length(drop)>0){
    AnswerCounts <- AnswerCounts[-drop]
    }
    Options <- paste(" ", c(1:length(AnswerCounts)), sep="")
    Resp <- 100*AnswerCounts/nrow(GradedTests)
    Dis <- DistractorDiscrimination(GradedTests,QuestionCounter(),Index)
    CountFrame <- data.frame(Options,AnswerCounts,Resp,Dis)
    
    StatNames <- c("Difficulty Rating", "Item Discriminator", "Point Biserial")
    Stats <- c(DR, ID, PB)
    StatFrame <- data.frame(StatNames,Stats)
    
    Values <- function(x){x}
    Frequency <- function(x){x}
    Percentage <- function(x){x}
    Discrimination <- function(x){x}
    
    cat("\\ \\\\")
    cat("\\ \\\\")
    
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
    
    fignum <- fignum + 1
    
    cat("\\begin{figure}[h]")
    dir.create("Sweavetest", showWarnings=FALSE)
    filename <- file.path( "Sweavetest", paste("fig", fignum, ".pdf", sep=""))
    
    pdf(filename, width=8, height=4)
    answerPlots(student,correct,GradedTests$ExamCode, QuestionCount = QuestionCounter())
    dev.off()
    cat("\\hspace{.05in}")
    cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
    
    fignum <- fignum + 1
    .STEnv$fignum <- fignum
    
    filename <- file.path( "Sweavetest", paste("fig", fignum, ".pdf", sep=""))
                
    pdf(filename, width=8, height=4)
    EmpiricalProbabilityPlot(GradedTests, QuestionCounter())
    dev.off()
    cat("\\hspace{.08in}")
    cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
    cat("\\end{figure}")
    cat("\\newpage")
  }
}
