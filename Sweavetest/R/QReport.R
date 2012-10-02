QReport <- function() {
  if (Version() != "Report") return(invisible())
  
  AnswerCountMatrix <- CreateIndex()
  student <- GradedTests()$Answers
  correct <- GradedTests()$Correct
  DR <- DifficultyRating(GradedTests(), QuestionCounter())
  ID <- ItemDiscriminator(GradedTests(), QuestionCounter())
  PB <- PointBiserial(GradedTests(), QuestionCounter())

  if(testversion() > 4){
    testversion(1)
  }
    
  cat("\\ \\\\")
  
  AnswerCounts <- as.numeric(AnswerCountMatrix[QuestionCounter(),paste0("A", 1:5)])
  drop <- which(is.na(AnswerCounts))
  if(length(drop) > 0){
    AnswerCounts <- AnswerCounts[-drop]
  }
  Resp <- 100*AnswerCounts/nrow(GradedTests())
  Dis <- DistractorDiscrimination(GradedTests(),QuestionCounter())
  CountFrame <- data.frame(Dis, Frequency=AnswerCounts,Percentage=Resp)
  
  StatNames <- c("Difficulty Rating", "Item Discriminator", "Point Biserial")
  Stats <- c(DR, ID, PB)
  StatFrame <- data.frame(StatNames,Stats)
  
  cat("\\begin{table}[h]")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\hspace{.2in}")
  latex(tabular(Factor(Option)*Heading()*identity ~ Frequency + Percentage + Discrimination,
                data = CountFrame), digits=2)
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
  answerPlots(QuestionCounter())
  dev.off()
  cat("\\hspace{.05in}")
  cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
  
  fignum(fignum() + 1)
  
  filename <- file.path( "Sweavetest", paste("fig", fignum(), ".pdf", sep=""))
  
  pdf(filename, width=8, height=4)
  EmpiricalProbabilityPlot(GradedTests(), QuestionCounter())
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