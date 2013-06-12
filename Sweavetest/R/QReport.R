QReport <- function(newpage = TRUE) {
  if (Version() != "Report") return(invisible())
  
  AnswerCountMatrix <- CreateIndex()
  student <- GradedTests()$Answers
  correct <- GradedTests()$Correct
  DR <- DifficultyRating(GradedTests(), QuestionCounter())
  ID <- ItemDiscriminator(GradedTests(), QuestionCounter())
  PB <- PointBiserial(GradedTests(), QuestionCounter())
    
  cat("\\ \\\\")
  
  AnswerCounts <- AnswerCountMatrix[AnswerCountMatrix$Question==QuestionCounter(),
                                    paste0("A", 1:5)]
  AnswerCounts <- as.numeric(apply(AnswerCounts, 2, sum))
  drop <- which(is.na(AnswerCounts))
  if(length(drop) > 0){
    AnswerCounts <- AnswerCounts[-drop]
  }
  Resp <- 100*AnswerCounts/nrow(GradedTests())
  Dis <- DistractorDiscrimination(GradedTests(),QuestionCounter())
  CountFrame <- data.frame(Dis, Frequency=AnswerCounts,Percentage=Resp)
  
  StatNames <- c("Difficulty Rating", "Item Discriminator", "Point Biserial")
  Stats <- c(DR, ID, PB)
  StatFrame <- data.frame(StatNames,Values=Stats)
  
  cat("\\begin{table}[h]")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\hspace{.2in}")
  latex(tabular(Factor(Option)*Heading()*identity ~ Frequency + Percentage + Discrimination,
                data = CountFrame), digits=2)
  cat("\\end{subtable}")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\centering")
  cat("\\hfill")
  latex(tabular(Factor(StatNames, name="Item Analysis")*Heading()*identity ~ Values, 
        data=StatFrame), digits=2)
  cat("\\end{subtable}")
  cat("\\end{table}")
  
  cat("\\ \\\\")
  
  Warnings(DR,ID,PB)
  
  fignum(fignum() + 1)
  
  dirname <- paste0(TestName(),"Figs")
  dir.create(dirname, showWarnings=FALSE)
  filename <- file.path(dirname, paste0("fig", fignum(), ".pdf"))
  
  pdf(filename, width=8, height=4)
  answerPlots(QuestionCounter())
  dev.off()
  cat("\\hspace{.05in}")
  cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
  
  fignum(fignum() + 1)
  
  filename <- file.path(dirname, paste0("fig", fignum(), ".pdf"))
  
  pdf(filename, width=8, height=4)
  EmpiricalProbabilityPlot(GradedTests(), QuestionCounter())
  dev.off()
  cat("\\hspace{.08in}")
  cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
  if (newpage)
    cat("\\newpage")    
  invisible()
}
