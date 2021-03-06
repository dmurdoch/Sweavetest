StatisticalOverview <- function( extraIntro = TRUE ){

if(Version() == "Report"){
  gradedTests <- GradedTests()
  TotalGrades <- TotalGrade()
  
  TotalGradedTests <- cbind(gradedTests,TotalGrades)
  
  KR <- KR20(gradedTests)
  FD <- FergusonsDelta(gradedTests)
  DR <- DifficultyRating(gradedTests)
  ID <- ItemDiscriminator(gradedTests)
  PB <- PointBiserial(gradedTests)
  
  numQ <- max(nchar(gradedTests$Correct))
  
  cat("\\begin{center}\n")
  cat("\\Huge\\bf EXAM REPORT\n")
  cat("\\end{center}\n")
  cat("\\newpage\n")
  
  cat("\\begin{center}\n")
  cat("{\\LARGE\\bf Statistical Overview}\n")
  cat("\\end{center}\n")
  cat("\\ \\\\\n")
  cat("\\vspace{.2 in}\n")

  cat("\\begin{table}[h]\n")
  cat("\\centering\n")
  cat("\\caption{Descriptive Statistics}\n")
  DescriptiveStatistics(100*TotalGrades/(numQ+LQMaxScores()), gradedTests$Section)
  cat("\\end{table}\n")
 
  cat("\\ \\\\\n")

  cat("\\begin{table}[h]\n")
  cat("\\caption{Student Counts and Test Means}\n")
  cat("\\begin{subtable}[h]{.5\\linewidth}\n")
  cat("\\centering\n")
  cat("\\caption{Student Counts}\n")
  latex(tabular((Heading("Exam Code")*factor(ExamCode)+1) 
              ~ (Heading(Section)*factor(Section)+1), data=gradedTests))
  cat("\\end{subtable}\n")
  cat("\\begin{subtable}[h]{.5\\linewidth}\n")
  cat("\\centering\n")
  cat("\\caption{Test Means}\n")

  pct <- function(x) 100*mean(x)/(numQ+LQMaxScores())
  
  latex(tabular((Factor(ExamCode, name="Exam Code")+1) 
              ~ (Factor(Section)+1)*Heading()*Heading()*TotalGrades
              *pct*Format(digits=3), data=TotalGradedTests))
  cat("\\end{subtable}\n")
  cat("\\end{table}\n")

  cat("\\ \\\\\n")
  cat("\\ \\\\\n")

  ###Produce Reliability Statistics###
  Values <- function(x){x}

  Names <- c("Kuder-Richardson 20", "Ferguson's Delta")
  Stats <- c(KR,FD)
  StatFrame <- data.frame(Names,Stats)

  cat("\\begin{table}[h]\n")
  cat("\\centering\n")
  cat("\\caption{Test Reliability}\n")
  latex(tabular(Factor(Names, name="Reliability Statistics")
                ~Heading()*Format(digits=2)*Stats*Values, data=StatFrame))
  cat("\\end{table}\n")

  cat("\\ \\\\\n")
  cat("\\ \\\\\n")

  if(KR < .7 & FD < .9){
    cat("\\ \\\\\n")
    cat("{\\bf Warning:}\n")
    cat("\\ \\\\\n")
    cat("\\ \\\\\n")
    cat("The calculated values for the Kuder-Richardson 20 and Ferguson's Delta statistics are both below the acceptable standards of .7 and .9 respectively. This indicates a lack of correlation between student ability and performance on the test. Please see the sections of the appendix relating to the Kuder-Richardson 20 statistic and Ferguson's Delta for suggestions on improving test reliability.\n")
    cat("\\ \\\\\n")
    cat("\\ \\\\\n")
  }
  
  if(KR < .7 & FD >= .9){
    cat("\\ \\\\\n")
    cat("{\\bf Warning:}\n")
    cat("\\ \\\\\n")
    cat("\\ \\\\\n")
    cat("The calculated value for the Kuder-Richardson 20 statistic is below the acceptable standard of .7, indicating a lack of correlation between student ability and performance on the test. Please see the section in the appendix regarding the Kuder-Richardson 20 statistic for suggestions on improving test reliability.\n")
    cat("\\ \\\\\n")
    cat("\\ \\\\\n")
  }
  
  if(KR >= .7 & FD < .9){
    cat("\\ \\\\\n")
    cat("{\\bf Warning:}\n")
    cat("\\ \\\\\n")
    cat("\\ \\\\\n")
    cat("The calculated value for Ferguson's Delta is below the acceptable standard of .9, indicating a lack of correlation between student ability and performance on the test. Please see the section in the appendix regarding Ferguson's Delta for suggestions on improving test reliability.\n")
    cat("\\ \\\\\n")
    cat("\\ \\\\\n")
  }

  ###Produce Histogram of Percentages Achieved by Student & Answer Correlation Plot###
  Percentage <- 100*gradedTests$Grade/numQ

  fignum(fignum() + 1)

  dirname <- paste0(TestName(), "Figs")
  dir.create(dirname, showWarnings=FALSE)
  filename <- file.path(dirname, paste("fig", fignum(), ".pdf", sep=""))

  pdf(filename, width=8, height=4)
  GradeHistogram <- hist(Percentage, main="Histogram of Student Scores", breaks = c(0,10,20,30,40,50,60,70,80,90,100))
  dev.off()            
  cat(paste("\\includegraphics[width=3.25in]{", filename, "}\n", sep=""))

  fignum(fignum() + 1)

  filename <- file.path( dirname, paste("fig", fignum(), ".pdf", sep=""))

  pdf(filename, width=8, height=4)
  answerCorrelations(gradedTests)
  dev.off()            
  cat(paste("\\includegraphics[width=3.25in]{", filename, "}\n", sep=""))

  if (extraIntro) {
    cat("\\newpage\n")
    cat("\\begin{center}\n")
    cat("{\\large\\bf Additional Exam Information and Instructions}\n")
    cat("\\ \\\\\n")
    cat("{\\small As Provided by the Instructor}\n")
    cat("\\end{center}\n")
    cat("\\ \\\\\n")
    cat("\\vspace{.5in}\n")
  }
}
}