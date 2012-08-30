StatisticalOverview <- function(GradedTests, KR, FD, fignum, Version){

  Version <- getglobal(Version, "Student")
  
if(Version == "Report"){
  
  KR <- getglobal(KR,0)
  FD <- getglobal(FD,0)
  GradedTests <- getglobal(GradedTests,c())
  fignum <- getglobal(fignum,0)
  
  weight <- max(seq_len(max(nchar(GradedTests$Correct))))
  
  cat("\\begin{center}")
  cat("\\Huge\\bf EXAM REPORT")
  cat("\\end{center}")
  cat("\\newpage")
  
  cat("\\begin{center}")
  cat("{\\LARGE\\bf Statistical Overview}")
  cat("\\end{center}")
  cat("\\ \\\\")
  cat("\\vspace{.2 in}")

  ###Produce Descriptive Statistics Table###
  grades <- GradedTests$Grade
  Percentage <- grades/weight

  Mean <- function(x) mean(x)
  Quantile25 <- function(x) quantile(x, .25)
  Quantile75 <- function(x) quantile(x, .75)
  Median <- function(x) quantile(x, .50)
  Max <- function(x) max(x)
  Min <- function(x) min(x)
  StdDev <- function(x) sqrt(var(x)) 

  pct <- function(x) 100*mean(x)/weight

  ###Produce Student Count Table and Test Means Tab;e###

  cat("\\begin{table}[h]")
  cat("\\centering")
  cat("\\caption{Descriptive Statistics}")
  latex(tabular((Heading(Section)*factor(Section)+1) ~ Format(digits=2)*((Heading())*Percentage*Mean+(Heading())*Percentage*StdDev
              +(Heading())*Percentage*Max+(Heading())*Percentage*Quantile75+(Heading())*Percentage*Median
              +(Heading())*Percentage*Quantile25+(Heading())*Percentage*Min), data=GradedTests))
  cat("\\end{table}")

  cat("\\ \\\\")

  cat("\\begin{table}[h]")
  cat("\\caption{Student Counts and Test Means}")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\centering")
  cat("\\caption{Student Counts}")
  latex(tabular((Heading("Exam Code")*factor(ExamCode)+1) 
              ~ (Heading(Section)*factor(Section)+1), data=GradedTests))
  cat("\\end{subtable}")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\centering")
  cat("\\caption{Test Means}")
  latex(tabular((Heading("Exam Code")*factor(ExamCode)+1) 
              ~ (Heading(Section)*factor(Section)+1)*Heading()*Heading()*Grade
              *pct*Format(digits=3), data=GradedTests))
  cat("\\end{subtable}")
  cat("\\end{table}")

  cat("\\ \\\\")
  cat("\\ \\\\")

  ###Produce Reliability Statistics###
  Values <- function(x){x}

  Names <- c("Kuder-Richardson 20", "Ferguson's Delta")
  Stats <- c(KR,FD)
  StatFrame <- data.frame(Names,Stats)

  cat("\\begin{table}[h]")
  cat("\\centering")
  cat("\\caption{Test Reliability}")
  latex(tabular(Heading("Reliability Statistics")*Names~Heading()*Format(digits=2)*Stats*Values, data=StatFrame))
  cat("\\end{table}")

  cat("\\ \\\\")
  cat("\\ \\\\")

  if(KR < .7 | FD < .9){
    cat("\\ \\\\")
    cat("{\\bf Warning:}")
    cat("\\ \\\\")
    cat("\\ \\\\")
    cat("The reliability of this test is poor. Please see the appendix for suggestions on improving test reliability.")
    cat("\\ \\\\")
    cat("\\ \\\\")
  }

  ###Produce Histogram of Percentages Achieved by Student & Answer Correlation Plot###
  Percentage <- 100*GradedTests$Grade/weight

  fignum <- fignum + 1

  dir.create("Sweavetest", showWarnings=FALSE)
  filename <- file.path( "Sweavetest", paste("fig", fignum, ".pdf", sep=""))

  pdf(filename, width=8, height=4)
  GradeHistogram <- hist(Percentage, main="Histogram of Student Scores", breaks = c(0,10,20,30,40,50,60,70,80,90,100))
  dev.off()            
  cat(paste("\\includegraphics[width=3.25in]{", filename, "}\n", sep=""))

  fignum <- fignum + 1
  .STEnv$fignum <- fignum

  filename <- file.path( "Sweavetest", paste("fig", fignum, ".pdf", sep=""))

  pdf(filename, width=8, height=4)
  answerCorrelations(GradedTests$Answers, GradedTests$Correct)
  dev.off()            
  cat(paste("\\includegraphics[width=3.25in]{", filename, "}\n", sep=""))

  cat("\\newpage")
  cat("\\begin{center}")
  cat("{\\large\\bf Additional Exam Information and Instructions}")
  cat("\\ \\\\")
  cat("{\\small As Provided by the Instructor}")
  cat("\\end{center}")
  cat("\\ \\\\")
  cat("\\vspace{.5in}")

  .STEnv$testversion <- 5
  
  }
}