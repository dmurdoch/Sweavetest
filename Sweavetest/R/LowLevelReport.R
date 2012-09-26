LowLevelReport <- function(scanex){
  
  ###Variable Initialization###
  GradedTests <- grades(scanex)
  KR <- KR20(GradedTests)
  FD <- FergusonsDelta(GradedTests)
  DR <- DifficultyRating(GradedTests)
  ID <- ItemDiscriminator(GradedTests)
  PB <- PointBiserial(GradedTests)
  fignum <- 0
  weight <- max(seq_len(max(nchar(GradedTests$Correct))))
  NumQ <- weight
  qs <- seq_len(max(nchar(GradedTests$Correct[1])))
  
  ###Report Begins###
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
  
  ###Produce Student Count Table and Test Means Table###
  
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
  
  filename <- file.path( "Sweavetest", paste("fig", fignum, ".pdf", sep=""))
  
  pdf(filename, width=8, height=4)
  answerCorrelations(GradedTests$Answers, GradedTests$Correct)
  dev.off()            
  cat(paste("\\includegraphics[width=3.25in]{", filename, "}\n", sep=""))
  
  cat("\\newpage")
  
  ###Individual Question Analysis###
  
  cat("\\begin{enumerate}")
  
  for(i in 1:NumQ){
  
  cat("\\item \\ \\\\")
    
  Dr <- DR[i]
  Id <- ID[i]
  Pb <- PB[i]
  Student <- GradedTests$Answers
  Correct <- GradedTests$Correct
  student <- answerMatrix(Student,qs)
  correct <- answerMatrix(Correct,qs)
  
  ExamCodes <- unique(GradedTests$ExamCode)
  ACount <- c()
  BCount <- c()
  CCount <- c()
  DCount <- c()
  ECount <- c()
  
  for(j in ExamCodes){
    WhichStudents <- which(GradedTests$ExamCode == j)
    StudentAnswers <- student[WhichStudents,i]
    ACounts <- length(which(StudentAnswers=="A"))
    BCounts <- length(which(StudentAnswers=="B"))
    CCounts <- length(which(StudentAnswers=="C"))
    DCounts <- length(which(StudentAnswers=="D"))
    ECounts <- length(which(StudentAnswers=="E"))
    ACount <- c(ACount,ACounts)
    BCount <- c(BCount,BCounts)
    CCount <- c(CCount,CCounts)
    DCount <- c(DCount,DCounts)
    ECount <- c(ECount,ECounts)
  }
  
  CountFrame <- data.frame(ExamCodes,ACount,BCount,CCount,DCount,ECount)
  
  StatNames <- c("Difficulty Rating", "Item Discriminator", "Point Biserial")
  Stats <- c(Dr, Id, Pb)
  StatFrame <- data.frame(StatNames,Stats)
  
  Values <- function(x){x}
  Frequency <- function(x){x}
  A <- function(x){x}
  B <- function(x){x}
  C <- function(x){x}
  D <- function(x){x}
  E <- function(x){x}
  Percentage <- function(x){x}
  Discrimination <- function(x){x}
  
  cat("\\begin{table}[h]")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\hspace{.2in}")
  latex(tabular(Heading("Exam Code")*ExamCodes~ Heading()*ACount*A+Heading()*BCount*B
                +Heading()*CCount*C+Heading()*DCount*D+Heading()*ECount*E, data=CountFrame))
  cat("\\end{subtable}")
  cat("\\begin{subtable}[h]{.5\\linewidth}")
  cat("\\centering")
  cat("\\hfill")
  latex(tabular(Heading("Item Analysis")*StatNames ~ Heading()*Format(digits=2)*Stats*Values, data=StatFrame))
  cat("\\end{subtable}")
  cat("\\end{table}")
  
  cat("\\ \\\\")
  
  Warnings(Dr,Id,Pb)
  
  fignum <- fignum + 1
  
  cat("\\begin{figure}[h!]")
  cat("\\vspace{-.40in}")
  dir.create("Sweavetest", showWarnings=FALSE)
  filename <- file.path( "Sweavetest", paste("fig", fignum, ".pdf", sep=""))
  
  pdf(filename, width=8, height=4)
  answerPlots(GradedTests$Answers,GradedTests$Correct,GradedTests$ExamCode,i)
  dev.off()
  cat("\\hspace{.05in}")
  cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
  
  fignum <- fignum + 1
  
  filename <- file.path( "Sweavetest", paste("fig", fignum, ".pdf", sep=""))
  
  pdf(filename, width=8, height=4)
  EmpiricalProbabilityPlot(GradedTests, i)
  dev.off()
  cat("\\hspace{.08in}")
  cat(paste("\\includegraphics[width=.5\\textwidth]{", filename, "}\n", sep=""))
  cat("\\end{figure}")
  
  if(i %% 2 == 0){
  cat("\\newpage")
  }
  
  }
  cat("\\end{enumerate}")
}