LongAnswer <- function(Question,Answer,Marks=0,report=TRUE,Part=0, Length=c("Sentence","Paragraph","Page")){
  
  LongQuestionCounter(LongQuestionCounter()+1)
  
  #LQMaxScores(c(LQMaxScores,Marks))
  
  if(Part == 0){
    if(!missing(Question)){
      cat(Question)
    }
    if(Version() == "Teacher" | Version() == "Report"){
      if(missing(Answer)){
        cat("Please Input an Answer")
      }else{cat(Answer)}
    }
  }
  
  if(Part > 0){
    cat("\\begin{enumerate}[label=\\alph*)]")
    cat(paste("\\setcounter{enumii}{",Part-1,"}"))
    if(missing(Question)){
      cat("\\item Please Input A Question")
    }else{cat(paste("\\item ", Question))}
    cat("\\\\")
    cat("\\\\")
    if(Version() == "Teacher" | Version() == "Report"){
      if(missing(Answer)){
        cat("No Answer Provided")
      }else{cat(Answer)}
    }
    cat("\\end{enumerate}")
  }
  
  if(Version() == "Student"){
    if(Length == "Sentence"){
      cat("\\vspace{20 mm}")
    }else if(Length == "Paragraph"){
      cat("\\vspace{60 mm}")
    }else{
      cat("\\newpage")
    }
  }
  
  if(Version() == "Report"){
    
    LQG <- LSGrades()
    LQG <- LQG[,LongQuestionCounter()+1]
    
    GradeDist <- c()
    for(i in 0:Marks){
      GradeDist <- c(GradeDist, length(which(LQG == i)))
    }
    
    Score <- seq(0,Marks,1)
    
    Resp <- GradeDist/length(LQG)
    
    CountFrame <- data.frame(Frequency=GradeDist,Percentage=Resp)
   
    ##########################
    gradedTests <- GradedTests()
    TotalGrades <- TotalGrade()
    
    TotalGradedTests <- cbind(gradedTests,TotalGrades)
    
    numQ <- max(nchar(gradedTests$Correct))
    
    Mean <- function(x) mean(x)
    Q25 <- function(x) quantile(x, .25)
    Q75 <- function(x) quantile(x, .75)
    Median <- function(x) quantile(x, .50)
    Max <- function(x) max(x)
    Min <- function(x) min(x)
    SD <- function(x) sqrt(var(x)) 
    
    Percentage <- LQG
    
    Section <- gradedTests$Section
    
    ###########################
    
    cat("\\begin{table}[h]")
    cat("\\begin{subtable}[h]{.45\\linewidth}")
    cat("\\hspace{.2in}")
    latex(tabular(Factor(Score)*Heading()*identity ~ Frequency + Percentage,
                 data = CountFrame), digits=2)
    cat("\\end{subtable}")
    cat("\\begin{subtable}[h]{.5\\linewidth}")
    cat("\\centering")
    cat("\\hfill")
    latex(tabular((Factor(Section)+1)*Heading()*Percentage ~ (n=1) + Mean + SD + Max + Q75
                  + Median + Q25 + Min), digits=2)    
    cat("\\end{subtable}")
    cat("\\end{table}")
    
    cat("\\ \\\\")
    
    fignum(fignum() + 1)
    
    dirname <- paste0(TestName(),"Figs")
    dir.create(dirname, showWarnings=FALSE)
    filename <- file.path(dirname, paste0("fig", fignum(), ".pdf"))
    
    pdf(filename, width=8, height=4)
    EmpiricalProbabilityPlot(GradedTests(), QuestionCounter(), LSGrades(), LongQuestionCounter())
    dev.off()
    cat("\\hspace{.08in}")
    cat(paste("\\includegraphics[width=\\textwidth]{", filename, "}\n", sep=""))

    cat("\\newpage")    
    invisible()
  }
  
  
}