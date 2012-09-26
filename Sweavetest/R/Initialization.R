Initialization <- function(Initialize = TRUE){
  if(Initialize == TRUE){
  
  if(Version() != "Report"){
    cat("\\renewcommand{\\labelenumii}{(\\Alph{enumii})}")
  }
  if(Version() == "Report"){
    cat("\\renewcommand{\\labelenumii}{\\arabic{enumii})}")
  }
  cat("\\renewcommand{\\labelenumi}{{\\bf\\arabic{enumi})}}")
  
  ###Initialization Variables###
  options(width=60)
  dir.create("figs", showWarnings=FALSE)
  newCommands()
  
  ###Test Creation Variables###
    randomize(Version() != "Teacher")
    itemlabels(paste0("(", LETTERS, ")")
    Answers(c())
    Index(data.frame(Question=numeric(0), ExamCode=character(0), 
                     Correct=numeric(0), A=numeric(0), B=numeric(0), 
                     C=numeric(0), D=numeric(0), E=numeric(0)))
    QuestionCounter(0)
    QuestionIndex(c())
    fignum(0)
    correct(c())
  
  ###Report Creation Variables###
  if(Version() == "Report") with(.STEnv, {
    randomize(FALSE)
    itemlabels(paste0("(", c(1:26), ")"))
    Index(read.csv("TestIndex.csv"))
    CorrectIndex <- cbind(Index$ExamCode, Index$Correct)
    GradedTests <- grades(scanex)
    KR <- KR20(GradedTests)
    FD <- FergusonsDelta(GradedTests)
    DR <- DifficultyRating(GradedTests)
    ID <- ItemDiscriminator(GradedTests)
    PB <- PointBiserial(GradedTests)
    AnswerCountMatrix <- CreateIndex(GradedTests)
    testversion(6)
  })
  
  if(Version() == "Student"){
    cat("\\newcommand{\\Correct}{}")
  }
  if(Version() == "Report" | Version() == "Teacher"){
    cat("\\newcommand{\\Correct}{*}")
  }
  
  cat("\\newcommand{\\B}{\\underline{\\LARGE\\hspace{1em}\\vspace{0.4in}\\ }}")
  
  }
}