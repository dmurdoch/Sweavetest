Initialization <- function(Initialize = TRUE, Version){
  
  Version <- getglobal(Version, "Student")
  
  if(Initialize == TRUE){
  
  if(Version != "Report"){
  cat("\\renewcommand{\\labelenumii}{(\\Alph{enumii})}")
  }
  if(Version == "Report"){
    cat("\\renewcommand{\\labelenumii}{\\arabic{enumii})}")
  }
  cat("\\renewcommand{\\labelenumi}{{\\bf\\arabic{enumi})}}")
  
  ###Initialization Variables###
  options(width=60)
  dir.create("figs", showWarn=FALSE)
  newCommands()
  
  ###Test Creation Variables###
  if(Version != "Teacher"){
  randomize <<- TRUE
  }
  if(Version == "Teacher"){
    randomize <<- FALSE
  }
  itemlabels <<- paste("(", LETTERS, ")", sep="")
  Answers <<- c()
  Index <<- c()
  QuestionCounter <<- 0
  QuestionIndex <<- c()
  fignum <<- 0
  correct <<- ExamNum[testversion]
  
  ###Report Creation Variables###
  if(Version == "Report"){
    randomize <<- FALSE
    itemlabels <<- paste("(", c(1:26), ")", sep="")
    Index <<- read.table("TestIndex.dat")
    CorrectIndex <<- read.table("CorrectIndex.dat")
    GradedTests <<- grades(scanex)
    KR <<- KR20(GradedTests)
    FD <<- FergusonsDelta(GradedTests)
    DR <<- DifficultyRating(GradedTests)
    ID <<- ItemDiscriminator(GradedTests)
    PB <<- PointBiserial(GradedTests)
    AnswerCountMatrix <<- CreateIndex(Index, GradedTests)
    testversion <<- 6
  }
  
  if(Version == "Student"){
    cat("\\newcommand{\\Correct}{}")
  }
  if(Version == "Report" | Version == "Teacher"){
    cat("\\newcommand{\\Correct}{*}")
  }
  
  cat("\\newcommand{\\B}{\\underline{\\LARGE\\hspace{1em}\\vspace{0.4in}\\ }}")
  
  }
}