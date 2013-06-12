Initialization <- function(name, version = Version(), scanex, numversions = 4){

  Version(version)
  
  if(version != "Report"){
    cat("\\renewcommand{\\labelenumii}{(\\Alph{enumii})}")
  }
  if(version == "Report"){
    cat("\\renewcommand{\\labelenumii}{\\arabic{enumii})}")
  }
  cat("\\renewcommand{\\labelenumi}{{\\bf\\arabic{enumi})}}")
  
  ###Initialization Variables###
  options(width=60)
  
  ###Test Creation Variables###
    NumVersions(numversions)
    VersionCodes(sort(sample(100:999, numversions)))
    randomize(TRUE)
    testversion(1)
    itemlabels(paste0("(", LETTERS, ")"))
    Index(data.frame(Question=numeric(0), ExamCode=character(0), 
                     Correct=numeric(0), A=numeric(0), B=numeric(0), 
                     C=numeric(0), D=numeric(0), E=numeric(0)))
    QuestionCounter(0)
    QuestionIndex(c())
    fignum(0)
    CheckDups(TRUE)
    TestName(name)
  
  ###Report Creation Variables###
  if(version == "Report") {
    itemlabels(paste0("(", c(1:26), ")"))
    Index(read.csv(paste0(TestName(), "Index.csv")))
    if (is.character(scanex))
      scanex <- readScanex(scanex)
    GradedTests(grades(scanex))
  }
  
  if(version == "Student"){
    cat("\\newcommand{\\Correct}{}")
  }
  if(version == "Report" | version == "Teacher"){
    randomize(FALSE)
    testversion(0)
    cat("\\newcommand{\\Correct}{*}")
  }
  
  cat("\\newcommand{\\B}{\\underline{\\LARGE\\hspace{1em}\\vspace{0.4in}\\ }}")
  
}