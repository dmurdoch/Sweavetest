
getglobal <- function(var, default) {
  name <- deparse(substitute(var))
  if (exists(name, .STEnv)) var <- get(name, .STEnv)
  else var <- default
  var
}

# The functions below give access to global variables
#  Version -- which type of processing to do
#  testversion -- which of the 4 test versions
#  QuestionCounter -- which is the current question number
#  correct -- a vector of correct responses (in the authored order)
#  Answers -- a vector of correct responses (in the randomized order)
#  QuestionIndex -- a vector of question numbers corresponding to the above
#  Index -- a dataframe giving the randomization for the test
#  fignum -- the figure number in reports
#  randomize -- whether to randomize answers
#  versioncode -- read-only access to the ExamNum vector:  just gives the testversion() entry
#  itemlabels -- the labels to use for answer options
#  GradedTests -- the dataframe of test results

Version <- function(x = "Student"){
  if (!missing(x)) {
    x <- match.arg(x, c("Teacher",  "Student", "Report", "Low Level"))
    .STEnv$Version <- x
    return(invisible(x))
  } 
  .STEnv$Version
}

testversion <- function(x = 1){
  if (!missing(x)) {
    .STEnv$testversion <- x
    return(invisible(x))
  }
  .STEnv$testversion
}

testversion <- function(x = 1){
  if (!missing(x)) {
    .STEnv$testversion <- x
    return(invisible(x))
  }
  .STEnv$testversion
}

QuestionCounter <- function(x = 0){
  if (!missing(x)) {
    .STEnv$QuestionCounter <- x
    return(invisible(x))
  }
  .STEnv$QuestionCounter
}

correct <- function(x = c()){
  if (!missing(x)) {
    .STEnv$correct <- x
    return(invisible(x))
  }
  .STEnv$correct
}

Answers <- function(x = c()){
  if (!missing(x)) {
    .STEnv$Answers <- x
    return(invisible(x))
  }
  .STEnv$Answers
}

QuestionIndex <- function(x = c()){
  if (!missing(x)) {
    .STEnv$QuestionIndex <- x
    return(invisible(x))
  }
  .STEnv$QuestionIndex
}

Index <- function(x = c()){
  if (!missing(x)) {
    .STEnv$Index <- x
    return(invisible(x))
  }
  .STEnv$Index
}

fignum <- function(x = 0){
  if (!missing(x)) {
    .STEnv$fignum <- x
    return(invisible(x))
  }
  .STEnv$fignum
}

randomize <- function(x = TRUE) {
  if (!missing(x)) {
    .STEnv$randomize <- x
    return(invisible(x))
  }
  .STEnv$randomize
}

itemlabels <- function(x = TRUE) {
  if (!missing(x)) {
    .STEnv$itemlabels <- x
    return(invisible(x))
  }
  .STEnv$itemlabels
}

GradedTests <- function(x = NULL) {
  if (!missing(x)) {
    .STEnv$GradedTests <- x
    return(invisible(x))
  }
  .STEnv$GradedTests
}


versioncode <- function() {
  .STEnv$ExamNum[testversion()]
}
