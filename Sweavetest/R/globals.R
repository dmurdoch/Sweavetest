
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
#  CheckDups -- whether to check for duplicate answers
#  versioncode -- read-only access to the ExamNum vector:  just gives the testversion() entry
#  itemlabels -- the labels to use for answer options
#  GradedTests -- the dataframe of test results

Version <- function(x = "Student"){
  if (!missing(x) || !exists("Version", .STEnv)) {
    x <- match.arg(x, c("Teacher",  "Student", "Report", "Low Level"))
    .STEnv$Version <- x
    return(invisible(x))
  } 
  .STEnv$Version
}

testversion <- function(x = 1){
  if (!missing(x) || !exists("testversion", .STEnv)) {
    .STEnv$testversion <- x
    return(invisible(x))
  }
  .STEnv$testversion
}

QuestionCounter <- function(x = 0){
  if (!missing(x) || !exists("QuestionCounter", .STEnv)) {
    .STEnv$QuestionCounter <- x
    return(invisible(x))
  }
  .STEnv$QuestionCounter
}

correct <- function(x = NULL){
  if (!missing(x) || !exists("correct", .STEnv)) {
    .STEnv$correct <- x
    return(invisible(x))
  }
  .STEnv$correct
}

Answers <- function(x = NULL){
  if (!missing(x) || !exists("Answers", .STEnv)) {
    .STEnv$Answers <- x
    return(invisible(x))
  }
  .STEnv$Answers
}

QuestionIndex <- function(x = NULL){
  if (!missing(x) || !exists("QuestionIndex", .STEnv)) {
    .STEnv$QuestionIndex <- x
    return(invisible(x))
  }
  .STEnv$QuestionIndex
}

Index <- function(x = NULL){
  if (!missing(x) || !exists("Index", .STEnv)) {
    .STEnv$Index <- x
    return(invisible(x))
  }
  .STEnv$Index
}

fignum <- function(x = 0){
  if (!missing(x) || !exists("fignum", .STEnv)) {
    .STEnv$fignum <- x
    return(invisible(x))
  }
  .STEnv$fignum
}

randomize <- function(x = TRUE) {
  if (!missing(x) || !exists("randomize", .STEnv)) {
    .STEnv$randomize <- x
    return(invisible(x))
  }
  .STEnv$randomize
}

CheckDups <- function(x = TRUE) {
  if (!missing(x) || !exists("CheckDups", .STEnv)) {
    .STEnv$CheckDups <- x
    return(invisible(x))
  }
  .STEnv$CheckDups
}

itemlabels <- function(x = paste0("(", LETTERS[1:5], ")")) {
  if (!missing(x) || !exists("itemlabels", .STEnv)) {
    .STEnv$itemlabels <- x
    return(invisible(x))
  }
  .STEnv$itemlabels
}

GradedTests <- function(x = NULL) {
  if (!missing(x) || !exists("GradedTests", .STEnv)) {
    .STEnv$GradedTests <- x
    return(invisible(x))
  }
  .STEnv$GradedTests
}

TestName <- function(x = "TestData") {
  if (!missing(x) || !exists("TestName", .STEnv)) {
    .STEnv$TestName <- make.names(x)
    return(invisible(x))
  }
  .STEnv$TestName
}


versioncode <- function() {
  .STEnv$ExamNum[testversion()]
}

LastIndices <- function(x = NULL) {
  if (!missing(x) || !exists("LastIndices", .STEnv)) {
    .STEnv$LastIndices <- x
    return(invisible(x))
  }
  .STEnv$LastIndices
}

