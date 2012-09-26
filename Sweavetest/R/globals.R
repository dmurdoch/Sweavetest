
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

versioncode <- function() {
  .STEnv$ExamNum[testversion()]
}
