
getglobal <- function(var, default) {
  name <- deparse(substitute(var))
  if (exists(name, .STEnv)) var <- get(name, .STEnv)
  else var <- default
  var
}

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

versioncode <- function() {
  .STEnv$ExamNum[testversion()]
}
