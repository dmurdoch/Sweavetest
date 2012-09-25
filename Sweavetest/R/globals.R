
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

versioncode <- function() {
  .STEnv$ExamNum[testversion()]
}
