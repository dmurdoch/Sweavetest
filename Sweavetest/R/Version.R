version <- function(x = "Student"){
  x <- match.arg(x,c("Teacher",  "Student", "Report", "Low Level"))
  .STEnv$Version <- x
}