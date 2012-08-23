version <- function(x = "Student"){
  x <- match.arg(x,c("Teacher",  "Student", "Report"))
  Version <<- x
}