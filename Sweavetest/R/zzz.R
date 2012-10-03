perms <- list()
perms[[2]] <- list(c(1,2), c(1,2), c(2,1), c(2,1))
perms[[3]] <- list(c(1,2,3), c(2,3,1), c(3,1,2), c(1,3,2))
perms[[4]] <- list(c(1,2,3,4), c(2,1,4,3), c(4,3,2,1), c(3,4,1,2))
perms[[5]] <- list(c(1,2,3,4,5), c(2,3,4,5,1), c(3,4,5,1,2),  c(5,2,1,3,4))
  
.STEnv <- new.env(parent=emptyenv())

.onLoad <- function(lib, pkg)
{
  .STEnv$ExamNum <- sort(sample(100:999, 4))
}