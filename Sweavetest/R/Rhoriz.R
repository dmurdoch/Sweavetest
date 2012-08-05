Rhoriz <-
function(..., Correct=1, KeepLast=0) {
  x <- unlist(list(...))
  x <- paste("\\verb!", x, "!")
  if (!is.na(Correct))
    x[Correct] <- paste("\\Correct", x[Correct])
  horiz(x, Correct=NA, KeepLast=KeepLast)
}

