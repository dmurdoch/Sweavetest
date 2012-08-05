clean <-
function(lines, stripleading=TRUE) {
  result <- paste(lines, collapse="\n")
  if (stripleading) result <- sub("^ *","",result)
  result
}

