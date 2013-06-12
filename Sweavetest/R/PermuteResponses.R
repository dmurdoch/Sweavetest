PermuteResponses <-
function(len, Correct=1, KeepLast=0) {
  
  n <- len - KeepLast  
  QuestionIndex(c(QuestionIndex(),QuestionCounter()))	 
  for (tv in seq_len(NumVersions()+1)-1) {  # 0 to NumVersions()
    rand <- sample(n)  # Leave this here in case randomization is only temporarily off
    if (tv) 
      indices <- c(if (n) perms[[n]][[tv]][rand], n+seq_len(KeepLast))
    else
      indices <- seq_len(len)
    
    if(length(indices)<5){		 			 
      indices <- c(indices,rep(NA,5-length(indices)))						 
    }	
    
    if (tv && Version() != "Report" && !is.na(Correct))
      Index(rbind(Index(),data.frame(Question=QuestionCounter(), 
  			       ExamCode=VersionCodes()[tv],
  			       Correct=Correct,
  			       A=indices[1],
  			       B=indices[2],
  			       C=indices[3],
  			       D=indices[4],
  			       E=indices[5])))	
  }
}

getPerm <- function(na.rm = TRUE, code = versioncode(), q = QuestionCounter(), rand = randomize(), index = Index()) {
  
  perm <- with(index, index[ExamCode == (if (code == "MASTER") VersionCodes()[1] else code) & Question == q,LETTERS[1:5]])
  if (code == "MASTER" || !rand) {
    n <- sum(!is.na(perm))
    perm[1:n] <- 1:n
  }
  if (na.rm) perm[!is.na(perm)] else perm
}
