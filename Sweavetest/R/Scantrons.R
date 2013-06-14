readScanex <- function(filename) {
    result <- read.fwf(filename,
             widths=c(9,-1,3,-1,3,2,-1,4,-1,180),
    	     col.names=c("Student ID", "Section", "ExamCode", "Sheet", "Scantron", "Answers"),
    	     as.is = TRUE, colClasses="character", check.names=FALSE)
    nonblank <- sub("[[:blank:]]*$", "", result$Answers)
    maxlen <- max(nchar(nonblank))
    result$Answers <- substr(result$Answers, 1, maxlen)
    result
}

grades <- function(scanex, key=scanex[scanex$"Student ID" == "999999999",],
                   correct = 1, blank = 0, wrong = 0) {
    key <- key[!is.na(key$ExamCode),]
    numquestions <- nchar(key$Answers[1])
    correct <- rep(correct, len=numquestions)
    blank <- rep(blank, len=numquestions)
    wrong <- rep(wrong, len=numquestions)
    
    result <- cbind(scanex[scanex$"Student ID" != "999999999",], Correct="", Grade=NA,
                    stringsAsFactors = FALSE)
    for (i in seq_len(nrow(result))) {
	Correct <- key[ key$ExamCode == result$ExamCode[i], ]
	if (nrow(Correct) < 1) warning("Bad ExamCode:", paste(result[i,], collapse=" "))
	else {
	    Correct <- gsub(" *$", "", Correct$Answers)
	    Student <- gsub(" *$", "", result$Answers[i])
	    result$Answers[i] <- Student
	    Grade <- 0
	    for (j in seq_len(nchar(Correct[1]))) {
	        answer <- substr(Student, j, j)
	        if (answer %in% c(" ", ""))
	            Grade <- Grade + blank[j]
	        else if (any(answer == substr(Correct, j, j))) {
	            Grade <- Grade + correct[j]
	            substr(Correct[1], j, j) <- answer
	        } else 
	            Grade <- Grade + wrong[j]
	    }
	    result$Correct[i] <- Correct[1]	    
	    result$Grade[i] <- Grade
	}
    }
    result
}

# This is cheater detection code:  remark each wrong answer using one of the
# other keys

wrongKey <- function(scanex, key=scanex[scanex$"Student ID" == "999999999",]) {
    key <- key[!is.na(key$ExamCode),]
    result <- cbind(scanex[scanex$"Student ID" != "999999999",], Grade=NA, Badcount=NA, Badkey=NA, Badcode=NA)
    for (i in seq_len(nrow(result))) {
	whichkey <- which( key$ExamCode == result$ExamCode[i] )
	Correct <- key[whichkey, ]
	Wrongkeys <- key[-whichkey, ]
	if (nrow(Correct) < 1) warning("Bad ExamCode:", paste(result[i,], collapse=" "))
	else {
	    Correct <- gsub(" *$", "", Correct$Answers)
	    Student <- gsub(" *$", "", result$Answers[i])
	    Wrongcodes <- Wrongkeys$ExamCode
	    Wrongkeys <- gsub(" *$", "", Wrongkeys$Answers)
	    
	    Grade <- 0
	    Badcount <- rep(0, length(Wrongkeys)) 
	    for (j in seq_len(nchar(Correct))) {
	        answer <- substr(Student, j, j)
	        if (answer %in% c(" ", "")) {
	             # ignore 
	        } else if (any(answer == substr(Correct, j, j)))
	            Grade <- Grade + 1
	        else for (k in seq_along(Wrongkeys))
	            if (answer == substr(Wrongkeys[k], j, j)){
	            	Badcount[k] <- Badcount[k] + 1
	            	break
	            }
	    }
	    result$Grade[i] <- Grade
	    result$Badcount[i] <- max(Badcount)
	    result$Badkey[i] <- Wrongkeys[which.max(Badcount)]
	    result$Badcode[i] <- Wrongcodes[which.max(Badcount)]
	}
    }
    result
}

mergeLists <- function(master, update, key="Student ID", full=TRUE) {
    key <- rep(key, len=2)
    both <- merge(master, update, all=TRUE, by.x=key[1], by.y=key[2])
    if (!full) {
    	result <- merge(master, update, all.x=TRUE, by.x=key[1], by.y=key[2])
    	
    	if (nrow(both) > nrow(result)) warning(nrow(both)-nrow(result),
    	                                      " unmatched rows in the update data")
    } else result <- both	   
    result
}

writeScanex <- function(name) {
    index <- read.csv(paste0(name, "Index.csv"))
    examcodes <- unique(index$ExamCode)
    qs <- as.numeric(unique(index$Question))
    out <- file(paste0(name, "Correct.dat"), "w")
    on.exit(close(out))
    
    for (examcode in examcodes) {
      correct <- rep(" ", max(qs))
      qs <-  index$Question[ index$ExamCode == examcode ]
      cor <- index$Correct[ index$ExamCode == examcode ]
      perm <- index[ index$ExamCode == examcode, LETTERS[1:5], drop=FALSE ]
      for (j in seq_along(qs))
        correct[ qs[j] ] <- LETTERS[ which(perm[j,] == cor[j]) ]
      cat(file=out, paste0("999999999|   :", examcode, "* [0000]", paste(correct, collapse=""), "\n"))
    }
}      
      