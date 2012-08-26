MergeClassList <- function(classlist,scanex,weights=rep(1,200),sections,TestTitle, MasterName = "MasterClassList",AcceptSectionCSV = TRUE, AcceptMasterCSV = FALSE){

###Grade the tests###
grade.test <- grades(scanex, correct=weights)

###Use mergeLists function to append result of "grade.test" function to class list###
classlist <- mergeLists(classlist, grade.test, c("Student..", "Student ID"))

###Get rid of unnecessary columns from merger###
drop <- which(names(classlist) %in% c("Section.y", "Marker"))
classlist <- classlist[,-drop]

###Check for NAs and send messages to user###

###If Student ID is in Update list, but not Master, return Student ID
###Section Number, and Scantron Number to user, as filled out on scantron card
###For analysis by the instructor

if (nrow(classlist[is.na(classlist$"User.ID"),])>0){

print("The following Student ID was not found in your class list, please check the scantron card as indicated in the following printout")
print(classlist[is.na(classlist$"User.ID"),])

}

###If Student ID is in Master, but not in Update List, return Student ID, User ID
###Section Number, First Name, and Last Name to User for analysis by instructor

if(nrow(classlist[is.na(classlist$"Grade"), c(1,2,3,4,5)])>0){

print("The following students do not have grades recorded for this test, please review to ensure accuracy")
print(classlist[is.na(classlist$"Grade"), c(1,2,3,4,5)])

}

###Change NAs into Blanks###
for (i in seq_len(ncol(classlist)))
    classlist[i][is.na(classlist[i])] <- ""

###Put students into proper order###
order.class <- with(classlist, order(Section.x, Last.Name, First.Name, na.last = FALSE))
classlist <- classlist[order.class,]

###Rename columns that may have been changed###
names(classlist) <- sub("Section.x", "Section", names(classlist))
names(classlist) <- sub("Student..", "Student #", names(classlist))
names(classlist) <- sub("Last.Name", "Last Name", names(classlist))
names(classlist) <- sub("First.Name", "First Name", names(classlist))
names(classlist) <- sub("User.ID", "User ID", names(classlist))

###Rename Columns Merged into Master###
Names <- "0"
Names[1] <- paste("ExamCode", TestTitle, sep="")
Names[2] <- paste("Scantron", TestTitle, sep="")
Names[3] <- paste("StudentAnswers", TestTitle, sep="")
Names[4] <- paste("CorrectAnswers", TestTitle, sep="")
Names[5] <- paste("Grade", TestTitle, sep="")

start <- which(names(classlist) %in% c("ExamCode"))
end <- start + 4
for(i in start:end){
	colnames(classlist)[i] <- Names[i-start+1]
}

###Write CSV Files###

###Master CSV###

master <- paste(MasterName, ".csv", sep="")

if(AcceptMasterCSV == TRUE){
	write.csv(classlist, master, row.names=FALSE, quote=FALSE)
}

if(AcceptMasterCSV == FALSE){
print("If you would like a master class file created, please set the AcceptMasterCSV indicator equal to TRUE")
}

###Individual Section CSV###

if (AcceptSectionCSV == TRUE){
	for (i in 1:length(sections)){
	section.name <- paste("Section", sections[i], TestTitle,".csv", sep="")
	classlist.section <- classlist[classlist$Section == sections[i],]
	write.csv(classlist.section, section.name, row.names=FALSE, quote=FALSE)
	}
}
if(AcceptSectionCSV == FALSE){
print("If you would like individual CSV files each section, please set the AceeptSectionCSV indicator equal to TRUE")
}
}
