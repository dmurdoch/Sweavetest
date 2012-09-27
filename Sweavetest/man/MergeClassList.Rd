\name{MergeClassList}
\alias{MergeClassList}
\title{
Merge information from graded tests to pre-existing class list
}
\description{
MergeClassList should be used after analysis of the test has been done. After appropriate analysis has
been completed, this function will re-grade the test with the desired weights and will merge five columns to the existing class list.
These columns are the test exam code, the Scantron code, the student's answers, the corresponding correct answers, and finally
the student's grade on the test.

This function also allows the user to choose to export these newly merged class lists as CSV files. The user can choose to export a Master Class List, which will be a
class list which includes students from all sections of the course. The user can also choose to export one CSV file per section of the course.

}
\usage{
MergeClassList(classlist, scanex, weights = rep(1, 200), sections, TestTitle, 
  MasterName = "MasterClassList", AcceptSectionCSV = TRUE, AcceptMasterCSV = FALSE)
}
\arguments{
  \item{classlist}{
      The object 'classlist' should be entered as a CSV file imported into R through the function read.csv(). The classlist object must include the following columns: 'Student #', 'Section', 'Last Name', 'First Name', 'User ID'. These columns should be the first five columns in the classlist file, but do not necessarily need to be in the order given. If these columns do not exist, an error message will be produced so the user can make the appropriate changes.
}
  \item{scanex}{
      is the result of running the function readScanex(). The readScanex() function should only be used with scantron files received by the instructor from the IT department.
}
  \item{weights}{
      is a vector of numeric values (generally 1s) corresponding to the weights the instructor wishes to place on one of the test questions. There should be one value per question in the weight vector. The default for this input is a vector of 1s.
}
  \item{sections}{
      is a character vector corresponding to the sections that are found in the 'Section' column of the classlist CSV.
}
  \item{TestTitle}{
      is a character input which will allow the instructor to distinguish the current test from all other tests.
}
  \item{MasterName}{
      is a character input corresponding to the name the instructor wishes to give to the Master Class List exported by this function. the file is exported as a .csv file into the current working directory. The default for this input is 'MasterClassList'.
}
  \item{AcceptSectionCSV}{
      takes either TRUE or FALSE as values, as specified by the user. A value of TRUE will allow this function to export CSV files for each section given in the 'section' input. A value of FALSE will force the function to skip over creating the individual section CSV files, and will also send a message to the user informing them of their choice. The default value for this input is TRUE.
}
  \item{AcceptMasterCSV}{
     is entered as either TRUE or FALSE, as specified by the user. A value of TRUE will allow the function to export a master CSV file, containing all sections present in the 'section' input. A value of FALSE will force the function to skip creating the master CSV file, and will also send a message to the user informing them of their choice. The default value for this input is FALSE.
}
}
\value{
This function returns CSV files to the working directory; one for each section of the class list (if the user desires) and one master file containing all students across all sections (if the user desires).

If the column titles in classlist do not conform to what the function requires, an error message is returned so that the user can make the appropriate adjustments to the class list file.

If during the merger process, one or more student numbers do not match, the function will still execute and export the CSV files. However, all of the information on the mismatched Student ID will be returned to the user so that further investigation of the student can be completed.

If the user does not choose to export both Master and Section CSV files, this function will return messages to the user with instructions on how to export these files if they desire.
}
\author{
Duncan Murdoch and Adam Rahman
}
\note{
For this function to execute properly, a working directory must be specified by the user. The function will export the CSV files chosen by the user to the specified directory.
}
\examples{
\donttest{
#For a single class list
library(Sweavetest)																						#This package contains the MergeClassList Function
library(tables)																							#This package contains functions used with MergeClassList
setwd(''C:/User/Courses/SS1024'')																		#Sets the path from which files are read from and exported to
classlist <- read.csv("ss1024A-001.csv", check.names=FALSE, as.is=TRUE,colClasses="character")			#class list read into R
scanex <- readScanex(''SS1024A-001.dat'')																#Scantron File read into R
weights <- rep(1,50)																					#50 Questions present in 'scanex' worth 1 mark each
sections <- c(''1'')																					#One section present in class list
MergeClassList(classlist,scanex,weights, sections, TestTitle=''Test1'')									#Execution of function

#For multiple class lists
library(Sweavetest)																						#This package contains the MergeClassList Function
library(tables)																							#This package contains functions used with MergeClassList
setwd(''C:/User/Courses/SS1024'')																		#Sets the path from which files are read from and exported to
class1 <- read.csv("ss1024A-001.csv", check.names=FALSE, as.is=TRUE,colClasses="character")				#class list read into R
class2 <- read.csv("ss1024A-002.csv", check.names=FALSE, as.is=TRUE,colClasses="character")				#Second class list is read into R
class3 <- read.csv("ss1024A-003.csv", check.names=FALSE, as.is=TRUE,colClasses="character")				#Third class list is read into R
classlist <- rbind(class1, class2, class3)																#All 3 class lists are bound into one Master List
scanex <- readScanex(''SS1024A-001.dat'')																#Scantron File read into R
weights <- rep(1,50)																					#50 Questions present in 'scanex' worth 1 mark each
sections <- c(''1'')																					#One section present in class list

MergeClassList(classlist,scanex,weights, sections, TestTitle=''Test1'', AcceptSectionCSV = TRUE, AcceptMasterCSV = TRUE)			#Execution of function
}
}
