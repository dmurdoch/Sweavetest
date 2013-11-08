\name{LongAnswer}
\alias{LongAnswer}

\title{
Contains all the funcitonality to include long answer questions within a multiple choice test.
}
\description{
This function is utilized to first format long answer questions, including long answer questions with multiple parts. Also contains functionality to analyze the results of the long answer question if the appropriate file containing the marks is provided.
}
\usage{
LongAnswer(Question, Answer, Marks = 0, report = TRUE, Part = 0, Length = c("Sentence", "Paragraph", "Page"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Question}{
    This argument takes the desired question from the instructor as a character string. This argument is optional for long answer questions that        are not multi-part, but must contain a question if the long answer question is multi-part.
    }
  \item{Answer}{
A completely optional character string input representing the answer to the input Question above.
}
  \item{Marks}{
The total number of marks available in this part of the long answer question
}
  \item{report}{
  Default value of true indicates that when a report is created, this question will be analyzed.
}
  \item{Part}{
This input takes on a value of 0 if this question is a single part question. If this question is to have multiple parts, then this input should be equal to 1,2,... depending on what part of the question the instructor is on.
}
  \item{Length}{
The size of spacing left to answer the question.
}
}

\value{
This function returns a completely formatted question when a student or teacher version is run. When a report is run, it re-formats the original question, formats an answer (if supplied), and analyzes the question utilizing a descriptive statistics table, score distribution table, and empirical probability plot.
}
\author{
Adam Rahman
}

\examples{
##Single Part Question

LongAnswer(Marks = 5, report = TRUE, Part = 0, Length = "Sentence")

##Multi-Part Question

#Part A

LongAnswer(Question = "State the Central Limit Theorem using no mathematical notation", Answer = "As the sample size n increases, the distribution of the sample mean converges to a normal distribution...", Marks = 5, report = TRUE, Part = 1, Length = "Sentence")

#Part B

LongAnswer(Question = "State the Law of Large Number using no mathematical notation", Answer = "", Marks = 5, report = TRUE, Part = 2, Length = "Sentence")

}

