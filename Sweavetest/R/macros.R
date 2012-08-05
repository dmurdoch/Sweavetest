newCommands <- function() 
cat("
\\newcommand{\\Var}{\\mbox{Var}}
\\newcommand{\\Cov}{\\mbox{Cov}}
\\newcommand{\\Unif}{\\mbox{Unif}}
\\newcommand{\\E}{\\mbox{E}}
\\newcommand{\\Lik}{{\\cal L}}
\\newcommand{\\lik}{{\\it l}}
\\renewcommand{\\thesection}{}
\\newcommand{\\X}{\\lowtilde{X}}
\\newcommand{\\x}{\\lowtilde{x}}
\\newcommand{\\Y}{\\lowtilde{Y}}
\\newcommand{\\g}{g(\\cdot)}
\\newcommand{\\F}{{\\cal F}}
\\newcommand{\\G}{{\\cal G}}
\\newcommand{\\vs}{\\hspace{2em}\\mbox{vs}\\hspace{2em}}
\\newcommand{\\Marks}[1]{\\marginpar{/#1}}
")

marklist <- function(marks, names=1:length(marks), testversion) {
  testversion <- getglobal(testversion, 1)
cat("\\begin{minipage}[t]{2in}
Marks (exam code ", versioncodes[testversion], ") \\\\
\\ \\vspace{0.5ex} \\\\
\\begin{tabular}{|c|c|}
\\hline
", sep="")
for (i in seq_along(marks)) 
  cat(names[i], " & \\hspace{0.5in}/", marks[i], " \\\\\n", sep="")         
cat("\\hline
Total & \\hspace{0.5in}/", sum(marks), "  \\\\
\\hline
\\end{tabular}
\\end{minipage}
", sep="")
}

multiplechoice <- function(names) {
cat("
\\begin{minipage}[t]{2in}
Multiple choice answers \\\\
\\ \\vspace{0.5ex} \\\\
\\begin{tabular}{|rc|rc|}
\\hline
")
n <- length(names)
nby2 <- (n + 1) %/% 2
left <- names[1:nby2]
if (n > 1)
  right <- names[(nby2+1):n]
else
  right <- character(0)
if (length(right) < length(left))
  right <- c(right, "")

for (i in seq_along(left)) 
cat(left[i], " & \\hspace{0.5in} & ", right[i], "& \\hspace{0.5in} \\\\
  &                &   &      \\\\
")
cat("\\hline
\\end{tabular}
\\end{minipage}
")
}
