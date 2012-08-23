newCommands <- function(Cov=TRUE, Var=TRUE, E=TRUE, Lik=TRUE, lik=TRUE, 
                        Binom=TRUE, Exp=TRUE, Poisson=TRUE, Unif=TRUE,
			X=TRUE, x=TRUE, Y=TRUE, y=TRUE,
                        F=TRUE, G=TRUE, vs=TRUE,
                        Marks=TRUE, thesection=TRUE,
                        lowtilde=TRUE) {
  if (Cov)
    cat("\\newcommand{\\Cov}{\\mbox{Cov}}\n")
  if (Var)
    cat("\\newcommand{\\Var}{\\mbox{Var}}\n")
  if (Binom)
    cat("\\newcommand{\\Binom}{\\mbox{Binom}}\n")
  if (Exp)
    cat("\\newcommand{\\Exp}{\\mbox{Exp}}\n")
  if (Poisson)
    cat("\\newcommand{\\Poisson}{\\mbox{Poisson}}\n")	
  if (Unif)
    cat("\\newcommand{\\Unif}{\\mbox{Unif}}\n")
  if (E)
    cat("\\newcommand{\\E}{\\mbox{E}}\n")
  if (Lik)
    cat("\\newcommand{\\Lik}{{\\cal L}}\n")
  if (lik)
    cat("\\newcommand{\\lik}{{\\it l}}\n")
  if (X)
    cat("\\newcommand{\\X}{\\lowtilde{X}}\n")
  if (x)
    cat("\\newcommand{\\x}{\\lowtilde{x}}\n")
  if (Y) 
    cat("\\newcommand{\\Y}{\\lowtilde{Y}}\n")
  if (y)
    cat("\\newcommand{\\y}{\\lowtilde{y}}\n")
  if (F)
    cat("\\newcommand{\\F}{{\\cal F}}\n")
  if (G)
    cat("\\newcommand{\\G}{{\\cal G}}\n")
  if (vs)
    cat("\\newcommand{\\vs}{\\hspace{2em}\\mbox{vs}\\hspace{2em}}\n")
  if (Marks)
    cat("\\newcommand{\\Marks}[1]{\\marginpar{/#1}}")
  if (thesection)
    cat("\\renewcommand{\\thesection}{}\n")
  if (lowtilde)
    cat("\\newcommand{\\lowtilde}[1]{\\mathop{#1}\\limits_{\\textstyle\\tilde{}}}\n")
}

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
