# Write out LaTeX for nominal roll

nominalRoll <- function(course, date, rooms, institution="THE UNIVERSITY OF WESTERN ONTARIO",
                        file = "nominal.tex") {
    out <- file("nominal.tex", "wt")
    on.exit(close(out))
    cat('
\\documentclass[12pt]{article}

\\usepackage{fancyhdr}

\\renewcommand{\\arraystretch}{1.4} 
\\setlength{\\oddsidemargin}{-0.5in}
\\setlength{\\evensidemargin}{-0.5in}
\\setlength{\\textwidth}{7.5in}
\\setlength{\\topmargin}{-0.5in}
\\setlength{\\textheight}{9in}
\\pagestyle{fancy}

\\lhead{', institution, '}
\\rhead{NOMINAL ROLL}
\\cfoot{}

\\begin{document}
', file=out)

for (room in names(rooms)) {
  for (row in seq_len(rooms[room])) {
    
    cat('\\noindent ', course, '\\hfill', date, '\\hfill', room, 
'\\vspace{0.2in}   \\\\
Proctor: \\underline{\\hfill}
\\vspace{0.2in} \\\\
Row number in exam room: ', row, '\\\\
\\ \\\\
\\vspace{0.2in}\\begin{tabular}{|c|c|c|c|c|c|}
\\hline
Seat & Print name & Signature & Student No. & ID & EXAM \\\\
     & \\hspace{1.8in}   & \\hspace{1.8in}   & \\hspace{1.8in}     & chk & recd  \\\\   
\\hline
', file=out)
    for (i in 1:20)
      cat(i, ' & & & & & \\\\
\\hline
', file=out)
    cat('
\\end{tabular}\\\\
\\vspace{0.1in}Note irregularities here:
\\newpage', file=out)
  }
}
cat('\\end{document}', file=out)
}

# Test code:

# nominalRoll("SS 1024A", "October 17, 2011", c("WSC 248"=1))