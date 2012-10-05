DescriptiveStatistics <- function(Percentage, Section) { 
  ###Produce Descriptive Statistics Table###

  Mean <- function(x) mean(x)
  Quantile25 <- function(x) quantile(x, .25)
  Quantile75 <- function(x) quantile(x, .75)
  Median <- function(x) quantile(x, .50)
  Max <- function(x) max(x)
  Min <- function(x) min(x)
  StdDev <- function(x) sqrt(var(x)) 

  pct <- function(x) 100*mean(x)/numQ

  ###Produce Student Count Table and Test Means Tab;e###

  latex(tabular((Factor(Section)+1)*Heading()*Percentage ~ Mean + StdDev + Max + Quantile75
                        + Median + Quantile25 + Min), digits=2)
}
