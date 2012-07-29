\name{fraction.subtraction.data}

\alias{fraction.subtraction.data}
\docType{data}

\title{
  Fraction Subtraction Data
}

\description{
  \cite{Tatsuoka's (1984)} fraction subtraction data set is comprised of 
  responses to \eqn{J=20} fraction subtraction test items from \eqn{N=536} 
  middle school students.  
}

\usage{
  data(fraction.subtraction.data)
}

\format{
  The \code{fraction.subtraction.data} data frame consists of 536
  rows and 20 columns, representing the responses of the \eqn{N=536} 
  students to each of the eqn{J=20} test items.  Each row in the data set
  corresponds to the responses of a particular student.  Thereby a "1"
  denotes that a correct response was recorded, while "0" denotes an
  incorrect response.  The other way round, each column corresponds
  to all responses to a particular item.  
}

\details{
  The items used for the fraction subtraction test originally appeared
  in \cite{Tatsuoka (1984)} and are published in \cite{Tatsuoka (2002)}.  They 
  can also be found in \cite{DeCarlo (2011)}.  All test items are based on 8 
  attributes (e.g. convert a whole number to a fraction, separate a whole 
  number from a fraction or simplify before subtracting).  The complete 
  list of skills can be found in \code{\link{fraction.subtraction.qmatrix}}.  
}

\source{
  The Royal Statistical Society Datasets Website, Series C, 
  Applied Statistics, Data analytic methods for latent partially
  ordered classification models:\cr
  URL: http://www.blackwellpublishing.com/rss/Volumes/Cv51p2_read2.htm
}

\references{
  DeCarlo, L. T. (2011) On the Analysis of Fraction Subtraction Data:  
  The DINA Model, Classification, Latent Class Sizes, and the Q-Matrix.
  \emph{Applied Psychological Measurement}, \bold{35}, 8--26.
  
  Tatsuoka, C. (2002) Data analytic methods for latent partially ordered classification 
  models. \emph{Journal of the Royal Statistical Society, Series C, Applied Statistics}, 
  \bold{51}, 337--350.
  
  Tatsuoka, K. (1984) \emph{Analysis of errors in fraction addition and subtraction 
  problems}. Final Report for NIE-G-81-0002, University of Illinois, Urbana-Champaign.
}

\seealso{
  \code{\link{fraction.subtraction.qmatrix}} for the corresponding Q-matrix.  
}

\examples{
##
## (1) examples based on dataset fractions.subtraction.data
##

## dataset fractions.subtraction.data and corresponding Q-Matrix
fraction.subtraction.data
fraction.subtraction.qmatrix

## Misspecification in parameter specification for method din()
## leads to warnings and terminates estimation procedure. E.g.,

# See Q-Matrix specification
fractions.dina.warning1 <- din(data = fraction.subtraction.data,
  q.matrix = t(fraction.subtraction.qmatrix)) 
  
# See guess.init specification
fractions.dina.warning2 <- din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, guess.init = rep(1.2,
  ncol(fraction.subtraction.data)))
  
# See rule specification   
fractions.dina.warning3 <- din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = c(rep("DINA",
  10), rep("DINO", 9)))

## Parameter estimation of DINA model
# rule = "DINA" is default
fractions.dina <- din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = "DINA")
fractions.dina # see print.din

attributes(fractions.dina)
str(fractions.dina)	  

## For instance accessing the guessing parameters through
## assignment
fractions.dina$guess

## corresponding summaries, including diagnostic accuracies,
## most frequent skill classes and information 
## criteria AIC and BIC
summary(fractions.dina)

## In particular, accessing detailed summary through assignment
detailed.summary.fs <- summary(fractions.dina)
str(detailed.summary.fs)


## Item discrimination index of item 8 is too low. This is also
## visualized in the first plot 
plot(fractions.dina)

## The reason therefore is a high guessing parameter
round(fractions.dina$guess[,1], 2)

## Fix the guessing parameters of items 5, 8 and 9 equal to .20
# define a constraint.guess matrix
constraint.guess <-  matrix(c(5,8,9, rep(0.2, 3)), ncol = 2)
fractions.dina.fixed <- din(data = fraction.subtraction.data, 
  q.matrix = fraction.subtraction.qmatrix, 
  constraint.guess=constraint.guess)

## The second plot shows the expected (MAP) and observed skill 
## probabilities. The third plot visualizes the skill classes
## occurrence probabilities; Only the 'top.n.skill.classes' most frequent 
## skill classes are labeled; it
## is obvious that the skill class '11111111' (all skills are
## mastered) is the most probable in this population. The fourth
## plot shows the skill probabilities conditional on response
## patterns; in this population the skills 3 and 6 seem to be
## mastered easier than the others. The fifth plot shows the
## skill probabilities conditional on a specified response
## pattern; it is shown whether a skill is mastered (above 
## .5+'uncertainty') unclassifiable (within the boundaries) or
## not mastered (below .5-'uncertainty'). In this case, the
## 527th respondent was chosen; if no response pattern is 
## specified, the plot will not be shown (of course)
pattern <- paste(fraction.subtraction.data[527, ], collapse = "")
plot(fractions.dina, pattern = pattern, display.nr = 5)

#uncertainty = 0.1, top.n.skill.classes = 6 are default
plot(fractions.dina.fixed, uncertainty = 0.1, top.n.skill.classes = 6, 
  pattern = pattern)
}

\keyword{datasets}
