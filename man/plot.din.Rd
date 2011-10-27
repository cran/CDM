\name{plot.din}

\alias{plot.din}

\title{
  Plot Method for Objects of Class din
}

\description{
  S3 method to plot objects of the class \code{din}.  
}

\usage{
  \method{plot}{din}(x, items = c(1:ncol(x$data)), pattern = "",
    uncertainty = 0.1, highest = 0.05, pdf.file = "", 
    hide.idi = FALSE, hide.obs = FALSE,
    display.nr = 1:5, \dots)
}

\arguments{
  \item{x}{a required object of class \code{din}, obtained from a
    call to the function \code{\link{din}}.  }
    
  \item{items}{an index vector giving the items to be visualized in the first
    plot, see \sQuote{Details}.  The default is 
    \code{items = c(1:ncol(x$data))}, which is all items.  }
	  
  \item{pattern}{an optional character specifying a response pattern
    of an respondent, whose attributes are analyzed in a separate
    graphic.  It is required to choose a pattern from the empirical
    data set.  }
	
  \item{uncertainty}{a numeric between 0 and 0.5 giving the
    uncertainty bounds for deriving the observed skill occurrence probabilities 
    in plot 2 and the simplified deterministic attribute profiles in plot 5.  }

  \item{highest}{a numeric between 0 and 1 giving the percentage 
    of skill patterns with highest occurrence frequency to be
    labeled in plot 3.  }
    
  \item{pdf.file}{an optional character string. If specified the graphics 
    obtained from the function \code{plot.din} are provided in a pdf file.  
    The default is \code{pdf.file=""}, which is not providing a pdf file.  
    Otherwise specify a directory and filename ending with \code{.pdf} where to 
    write the document.  }
  
  \item{hide.idi}{an optional logical value. If set to \code{TRUE}, the IDI 
    curve in first graphic is not displayed.   }

  \item{hide.obs}{an optional logical value. If set to \code{TRUE}, the 
    polygonal chain for observed frequencies of skill class probabilities in the 
    second graphic is not displayed.  }
    
  \item{display.nr}{an optional numeric or numeric vector. If specified, only the plots in
    \code{display.nr} are displayed. Default is \code{display.nr = 1:5} causing the display of 
    all five plots.   }
  
  \item{\dots}{Optional graphical parameters to be passed to or from other
    methods will be ignored.  }
}

\details{
  The \code{plot} method graphs the results obtained from a CDM analysis.  
  Four or five graphics to analyze the fitted model are produced, respectively.  
  
  The first graphic depicts the parameter estimates their diagnostic accuracy
  for each of chosen the items in \code{items}.  Parameter estimates are 
  splitted in guessing and slipping errors for each item.  See \code{\link{din}}
  for further information.  
  
  The second graphic shows the estimated occurrence probabilities of the attributes
  underlying the items.  The included polygonal line gives the observed occurrence 
  probabilities in the sample.  They are determined by evaluation of the simplified
  deterministic attribute profiles.  Classifying a respondent as master of a certain 
  attribute increases the observed occurrence probability of that attribute.  
  
  The third graphic illustrates the distribution of the skill pattern occurrence
  probabilities.  The skill patterns with the \code{highest} occurrence
  frequency are labeled.  
  
  The forth plot is a parallel coordinate plot of the individual skill profiles.
  Each line represents an individual skill profile.  For each of these skill profiles
  on the vertical lines the individual probabilities of mastering the corresponding
  attributes are drawn.  
  
  If in \code{pattern} an empirical response pattern is specified, the fifth plot
  shows the individual skill profile of an examinee having this response pattern.
  For each attribute, having a mastering probability below \eqn{0.5 - uncertainty}
  the examinee is classified as non-master of the corresponding attribute.  For
  mastering probabilities higher than \eqn{0.5 + uncertainty} the examinee is
  classified as master of the corresponding attribute.  
}

\value{
  If the argument \code{x} is of required type,
  and if the optional arguments \code{items}, \code{uncertainty},
  \code{highest} and \code{pdf.file} are specified as required, the
  \code{plot.din} produces several graphics to analyze a CDM model.  
}

\author{
  Alexander Robitzsch, Thomas Kiefer, Ann Cathrice George, Ali Uenlue 

  Maintainer: Alexander Robitzsch <a.robitzsch@bifie.at>,
              Thomas Kiefer <t.kiefer@bifie.at>,
              Ann Cathrice George <george@statistik.tu-dortmund.de>,
              Ali Uenlue <uenlue@statistik.tu-dortmund.de>
}

\references{
  Rupp, A. A., Templin, J. L. and Henson, R. A. (2010) \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}. New York: The Guilford
  Press.
}

\seealso{
  \code{\link{print.din}}, the S3 method for printing objects of
  the class \code{din}; \code{\link{summary.din}}, the S3
  method for summarizing objects of the class \code{din}, which
  creates objects of the class \code{summary.din};
  \code{\link{print.summary.din}}, the S3 method for printing
  objects of the class \code{summary.din}; \code{\link{din}},
  the main function for DINA and DINO parameter estimation, which
  creates objects of the class \code{din}.  See also \code{\link{CDM-package}}
  for general information about this package.
}

\examples{
##
## (1) examples based on dataset fractions.subtraction.data
##

## Set an upper boundary for the guessing parameter of 
## item 5, 8 and 9
fractions.dina.bound <- din(data = fraction.subtraction.data, 
  q.matrix = fraction.subtraction.qmatrix, constraint.guess =
  matrix(c(5,8,9, rep(0.2, 3)), ncol = 2))
fractions.dina.bound

## The second plot shows the expected (MAP) and observed skill 
## probabilities. The third plot visualizes the skill pattern
## occurrence probabilities; Only the 'highest' are labeled; it
## is obvious that the skill class '11111111' (all skills are
## mastered) is the most probable in this population. The fourth
## plot shows the skill probabilities conditional on response
## patterns; in this population the skills 3 and 6 seem to be
## mastered easier than the others. The fifth plot shows the
## skill probabilities conditional on a specified response
## pattern; it is shown whether a skill is mastered (above 
## .5+'uncertainty') unclassifiable (within the boundaries) or
## not mastered (below .5-'uncertainty'). In this case, the
## fifteenth respondent was chosen; if no response pattern is 
## specified, the plot will not be shown (of course)
pattern <- paste(fraction.subtraction.data[15,], collapse = "")

#uncertainty = 0.1, highest = 0.05 are default
plot(fractions.dina.bound, uncertainty = 0.1, highest = 0.05, 
  pattern = pattern)
}

\keyword{methods}
\keyword{print}