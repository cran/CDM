\name{print.summary.din}

\alias{print.summary.din}

\title{
  Print Method for Objects of Class summary.din
}

\description{
  S3 method to print objects of the class \code{summary.din}.  
}

\usage{
  \method{print}{summary.din}(x, \dots)
}

\arguments{
  \item{x}{a required object of class \code{summary.din}, obtained
    from a call to the function \code{\link{summary.din}} (through
    generic function \code{\link[base]{summary}}).  }
    
  \item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.  }
}

\details{
  The \code{print} method prints the summary information about objects
  of the class \code{din} computed by \code{\link{summary.din}}, 
  which are the diagnostic accuracies of the items, a summary of 
  the corresponding skill pattern distribution and the model information
  criteria AIC and BIC.  Specific summary information details such as
  individual items with their diagnostic accuracy can be accessed through
  assignment (see \sQuote{Examples}).  
}

\value{
  If the argument \code{x} is of required type,
  \code{print.summary.din} prints the afore mentioned summary
  information in \sQuote{Details}, and invisibly returns \code{x}.  
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
  \code{\link{plot.din}}, the S3 method for plotting objects of
  the class \code{din}; \code{\link{print.din}}, the S3 method
  for printing objects of the class \code{din};
  \code{\link{summary.din}}, the S3 method for summarizing objects
  of the class \code{din}, which creates objects of the class
  \code{summary.din}; \code{\link{din}}, the main function for
  DINA and DINO parameter estimation, which creates objects of the class
  \code{din}.  See also \code{\link{CDM-package}} for general
  information about this package.
}

\examples{
##
## (1) examples based on dataset fractions.subtraction.data
##

## In particular, accessing detailed summary through assignment
detailed.summary.fs <- summary(din(data =
  fraction.subtraction.data, q.matrix = 
  fraction.subtraction.qmatrix, rule = "DINA"))
str(detailed.summary.fs)
}

\keyword{methods}
\keyword{print}