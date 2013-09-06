\name{print.din}

\alias{print.din}

\title{
  Print Method for Objects of Class din
}

\description{
  S3 method to print objects of the class \code{din}.  
}

\usage{
  \method{print}{din}(x, highest=0.05, \dots)
}

\arguments{
  \item{x}{A required object of class \code{din}, obtained from a
    call to the function \code{\link{din}}.  }
  \item{highest}{Print classes with probability higher than \code{highest}}        
  \item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.  }
}

\details{
  The \code{print} method prints the main results obtained from
  a DINA or DINO model, which are the itemwise guessing and 
  slipping values with standard errors, as well as the skill 
  probabilities.  
}

\value{
  If the argument \code{x} is of required type, \code{print.din}
  prints the itemwise guessing and slipping values with standard errors,
  as well as the skill probabilities. It invisibly returns \code{x}.  
}




\seealso{
  \code{\link{plot.din}}, the S3 method for plotting objects of
  the class \code{din}; \code{\link{summary.din}}, the S3
  method for summarizing objects of the class \code{din}, which
  creates objects of the class \code{summary.din};
  \code{\link{print.summary.din}}, the S3 method for printing
  objects of the class \code{summary.din}; \code{\link{din}},
  the main function for DINA and DINO parameter estimation,
  which creates objects of the class \code{din}.  See also
  \code{\link{CDM-package}} for general information about this package.
}

\examples{
##
## (1) examples based on dataset fractions.subtraction.data
##

## Parameter estimation of DINA model
# rule = "DINA" is default
fractions.dina <- din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = "DINA")
str(fractions.dina)
}

\keyword{methods}
\keyword{print}