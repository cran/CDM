\name{summary.din}

\alias{summary.din}

\title{
  Summary Method for Objects of Class din
}

\description{
  S3 method to summarize objects of the class \code{din}.  
}

\usage{
  \method{summary}{din}(object, \dots)
}

\arguments{
  \item{object}{a required object of class \code{din}, obtained
    from a call to the function \code{\link{din}}.  }
    
  \item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.  }
}

\details{
  The function \code{summary.din} returns an object of the class
  \code{summary.din} (see \sQuote{Value}), for which a
  \code{print} method, \code{\link{print.summary.din}}, is
  provided.  Specific summary information details such as
  individual item parameters and their diagnostic accuracy can be accessed through
  assignment (see \sQuote{Examples}).  
}

\value{
  If the argument \code{object} is of required type,
  \code{summary.din} returns a named list, of the class
  \code{summary.din}, consisting of the following five components:
  
  \item{CALL}{a character specifying the model rule, the number of
    items and the number of attributes underlying the items.  }
    
  \item{IDI}{a matrix giving the diagnostic accuracy
    for each item \eqn{j}
    \deqn{ IDI_j = 1 - \frac{g_j}{1-s_j}, }{ IDI_j = 1 - g_j/(1-s_j), }
    where high diagnostic accuracy corresponds to good test items
    which have both low guessing and slipping rates.  Note that
    a negative diagnostic accuracy in one item contradicts the
    error parameter constraint \eqn{g_j < 1 - s_j}.  See \code{\link{din}}
    for help.  }
    
  \item{SKILL.CLASS.PROB}{a table which returns the
    minimum, maximum, quantile and mean information of the skill
    pattern distribution.  }
    
  \item{AIC}{a numeric giving the AIC of the specified model
    \code{object}.  }
    
  \item{BIC}{a numeric giving the BIC of the specified model
    \code{object}.  }
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
  Measurement: Theory, Methods, and Applications}.  New York: The Guilford
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

## Parameter estimation of DINA model
# rule = "DINA" is default
fractions.dina <- din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = "DINA")
  
## corresponding summaries, including diagnostic accuracies,
## summary of skill pattern distribution and information 
## criteria AIC and BIC
summary(fractions.dina)

## In particular, accessing detailed summary through assignment
detailed.summary.fs <- summary(fractions.dina)
str(detailed.summary.fs)
}

\keyword{methods}
\keyword{summary}
