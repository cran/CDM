\name{CDM-internal}

\alias{CDM-internal}
\alias{check.input}
\concept{check input}
\alias{rowMaxs}
\concept{row maxs}
\alias{rowProds}
\concept{row prods}

\title{
  Internal CDM Functions
}

\description{
  The three internal functions of the \code{CDM} package used for checking
  the input format and computing intermediate information.  
}

\usage{
  check.input(data, q.matrix, conv.crit = 0.001, maxit = 100,
    constraint.guess = NULL, constraint.slip = NULL, 
    guess.init = rep(0.20, ncol(data)), slip.init =
    guess.init, weights = rep(1, nrow(data)), rule = "DINA",
    progress = TRUE)
  rowMaxs(mat)
  rowProds(matr)
  rowProds2(matr)
  %  .create.Mj( Aj , rule = "GDINA" )
  %ICI(object)
}

\details{
  These functions are not intended to be called by the user. They are for
  checking the input format and computing intermediate information,
  processed to give the final cognitive diagnosis modeling results.  
}

\note{
  There are also the internal functions \code{\link{plot.din}},
  \code{\link{print.din}}, and \code{\link{summary.din}}, the \code{plot},
  \code{print}, and \code{summary} methods for objects of the class
  \code{din}, and the internal function \code{\link{print.summary.din}},
  the \code{print} method for objects of the class \code{summary.din}.  
  These functions are documented, respectively, at the afore mentioned
  links.  
}

\keyword{internal}
