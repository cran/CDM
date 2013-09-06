\name{din.validate.qmatrix}
\alias{din.validate.qmatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Q-Matrix Validation (Q-Matrix Modification) for Mixed DINA/DINO Model
}
\description{
Q-matrix entries can be modified by the Q-matrix validation method
of de la Torre (2008). After estimating a mixed DINA/DINO model
using the \code{\link{din}} function, item parameters and the item
discrimination parameters \eqn{IDI_j} are recalculated. Q-matrix rows
are determined by maximizing the estimated item discrimination index
\eqn{IDI_j = 1-s_j -g_j}.
}

\usage{
din.validate.qmatrix(object, digits = 3, print = TRUE)
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{din}
}
  \item{digits}{
Number of digits for rounding the output matrix
}
\item{print}{An optional logical indicating whether the function
    should print the progress of iteration in the estimation process.  } 
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}


\value{
A list with following entries:
\item{coef.modified}{Estimated parameters by applying Q-matrix
   modificatioons}
\item{coef.modified.short}{A shorted matrix of \code{coef.modified}.
	Only Q-matrix rows which increase the \eqn{IDI} are displayed.
	}
\item{q.matrix.prop}{The proposed Q-matrix by Q-matrix validation.}
}


\references{
de la Torre, J. (2008). An empirically based method of Q-matrix 
validation for the DINA model: Development and applications. 
\emph{Journal of Educational Measurement}, \bold{45}, 343-362.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Estimated the mixed DINA/DINO model with \code{\link{din}}.
}
\examples{
#################################################################
# SIMULATED EXAMPLE 1: Detection of a mis-specified Q-matrix
#################################################################
set.seed(679)
# specify true Q-matrix
q.matrix <- matrix( 0 , 12 , 3 )
q.matrix[1:3,1] <- 1
q.matrix[4:6,2] <- 1
q.matrix[7:9,3] <- 1
q.matrix[10,] <- c(1,1,0)
q.matrix[11,] <- c(1,0,1)
q.matrix[12,] <- c(0,1,1)
# simulate data
dat <- sim.din( N=4000 , q.matrix)$dat
# incorrectly modify Q-matrix rows 1 and 10
Q1 <- q.matrix
Q1[1,] <- c(1,1,0)
Q1[10,] <- c(1,0,0)
# estimate DINA model
mod <- din( dat , q.matr = Q1, rule = "DINA")
# apply Q-matrix validation
res <- din.validate.qmatrix( mod )	
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Q-matrix validation}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
