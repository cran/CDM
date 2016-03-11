\name{numerical_Hessian}
\alias{numerical_Hessian}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Numerical Computation of the Hessian Matrix
}
\description{
Computes numerically the Hessian matrix of a given function.
}
\usage{
numerical_Hessian(par, FUN, h = 1e-05, gradient = FALSE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{par}{
Parameter vector
}
  \item{FUN}{
Specified function with argument vector \code{x}
}
  \item{h}{
Numerical differentiation parameter. Can be also a vector.
}
  \item{gradient}{
Logical indicating whether the gradient should be calculated.
If it is \code{FALSE}, then the Hessian matrix is computed. 
}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
Gradient vector or Hessian matrix
}

%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See the \pkg{numDeriv} package and the \code{mirt::numerical_deriv}
function from the \pkg{mirt} package.
}

\examples{
#############################################################################
# EXAMPLE 1: Toy example for Hessian matrix
#############################################################################
	
# define function
f <- function(x){
     3*x[1]^3 - 4*x[2]^2 - 5*x[1]*x[2] + 10 * x[1] * x[3]^2 + 6*x[2]*sqrt(x[3])
            }
# define point for evaluating partial derivatives            
par <- c(3,8,4)

#--- compute gradient 
numerical_Hessian( par = par , FUN = f , gradient=TRUE )
\dontrun{
mirt::numerical_deriv(par = par , f = f, gradient=TRUE)

#--- compute Hessian matrix
numerical_Hessian( par = par , FUN = f )
mirt::numerical_deriv(par = par , f = f, gradient=FALSE)
numerical_Hessian( par = par , FUN = f , h = 1E-4 )
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
