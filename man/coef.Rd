\name{coef}
\alias{coef.din}
\alias{coef.gdina}
\alias{coef.gdm}
\title{Extract Estimated Item Parameters}
\description{
  Extracts the estimated parameters from either 
  \code{din}, \code{gdina} or \code{gdm} objects.
}
\usage{
\method{coef}{din}(object, \dots)
\method{coef}{gdina}(object, \dots)
\method{coef}{gdm}(object, \dots)
}

\arguments{
  \item{object}{An object inheriting from either class \code{din}, 
  class \code{gdina} or class \code{gdm}. 
  			}
  \item{\dots}{Additional arguments; currently none is used. }
}

%%\details{
%%%% nothing yet
%%}

\value{
   A matrix or a data frame of the estimated parameters for the fitted model.
	}

%%\references{
%% include something here
%%}

%%\author{
%% CDM authors
%%}

\seealso{
	\code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}}
}

\examples{
# use sim.dina dataset
data(sim.dina)

# DINA model
d1 <- din(sim.dina, q.matr = sim.qmatrix)
coef(d1)

# GDINA model
d2 <- gdina(sim.dina, q.matr = sim.qmatrix)
coef(d2)

# GDM model (use only 10 iterations for computation time reasons)
theta.k <- seq(-4,4,len=11)
d3 <- gdm( sim.dina , irtmodel="2PL" , theta.k=theta.k , 
     Qmatrix=as.matrix(sim.qmatrix) , centered.latent=TRUE , maxiter=10 )
coef(d3)
}

\keyword{methods}
\keyword{coef}
