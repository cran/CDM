\name{gdina.wald}
\alias{gdina.wald}
\alias{summary.gdina.wald}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Wald Statistic for Item Fit of the DINA and ACDM Rule for GDINA Model
}
\description{
This function tests with a Wald test for the GDINA model whether a DINA or a ACDM
condensation rule leads to a sufficient item fit compared
to the saturated GDINA rule (de la Torre & Lee, 2013).
}
\usage{
gdina.wald(object)

\method{summary}{gdina.wald}(object, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A fitted \code{\link{gdina}} model
}
\item{\dots}{Further arguments to be passed}
}

%\details{
%%  ~~ If necessary, more details than the description above ~~
%}

\value{
  \item{stats}{Data frame with Wald statistic for every item,
  correponding p values and a RMSEA fit statistic}
}

\references{
de la Torre, J., & Lee, Y. S. (2013). Evaluating the Wald test for 
item-level comparison of saturated and reduced models in cognitive diagnosis. 
\emph{Journal of Educational Measurement}, \bold{50}, 355-373.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: Wald test for DINA simulated data sim.dina
#############################################################################		
data(sim.dina)
data(sim.qmatrix)

# Model 1: estimate GDINA model
mod1 <- gdina( sim.dina , q.matrix = sim.qmatrix ,  rule = "GDINA")
summary(mod1)

# perform Wald test
res1 <- gdina.wald( mod1 )
summary(res1)
# -> results show that all but one item fit according to the DINA rule
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Wald test}
\keyword{GDINA model}% __ONLY ONE__ keyword per line
