\name{anova.din}
\alias{anova.din}
\alias{anova.gdina}
\alias{anova.gdm}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Likelihood Ratio Test for Model Comparisons
}
\description{
This function compares two models estimated with \code{din}, \code{gdina}
or \code{gdm} using a likelihood ratio test.
}
\usage{
\method{anova}{din}(object,object1,\dots)
\method{anova}{gdina}(object,object1,\dots)
\method{anova}{gdm}(object,object1,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object of class \code{din}, \code{gdina} or \code{gdm}
}
  \item{object1}{
Object of class \code{din}, \code{gdina} or \code{gdm}
}
  \item{\dots}{
Further arguments to be passed
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
%\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
%}
%\references{
%% ~put references to the literature/web site here ~
%}
%\author{
%Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
#################################
# anova with din objects

# Model 1
d1 <- din(sim.dina, q.matr = sim.qmatrix )
# Model 2 with equal guessing and slipping parameters
d2 <- din(sim.dina, q.matr = sim.qmatrix , guess.equal=TRUE , slip.equal =TRUE)
# model comparison
anova(d1,d2)
##   > anova(d1,d2)
##       Model   loglike Deviance Npars      AIC      BIC    Chisq df  p
##   2 Model 2 -2176.482 4352.963     9 4370.963 4406.886 268.2071 16  0
##   1 Model 1 -2042.378 4084.756    25 4134.756 4234.543       NA NA NA

\dontrun{
#################################
# anova with gdina objects

# Model 3: GDINA model
d3 <- gdina( sim.dina, q.matr = sim.qmatrix )

# Model 4: DINA model
d4 <- gdina( sim.dina, q.matr = sim.qmatrix , rule="DINA")

# model comparison
anova(d3,d4)
##   > anova(d3,d4)
##       Model   loglike Deviance Npars      AIC      BIC    Chisq df       p
##   2 Model 2 -2042.378 4084.756    25 4134.756 4234.543 31.45153 16 0.01178
##   1 Model 1 -2026.652 4053.305    41 4135.305 4298.955       NA NA      NA }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Likelihood ratio test}
\keyword{anova}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
