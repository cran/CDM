\name{modelfit.cor}
\alias{modelfit.cor}
\alias{modelfit.cor.din}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assessing Model Fit by Comparing Observed and Expected Item Pair Correlations
}
\description{
This function computes several measures of absolute model fit which are 
based on comparing observed and expected frequencies of item pairs.
}
\usage{
modelfit.cor(data, posterior, probs)

modelfit.cor.din( dinobj )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
An \eqn{N} times \eqn{I} data frame of dichotomous item responses
}
  \item{posterior}{
Posterior distribution
}
  \item{probs}{
Probabilities as an array: [items,categories,attribute classes]
}
\item{dinobj}{An object of class \code{din}, \code{gdina} or
\code{gdm}}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
A list with following entries
%%  If it is a LIST, use
  \item{modelfit}{Model fit statistics:
  	  
   \code{MADcor}: mean of absolute deviations in observed and expected correlations 
        (DiBello et al., 2007) 
        
    \code{MX2}: Mean of \eqn{\chi^2} statistics of all item pairs 
    (Chen & Thissen, 1997)
    
    \code{MADRESIDCOV}: Mean of absolute deviations of residual covariances
    (McDonald & Mok, 1995) 
    \code{MADQ3}: Mean of absolute values of \eqn{Q_3} statistic (Yen, 1984)
            }
  \item{itempairs}{Fit of itempairs}
  \item{pfit}{Fit of single fit with respect to the observed proportions}  
}
\references{
Chen, W., & Thissen, D. (1997). Local dependence indexes for item pairs 
 using item response theory. \emph{Journal of Educational and Behavioral Statistics},
 \bold{22}, 265-289.

  DiBello, L. V., Roussos, L. A., & Stout, W. F. (2007). Review of
  cognitively diagnostic assessment and a summary of psychometric models.
  In C. R. Rao and S. Sinharay (Eds.), \emph{Handbook of Statistics},
  Vol. 26 (pp. 979--1030). Amsterdam: Elsevier.

  McDonald, R. P., & Mok, M. M.-C. (1995). Goodness of fit in item response models.
  \emph{Multivariate Behavioral Research}, \bold{30}, 23-40.

Yen, W. M. (1984). Effects of local item dependence on the fit and equating 
performance of the three-parameter logistic model. 
\emph{Applied Psychological Measurement}, \bold{8}, 125-145.
}
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
\dontrun{	
# use DINA and DINO model for DINA simulated data
d1 <- din(sim.dina, q.matr = sim.qmatrix, rule = "DINA" )
d2 <- din(sim.dina, q.matr = sim.qmatrix, rule = "DINO" )
# DINA model in gdina function
d3 <- gdina( sim.dina , q.matr = sim.qmatrix , rule="DINA" )
# estimation with DINA model
g1 <- modelfit.cor.din( d1 )
g3 <- modelfit.cor.din( d3 )
# estimation with DINO model => wrong model specification
g2 <- modelfit.cor.din( d2 )

##   > # estimation with DINA model
##   > g1 <- modelfit.cor.din( d1 )
##                       est
##   MADcor          0.03025
##   MX2             0.71949
##   100*MADRESIDCOV 0.67140
##   MADQ3           0.06184
##
##  -> g3 provides the same result as g1
##
##   > # estimation with DINO model => wrong model specification
##   > g2 <- modelfit.cor.din( d2 )
##                       est
##   MADcor          0.05552
##   MX2             2.20449
##   100*MADRESIDCOV 1.22491
##   MADQ3           0.07294 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Model fit}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
