\name{cdm.est.class.accuracy}
\alias{cdm.est.class.accuracy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Classification Reliability in a CDM
}
\description{
This function computes the classification accuracy and
consistency by the method of Cui, Gierl and Chang (2012)
and by simulation. The simulation function now only works 
for models of class \code{din} (i.e. the mixed DINA and
DINO model).
}
\usage{
cdm.est.class.accuracy(cdmobj, n.sims = 0, seed = 987)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cdmobj}{
Object of class \code{din} or \code{gdina}
}
  \item{n.sims}{
Number of simulated persons. If \code{n.sims=0}, then the number
of persons in the original data is used as the sample size.
In case of missing item responses, everytime this sample
size is used.
Note that the simulation does (up to now) only work for 
CDMs of class \code{din}.
}
  \item{seed}{
Simulation seed
}
}
\details{
The item parameters and the probability distribution of
latent classes is used as the basis of the simulation. 
Accuracy and consistency is estimated for both MLE and MAP
classification estimators.
}
\value{
A data frame for MLE and MAP classification reliability with
following columns
%%  If it is a LIST, use
  \item{P_a}{Classification accuracy (Gierl et al., 2012)}
  \item{P_a_sim}{Classification accuracy based on simulated data}  
  \item{P_c}{Classification consistency (Gierl et al., 2012)}
  \item{P_c_sim}{Classification consistency based on simulated data}  
}
\references{
Cui, Y., Gierl, M. J., & Chang, H.-H. (2012). 
Estimating classification consistency and accuracy for cognitive diagnostic assessment. 
\emph{Journal of Educational Measurement}, \bold{49}, 19-38.
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
######################################
# DINO data example

# estimate DINO model
mod <- din( sim.dino , q.matrix = sim.qmatrix , rule="DINO")
# estimate classification reliability
cdm.est.class.accuracy(  mod , n.sims=5000)
##   > cdm.est.class.accuracy(  mod , n.sims=5000)
##         P_a P_a_sim   P_c P_c_sim
##   MLE 0.668   0.661 0.583   0.541
##   MAP 0.807   0.785 0.717   0.670
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Classification reliability}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
