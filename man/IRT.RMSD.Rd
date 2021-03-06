%% File Name: IRT.RMSD.Rd
%% File Version: 0.23

\name{IRT.RMSD}
\alias{IRT.RMSD}
\alias{summary.IRT.RMSD}
\alias{IRT_RMSD_calc_rmsd}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Root Mean Square Deviation (RMSD) Item Fit Statistic
}

\description{
Computed the item fit statistics root mean square deviation (RMSD),
mean absolute deviation (MAD) and mean deviation (MD).
See Oliveri and von Davier (2011) for details.

The RMSD statistics was denoted as the RMSEA statistic in older
publications, see \code{\link{itemfit.rmsea}}.

If multiple groups are defined in the model object, a weighted item fit
statistic (WRMSD; Yamamoto, Khorramdel, & von Davier, 2013;
von Davier, Weeks, Chen, Allen & van der Velden, 2013) is
additionally computed.
}


\usage{
IRT.RMSD(object)

\method{summary}{IRT.RMSD}(object, file=NULL, digits=3, \dots)

## core computation function
IRT_RMSD_calc_rmsd( n.ik, pi.k, probs, eps=1E-30 )
}


%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
Object for which the methods \code{IRT.expectedCounts} and
\code{IRT.irfprob} can be applied.
}
\item{n.ik}{Expected counts}
\item{pi.k}{Probabilities trait distribution}
\item{probs}{Item response probabilities}
\item{eps}{Numerical constant avoiding division by zero}
  \item{digits}{Number of digits used for rounding}
\item{file}{Optional file name for a file in which \code{summary}
should be sinked.}
    \item{\dots}{Optional parameters to be passed.  }
}

\details{
The RMSD and MD statistics are in operational use in PISA studies
since PISA 2015. These fit statistics can also be used for investigating
uniform and nonuniform differential item functioning.
}

\value{
List with entries
 \item{RMSD}{Item-wise and group-wise RMSD statistic}
 \item{RMSD_bc}{Item-wise and group-wise RMSD statistic with analytical
      bias correction}
 \item{MAD}{Item-wise and group-wise MAD statistic}
 \item{MD}{Item-wise and group-wise MD statistic}
 \item{chisquare_stat}{Item-wise and group-wise \eqn{\chi^2} statistic}
 \item{\dots}{Further values}
}

\references{
Oliveri, M. E., & von Davier, M. (2011).
Investigation of model fit and score scale comparability in
international assessments.
\emph{Psychological Test and Assessment Modeling, 53}, 315-333.

von Davier, M., Weeks, J., Chen, H., Allen, J., & van der Velden, R. (2013).
Creating simple and complex derived variables and validation of background
questionnaire data.
In OECD (Eds.). \emph{Technical Report of the Survey of Adults Skills (PIAAC)}
(Ch. 20). Paris: OECD.

Yamamoto, K., Khorramdel, L., & von Davier, M. (2013).
Scaling PIAAC cognitive data.
In OECD (Eds.). \emph{Technical Report of the Survey of Adults Skills (PIAAC)}
(Ch. 17). Paris: OECD.
}

%\author{
%%  ~~who you are~~
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{itemfit.rmsea}}
}

\examples{
\dontrun{
#############################################################################
# EXAMPLE 1: data.read | 1PL model in TAM
#############################################################################

data(data.read, package="sirt")
dat <- data.read

#*** Model 1: 1PL model
mod1 <- TAM::tam.mml( resp=dat )
summary(mod1)

# item fit statistics
imod1 <- CDM::IRT.RMSD(mod1)
summary(imod1)

#############################################################################
# EXAMPLE 2: data.math| RMSD and MD statistic for assessing DIF
#############################################################################

data(data.math, package="sirt")
dat <- data.math$data
items <- grep("M[A-Z]", colnames(dat), value=TRUE )

#-- fit multiple group Rasch model
mod <- TAM::tam.mml( dat[,items], group=dat$female )
summary(mod)

#-- fit statistics
rmod <- CDM::IRT.RMSD(mod)
summary(rmod)

#############################################################################
# EXAMPLE 3: RMSD statistic DINA model
#############################################################################

data(sim.dina)
data(sim.qmatrix)
dat <- sim.dina
Q <- sim.qmatrix

#-- fit DINA model
mod1 <- CDM::gdina( dat, q.matrix=Q, rule="DINA" )
summary(mod1)

#-- compute RMSD fit statistic
rmod1 <- CDM::IRT.RMSD(mod1)
summary(rmod1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
