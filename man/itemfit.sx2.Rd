\name{itemfit.sx2}
\alias{itemfit.sx2}
\alias{summary.itemfit.sx2}
\alias{plot.itemfit.sx2}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{
S-X2 Item Fit Statistic For Dichotomous Data
}

\description{
Computes the S-X2 item fit statistic (Orlando & Thissen; 2000, 2003)
for dichotomous data. Note that completely observed data is
necessarry for applying this function.
}
\usage{
itemfit.sx2(object, Eik_min = 1, progress = TRUE)

\method{summary}{itemfit.sx2}(object, \dots)

\method{plot}{itemfit.sx2}(x, ask=TRUE, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{object}{
Object of class \code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}},  
\code{\link[sirt:rasch.mml]{rasch.mml}} (\pkg{sirt}),
\code{\link[sirt:smirt]{smirt}} (\pkg{sirt}) or
\code{\link[TAM:tam.mml]{tam.mml}} (\pkg{TAM})
}
\item{x}{
Object of class \code{\link{din}}, \code{\link{gdina}}, \code{\link{gdm}},  
\code{\link[sirt:rasch.mml]{rasch.mml}} (\pkg{sirt}),
\code{\link[sirt:smirt]{smirt}} (\pkg{sirt}) or
\code{\link[TAM:tam.mml]{tam.mml}} (\pkg{TAM})
}
  \item{Eik_min}{
The minimum expected cell size for merging score groups.
}
  \item{progress}{
An optional logical indicating whether progress should be displayed.
}
\item{ask}{An optional logical indicating whether every item should be
separately displayed.} 
\item{\dots}{Further arguments to be passed}
}

\details{
The S-X2 item fit statistic compares observed and expected proportions
\eqn{O_{jk}} and \eqn{E_{jk}} for item \eqn{j} and 
each score group \eqn{k} and forms a chi-square distributed statistic
\deqn{S-X_j^2 = \sum_{k=1}^{J-1} N_k \frac{ ( O_{jk} - E_{jk} )^2 }
	{ E_{jk} ( 1 - E_{jk} ) } }
The degrees of freedom are \eqn{J-1-P_j} where \eqn{P_j} denotes
the number of estimated item parameters.
}

\value{
A list with following entries

\item{itemfit.stat}{Data frame containing item fit statistics}
\item{itemtable}{Data frame with expected and observed proportions
for each score group and each item. Beside the \code{p} value,
a p value obtained by correction due to multiple testing
is provided (\code{p.holm}, see \code{\link{p.adjust}}).
}
}

\references{
Orlando, M., & Thissen, D. (2000). Likelihood-based item-fit indices for 
dichotomous item response theory models. 
\emph{Applied Psychological Measurement}, \bold{24}, 50-64.
	
Orlando, M., & Thissen, D. (2003). Further investigation of the performance of 
S-X2: An item fit index for use with dichotomous item response theory models. 
\emph{Applied Psychological Measurement}, \bold{27}, 289-298.
}

\author{
Alexander Robitzsch
}

\note{
This function does not work properly for multiple groups.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
#############################################################################
# SIMULATED EXAMPLE 1: Items with unequal item slopes
#############################################################################

# simulate data
set.seed(9871)
I <- 11
b <- seq( -1.5, 1.5 , length=I)
a <- rep(1,I)
a[4] <- .4
N <- 1000
library(sirt)
dat <- sim.raschtype( theta= rnorm(N) , b=b , fixed.a=a)

#*** 1PL model estimated with gdm
mod1 <- gdm( dat , theta.k=seq(-6,6,len=21) , irtmodel="1PL" )
summary(mod1)
# estimate item fit statistic
fitmod1 <- itemfit.sx2(mod1)
summary(fitmod1)
##       item itemindex   S-X2 df     p S-X2_df RMSEA Nscgr Npars p.holm
##   1  I0001         1 29.124  9 0.001   3.236 0.047    10     1  0.006
##   2  I0002         2 14.714  9 0.099   1.635 0.025    10     1  0.676
##   3  I0003         3  6.271  9 0.712   0.697 0.000    10     1  1.000
##   4  I0004         4 35.479  9 0.000   3.942 0.054    10     1  0.001
##   5  I0005         5 12.093  9 0.208   1.344 0.019    10     1  0.914
##   6  I0006         6 19.026  9 0.025   2.114 0.033    10     1  0.225
##   7  I0007         7 14.802  9 0.097   1.645 0.025    10     1  0.676
##   [...]
##   -- Average Item Fit Statistics --
##   S-X2 = 16.102 | S-X2_df = 1.789 

## -> 4th item does not fit to the 1PL model

# plot item fit
plot(fitmod1)

\dontrun{
#*** 2PL model estimated with gdm
mod2 <- gdm( dat , theta.k=seq(-6,6,len=21) , irtmodel="2PL" , maxiter=100 )
summary(mod2)
# estimate item fit statistic
fitmod2 <- itemfit.sx2(mod2)
summary(fitmod2)
##       item itemindex   S-X2 df     p S-X2_df RMSEA Nscgr Npars p.holm
##   1  I0001         1 35.086  8 0.000   4.386 0.058    10     2  0.000
##   2  I0002         2 15.570  8 0.049   1.946 0.031    10     2  0.490
##   3  I0003         3  6.233  8 0.621   0.779 0.000    10     2  1.000
##   4  I0004         4  5.975  8 0.650   0.747 0.000    10     2  1.000
##   5  I0005         5 12.453  8 0.132   1.557 0.024    10     2  0.793
##   6  I0006         6 14.388  8 0.072   1.799 0.028    10     2  0.525
##   7  I0007         7 14.680  8 0.066   1.835 0.029    10     2  0.525
##   [...]
##   -- Average Item Fit Statistics --
##   S-X2 = 13.157 | S-X2_df = 1.645

#*** 1PL model estimation in smirt (sirt package)
Qmatrix <- matrix(1 , I,1 )
mod1a <- smirt( dat , Qmatrix=Qmatrix )
summary(mod1a)
# item fit statistic
fitmod1a <- itemfit.sx2(mod1a)
summary(fitmod1a)

#*** 2PL model estimation in smirt (sirt package)
mod2a <- smirt( dat , Qmatrix=Qmatrix , est.a="2PL")
summary(mod2a)
# item fit statistic
fitmod2a <- itemfit.sx2(mod2a)
summary(fitmod2a)

#*** 1PL model estimated with rasch.mml2 (in sirt)
mod1b <- rasch.mml2( dat )
summary(mod1b)
# estimate item fit statistic
fitmod1b <- itemfit.sx2(mod1b)
summary(fitmod1b)

#*** 1PL estimated in TAM
library(TAM)
mod1c <- tam.mml( resp=dat )
summary(mod1c)
# item fit
summary( itemfit.sx2( mod1c) )

#*** 2PL estimated in TAM
mod2c <- tam.mml.2pl( resp=dat )
summary(mod2c)
# item fit
summary( itemfit.sx2( mod2c) )

#############################################################################
# EXAMPLE 2: Item fit statistics sim.dina dataset
#############################################################################

data(sim.dina)
data(sim.qmatrix)

#*** Model 1: DINA model (correctly specified model)
mod1 <- din( data=sim.dina , q.matrix = sim.qmatrix )
summary(mod1)
# item fit statistic
summary( itemfit.sx2( mod1 ) )
##   -- Average Item Fit Statistics --
##   S-X2 = 12.248 | S-X2_df = 2.041 

#*** Model 2: Mixed DINA/DINO model
#***  1th item is misspecified according to DINO rule
I <- ncol(sim.dina)
rule <- rep("DINA" , I )
rule[1] <- "DINO"
mod2 <- din( data=sim.dina , q.matrix = sim.qmatrix , rule=rule)
summary(mod2)
# item fit statistic
summary( itemfit.sx2( mod2 ) )
##   -- Average Item Fit Statistics --
##   S-X2 = 25.844 | S-X2_df = 4.307 

#*** Model 3: Additive GDINA model
mod3 <- gdina( data=sim.dina , q.matrix = sim.qmatrix , rule="ACDM")
summary(mod3)
# item fit statistic
summary( itemfit.sx2( mod3 ) )
##   -- Average Item Fit Statistics --
##   S-X2 = 12.642 | S-X2_df = 2.475
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Item fit}
\keyword{summary}
\keyword{plot}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
