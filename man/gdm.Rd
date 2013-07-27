\name{gdm}
\alias{gdm}
\alias{summary.gdm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
General Diagnostic Model
}
\description{
This function estimates the general diagnostic model
(von Davier, 2008; Xu & von Davier, 2008) which handles 
multidimensional item response models with ordered discrete
or continuous latent variables for polytomous item
responses.
}
\usage{
gdm( data, theta.k, irtmodel="2PL", group=NULL, weights=rep(1, nrow(data)), 
    Qmatrix=NULL , thetaDes = NULL , skillspace="loglinear",  
    b.constraint=NULL, a.constraint=NULL, 
    mean.constraint=NULL , Sigma.constraint=NULL , delta.designmatrix=NULL, 
    standardized.latent=FALSE , centered.latent=FALSE , 
    centerintercepts=FALSE , centerslopes=FALSE ,
    maxiter=1000,  conv=10^(-5), globconv=10^(-5), msteps=4 , convM=.0005 ,
    decrease.increments = FALSE , use.freqpatt=FALSE , ...) 

\method{summary}{gdm}(object, \dots)    
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ARGUMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
    An \eqn{N} times \eqn{I} matrix of polytomous item
    responses with categories \eqn{k=0,1,...,K}
}
  \item{theta.k}{
In the one-dimensional case it must be a vector.
For multidimensional models it has to be a list
of skill vectors if the theta grid differs between
dimensions. If not, a vector input can be supplied.
If an estimated skillspace (\code{skillspace="est"}, a vector or
a matrix \code{theta.k} will be initial values of the estimated 
\eqn{\bold{\theta}} grid.
}
  \item{irtmodel}{
The default \code{2PL} corresponds to the model
where item slopes on dimensions are equal for all
item categories. If item-category slopes should
be estimated, use \code{2PLcat}. If no item slopes
should be estimated then \code{1PL} can be selected.
Note that fixed item slopes can be specified in 
the Q-matrix (argument \code{Qmatrix}).
}
  \item{group}{
An optional vector of group identifiers for 
multiple group estimation
}
  \item{weights}{
Sample weights
}
  \item{Qmatrix}{
An optional array \eqn{I} times \eqn{D} times \eqn{K} 
which indicates pre-specified item loadings
on dimensions. The default for category \eqn{k} is the score \eqn{k}, i.e.
the scoring in the (generalized) partial credit model.
}
\item{thetaDes}{
A design matrix for specifying nonlinear item response
functions (see Example 1, Models 4 and 5)
}
  \item{skillspace}{
The parametric assumption of the skillspace. 
If \code{skillspace="normal"} then a univariate or
multivariate normal distribution is assumed. 
The default \code{"loglinear"} corresponds to log-linear
smoothing of the skillspace distribution (Xu & von Davier, 2008).
If \code{skillspace="full"}, then all probabilities of the skill space
are nonparametrically estimated. If \code{skillspace="est"}, then the 
\eqn{\bold{\theta}} distribution vectors will be estimated
(see Details and Simulated Examples 4 and 5; Bartolucci, 2007).
}
  \item{b.constraint}{
In this optional matrix with \eqn{C_b} rows and three columns,
\eqn{C_b} item intercepts \eqn{b_{ik}} can be fixed.
1st column: item index, 2nd column: category index,
3rd column: fixed item thresholds
}
  \item{a.constraint}{
In this optional matrix with \eqn{C_a} rows and four columns,
\eqn{C_a} item intercepts \eqn{a_{idk}} can be fixed.
1st column: item index, 2nd column: dimension index,
3rd column: category index, 4th column: fixed item slopes \cr
}
\item{mean.constraint}{A \eqn{C} times 3 matrix for 
  constraining \eqn{C} means in the
  normal distribution assumption (\code{skillspace="normal"}).
  1st column: Dimension, 2nd column: Group, 3rd column: Value
        }
\item{Sigma.constraint}{A \eqn{C} times 4 matrix for 
  constraining \eqn{C} covariances in the
  normal distribution assumption (\code{skillspace="normal"}).
  1st column: Dimension 1, 2nd column: Dimension 2, 
  3rd column: Group, 4th column: Value
        }
  \item{delta.designmatrix}{
The design matrix of \eqn{\delta} parameters
for the reduced skillspace estimation (see Xu &
von Davier, 2008)
}
\item{standardized.latent}{Should in a uni- or multidimensional
model all latent variables of the first group be normally distributed and standardized?
The default is \code{FALSE}.}
\item{centered.latent}{
	Should in a uni- or multidimensional
	model all latent variables of the first group be normally distributed
	and have zero means? The default is \code{FALSE}.
		}
\item{centerintercepts}{
	Should intercepts be centerred to have a mean of 0 for
	all dimensions? This argument does not (yet) work properly
	for varying numbers of item categories.
		}
\item{centerslopes}{
	Should slopes be centered to have a mean of 1 for all dimensions?
	This argument only works for \code{irtmodel="2PL"}. The default is
	\code{FALSE}.
		}
  \item{maxiter}{
Maximum number of iterations
}
  \item{conv}{
Convergence criterion for item parameters and
distribution parameters
}
  \item{globconv}{
Global deviance convergence criterion
}
\item{msteps}{
	Maximum number of M steps in estimating \eqn{b} and
	\eqn{a} item parameters. The default is 4 M steps.
}
\item{convM}{
	Convergence criterion in M step
		}
\item{decrease.increments}{Should in the M step the increments
    of \eqn{a} and \eqn{b} parameters decrease during iterations?
    The default is \code{FALSE}. If there is an increase in deviance
    during estimation, setting \code{decrease.increments} to \code{TRUE}
    is recommended.
}
\item{use.freqpatt}{
	Should frequencies of unique item response patterns
	be used. In case of large data set \code{use.freqpatt=TRUE}
	can speed calculations. Note that in this case, not all
	person parameters are calculated as usual in the
	output.
		}
  \item{object}{a required object of class \code{gdm}}
  \item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.  }  		
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DETAILS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\details{
Case \code{irtmodel="1PL"}: \cr
Equal item slopes of 1 are assumed in this model. Therefore,
it corresponds to a generalized multidimensional Rasch model.
\deqn{logit P( X_{nj} = k | \theta_n ) = b_{j0} +
    \sum_d q_{jdk} \theta_{nd} }
The Q-matrix entries \eqn{q_{jdk}} are pre-specified by the user.

Case \code{irtmodel="2PL"}: \cr
For each item and each dimension, different item slopes \eqn{a_{jd}}
are estimated:
\deqn{logit P( X_{nj} = k | \theta_n ) = b_{j0} +
    \sum_d a_{jd} q_{jdk} \theta_{nd} }
Case \code{irtmodel="2PLcat"}: \cr
For each item, each dimension and each category, 
different item slopes \eqn{a_{jdk}}
are estimated:
\deqn{logit P( X_{nj} = k | \theta_n ) = b_{j0} +
    \sum_d a_{jdk} q_{jdk} \theta_{nd} }

Note that this model can be generalized to include terms of 
any transformation \eqn{t_h} of the \eqn{\theta_n} vector (e.g. quadratic terms,
step functions or interaction) such that the model can be formulated as
\deqn{logit P( X_{nj} = k | \theta_n ) = b_{j0} +
    \sum_h a_{jhk} q_{jhk} t_h( \theta_{n} ) }
In general, the number of functions \eqn{t_1 , ... , t_H} will be 
larger than the \eqn{\theta} dimension of \eqn{D}.

The estimation follows an EM algorithm as described in von Davier and
Yamamoto (2004) and von Davier (2008)

In case of \code{skillspace="est"}, the \eqn{\bold{\theta}} vectors 
(the grid of the theta distribution) are estimated (Bartolucci, 2007;
Bacci, Bartolucci & Gnaldi, 2012). This model is called a multidimensional
latent class item response model.
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VALUES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\value{
An object of class \code{gdm}. The list contains the
following entries:
% \item{xxx}{yyy}
\item{item}{Data frame with item parameters}
\item{person}{Data frame with person parameters:
    \code{EAP} denotes the mean of the individual posterior distribution,
    \code{SE.EAP} the corresponding standard error, 
    \code{MLE} the maximum likelihood estimate at \code{theta.k}
    and \code{MAP} the mode of the posterior distribution
}
\item{EAP.rel}{Reliability of the EAP}
\item{deviance}{Deviance}
\item{ic}{Information criteria, number of estimated parameters}
\item{b}{Item intercepts \eqn{b_{jk}}}
\item{se.b}{Standard error of item intercepts \eqn{b_{jk}}}
\item{a}{Item slopes \eqn{a_{jd}} resp. \eqn{a_{jdk}}}
\item{se.a}{Standard error of item slopes \eqn{a_{jd}} resp. \eqn{a_{jdk}}}
\item{itemfit.rmsea}{The RMSEA item fit index (see \code{\link{itemfit.rmsea}}).
	This entry comes as a list with total and group-wise item fit
	statistics.
            }
\item{mean.rmsea}{Mean of RMSEA item fit indexes.
            }            
\item{Qmatrix}{Used Q-matrix}
\item{pi.k}{Trait distribution}
\item{mean.trait}{Means of trait distribution}
\item{sd.trait}{Standard deviations of trait distribution}
\item{skewness.trait}{Skewnesses of trait distribution}
\item{correlation.trait}{List of correlation matrices of trait distribution 
      corresponding to each group}
\item{pjk}{Item response probabilities evaluated at grid \code{theta.k}}
\item{n.ik}{An array of expected counts \eqn{n_{cikg}} of ability class \eqn{c}
    at item \eqn{i} at category \eqn{k} in group \eqn{g}}
\item{G}{Number of groups}
\item{D}{Number of dimension of \eqn{\bold{\theta}}}
\item{I}{Number of items}
\item{N}{Number of persons}
\item{delta}{Parameter estimates for skillspace representation}
\item{covdelta}{Covariance matrix of parameter estimates for
      skillspace representation}
\item{data}{Original data frame}
\item{group.stat}{Group statistics (sample sizes, group labels)}
\item{p.xi.aj}{Individual likelihood}
\item{posterior}{Individual posterior distribution}
\item{skill.levels}{Number of skill levels per dimension}
\item{K.item}{Maximal category per item}
\item{theta.k}{Used theta design or estimated theta trait distribution
	in case of \code{skillspace="est"} }
\item{thetaDes}{Used theta design for item responses}
\item{se.theta.k}{Estimated standard errors of \code{theta.k} if it is
	estimated}
\item{time}{Info about computation time}
\item{skillspace}{Used skillspace parametrization}
\item{iter}{Number of iterations}
\item{object}{a required object of class \code{gdm}}
\item{\dots}{Optional parameters to be passed to or from other
    methods will be ignored.  }  

}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% REFERENCES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\references{	
Bacci, S., Bartolucci, F., & Gnaldi, M. (2012). 
A class of Multidimensional Latent Class IRT models for ordinal 
polytomous item responses. \emph{arXiv preprint}, \emph{arXiv:1201.4667}.
	
Bartolucci, F. (2007). A class of multidimensional IRT models for testing 
unidimensionality and clustering items. \emph{Psychometrika}, \bold{72}, 141-157.

von Davier, M. (2008). A general diagnostic model applied to 
language testing data. \emph{British Journal
of Mathematical and Statistical Psychology}, \bold{61}, 287-307.

von Davier, M., & Yamamoto, K. (2004). Partially observed mixtures of IRT models: 
An extension of the generalized partial-credit model. 
\emph{Applied Psychological Measurement}, \bold{28}, 389-406.  
  
Xu, X., & von Davier, M. (2008). \emph{Fitting the structured general diagnostic
model to NAEP data}. ETS Research Report ETS RR-08-27. Princeton, ETS.
}
%\author{
%Alexander Robitzsch
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Cognitive diagnostic models for dichotomous data can be estimated
with \code{\link{din}} (DINA or DINO model) or \code{\link{gdina}} 
(GDINA model, which contains many CDMs as special cases).

For assessment of model fit see \code{\link{modelfit.cor.din}} and 
\code{\link{anova.gdm}}.

For the estimation of the multidimensional
latent class item response model see the \pkg{MultiLCIRT}
and \pkg{sirt} packages.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\examples{
#############################################################################
# EXAMPLE 1: Fraction Dataset 1
#      Unidimensional Models for dichotomous data
#############################################################################

data( data.fraction1 )
dat <- data.fraction1$data
theta.k <- seq( -6 , 6 , len=15 )   # discretized ability

#***
# Model 1: Rasch model (normal distribution)
mod1 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , skillspace="normal" ,
        centered.latent=TRUE)
summary(mod1)

#***
# Model 2: Rasch model (log-linear smoothing)
# set the item difficulty of the 8th item to zero
b.constraint <- matrix( c(8,1,0) , 1 , 3 )  
mod2 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , 
          skillspace="loglinear" , b.constraint=b.constraint  )
summary(mod2)

#***
# Model 3: 2PL model
mod3 <- gdm( dat , irtmodel="2PL" , theta.k=theta.k , 
          skillspace="normal" , standardized.latent=TRUE  )
summary(mod3)

\dontrun{
#***
# Model 4: include quadratic term in item response function
#   using the argument decrease.increments=TRUE leads to a more
#   stable estimate
thetaDes <- cbind( theta.k , theta.k^2 )
colnames(thetaDes) <- c( "F1" , "F1q" )
mod4 <- gdm( dat , irtmodel="2PL" , theta.k=theta.k , 
          thetaDes = thetaDes , skillspace="normal" ,
          standardized.latent=TRUE , decrease.increments=TRUE)
summary(mod4)

#***
# Model 5: step function for ICC
#          two different probabilities theta < 0 and theta > 0
thetaDes <- matrix( 1*(theta.k>0) , ncol=1 )
colnames(thetaDes) <- c( "Fgrm1" )
mod5 <- gdm( dat , irtmodel="2PL" , theta.k=theta.k , 
          thetaDes = thetaDes , skillspace="normal" )
summary(mod5)

#***
# Model 6: DINA model with din function
mod6 <- din( dat , q.matrix = matrix( 1 , nrow=ncol(dat),ncol=1 ) )
summary(mod6)

#***
# Model 7: Estimating a version of the DINA model with gdm
theta.k <- c(-.5,.5)
mod7 <- gdm( dat , irtmodel="2PL" , theta.k=theta.k , skillspace="loglinear" )
summary(mod7)

#############################################################################
# EXAMPLE 2: Cultural Activities - data.Students
#      Unidimensional Models for polytomous data
#############################################################################

data( data.Students )
dat <- data.Students[ , grep( "act" , colnames(data.Students) ) ]
theta.k <- seq( -4 , 4 , len=11 )   # discretized ability

#***
# Model 1: Partial Credit Model (PCM)
mod1 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , skillspace="normal" ,
           centered.latent=TRUE)
summary(mod1)

#***
# Model 1b: PCM using frequency patterns
mod1b <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , skillspace="normal" ,
           centered.latent=TRUE , use.freqpatt=TRUE)
summary(mod1b)

#***
# Model 2: PCM with two groups
mod2 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , 
            group=data.Students$urban + 1 , skillspace="normal" ,
            centered.latent=TRUE)
summary(mod2)

#***
# Model 3: PCM with loglinear smoothing
b.constraint <- matrix( c(1,2,0) , ncol=3 )
mod3 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , 
    skillspace="loglinear" , b.constraint=b.constraint )
summary(mod3)

#***
# Model 4: Model with pre-specified item weights in Q-matrix
Qmatrix <- array( 1 , dim=c(5,1,2) )
Qmatrix[,1,2] <- 2
Qmatrix[c(2,4),1,1] <- .74
Qmatrix[c(2,4),1,2] <- 2.3
# score for category 1 is 1.2 and for category 2 is 1.8
mod4 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , Qmatrix=Qmatrix ,
           skillspace="normal"  , centered.latent=TRUE)
summary(mod4)

#***
# Model 5: Generalized partial credit model
mod5 <- gdm( dat , irtmodel="2PL" , theta.k=theta.k ,  
          skillspace="normal" , standardized.latent=TRUE )
summary(mod5)

#***
# Model 6: Item-category slope estimation
mod6 <- gdm( dat , irtmodel="2PLcat" , theta.k=theta.k ,  
          skillspace="normal" , standardized.latent=TRUE ,
          decrease.increments=TRUE)        
summary(mod6)

#***
# Models 7: items with different number of categories
dat0 <- dat
dat0[ paste(dat0[,1]) == 2 , 1 ] <- 1 # 1st item has only two categories
dat0[ paste(dat0[,3]) == 2 , 3 ] <- 1 # 3rd item has only two categories

# Model 7a: PCM
mod7a <- gdm( dat0 , irtmodel="1PL" , theta.k=theta.k ,  
           centered.latent=TRUE )        
summary(mod7a)

# Model 7b: Item category slopes
mod7b <- gdm( dat0 , irtmodel="2PLcat" , theta.k=theta.k ,  
            standardized.latent=TRUE , decrease.increments=TRUE )        
summary(mod7b)

#############################################################################
# EXAMPLE 3: Fraction Dataset 2
#      Multidimensional Models for dichotomous data
#############################################################################

data( data.fraction2 )
dat <- data.fraction2$data
Qmatrix <- data.fraction2$q.matrix3

#***
# Model 1: One-dimensional Rasch model
theta.k <- seq( -4 , 4 , len=11 )   # discretized ability
mod1 <- gdm( dat , irtmodel = "1PL" , theta.k=theta.k ,
           centered.latent=TRUE) 
summary(mod1)

#***
# Model 2: One-dimensional 2PL model
mod2 <- gdm( dat , irtmodel = "2PL" , theta.k=theta.k ,
           standardized.latent=TRUE) 
summary(mod2)

#***
# Model 3: 3-dimensional Rasch Model (normal distribution)
mod3 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , 
            Qmatrix=Qmatrix , centered.latent=TRUE , 
            globconv=5*10^(-3) , conv=10^(-4) , maxiter=10 )
summary(mod3)            

#***
# Model 4: 3-dimensional Rasch model (loglinear smoothing)
# set some item parameters of items 4,1 and 2 to zero
b.constraint <- cbind( c(4,1,2) , 1 , 0 )
mod4 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , 
            Qmatrix=Qmatrix , b.constraint=b.constraint , 
            skillspace="loglinear" , globconv= 5*10^(-2) , conv=10^(-3) )
summary(mod4)            

#***
# Model 5: define a different theta grid for each dimension
theta.k <- list( "Dim1"= seq( -5 , 5 , len=11 ) , 
                 "Dim2"= seq(-5,5,len=8) , 
                 "Dim3"=seq( -3,3,len=6) )
mod5 <- gdm( dat , irtmodel="1PL" , theta.k=theta.k , 
            Qmatrix=Qmatrix , b.constraint=b.constraint , 
            skillspace="loglinear" , globconv= 5*10^(-2) , conv=10^(-3) )
summary(mod5)            

#***
# Model 6: multdimensional 2PL model (normal distribution)
theta.k <- seq( -5 , 5 , len=13 )
a.constraint <- cbind( c(8,1,3) , 1:3 , 1 , 1 ) # fix some slopes to 1
mod6 <- gdm( dat , irtmodel="2PL" , theta.k=theta.k , 
            Qmatrix=Qmatrix , centered.latent=TRUE , 
            a.constraint=a.constraint , decrease.increments=TRUE , 
            skillspace="normal" , maxiter=400)
summary(mod6)

#***
# Model 7: multdimensional 2PL model (loglinear distribution)
a.constraint <- cbind( c(8,1,3) , 1:3 , 1 , 1 )
b.constraint <- cbind( c(8,1,3) , 1 , 0 )
mod7 <- gdm( dat , irtmodel="2PL" , theta.k=theta.k , 
            Qmatrix=Qmatrix , b.constraint=b.constraint , 
            a.constraint=a.constraint , decrease.increments=FALSE , 
            skillspace="loglinear" , maxiter=400)
summary(mod7)

#############################################################################
# SIMULATED EXAMPLE 4: Unidimensional latent class 1PL IRT model
#############################################################################

# simulate data
set.seed(754)
I <- 20     # number of items
N <- 2000   # number of persons
theta <- c( -2 , 0 , 1 , 2 )
theta <- rep( theta , c(N/4,N/4 , 3*N/8 , N/8)  ) 
b <- seq(-2,2,len=I)
dat <- sim.raschtype( theta=theta , b=b )

theta.k <- seq(-1 , 1 , len=4)      # initial vector of theta
# estimate model
mod1 <- gdm( dat , theta.k = theta.k , skillspace="est" , irtmodel="1PL",
        centerintercepts=TRUE , maxiter=200)
summary(mod1)

#############################################################################
# SIMULATED EXAMPLE 5: Multidimensional latent class IRT model
#############################################################################

# simulate data
set.seed(754)
I <- 13     # number of items
N <- 2400   # number of persons
theta <- c( -2 , 0 , 1 , 2 )
theta <- rep( theta , c(N/4,N/4 , 3*N/8 , N/8)  ) 
b <- seq(-2,2,len=I)
dat1 <- sim.raschtype( theta=theta , b=b )

theta <- c( -3 , 0 , 1.5 , 2 )
theta <- rep( theta , c(N/4,N/4 , 3*N/8 , N/8)  ) 
dat2 <- sim.raschtype( theta=theta , b=b )
colnames(dat2) <- gsub( "I" , "U" , colnames(dat2))
dat <- cbind( dat1 , dat2 )

# define Q-matrix
Qmatrix <- matrix(0,2*I,2)
Qmatrix[ cbind( 1:(2*I) , rep(1:2 , each=I) ) ] <- 1

theta.k <- seq(-1 , 1 , len=4)      # initial matrix
theta.k <- cbind( theta.k , theta.k )
colnames(theta.k) <- c("Dim1","Dim2")

# estimate model
mod2 <- gdm( dat , theta.k = theta.k , skillspace="est" , irtmodel="1PL",
        Qmatrix=Qmatrix , centerintercepts=TRUE , maxiter=200)
summary(mod2)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{General diagnostic model}
\keyword{summary}
%% \keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
