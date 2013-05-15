\name{gdina}
\alias{gdina}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for estimating the generalized DINA (GDINA) model (de la Torre, 2011)
}
\description{
    This function implements the generalized DINA model 
    (GDINA; de la Torre, 2011). See the paper
    for details about estimable cognitive diagnosis models. 
    In addition, multiple group estimation 
    is also possible using the \code{gdina} function.
}
\usage{
gdina(data, q.matrix, conv.crit=0.0001, dev.crit=.1 ,  maxit=1000, 
    linkfct="identity", Mj=NULL, group=NULL , method="WLS" , 
    delta.designmatrix=NULL, delta.basispar.lower=NULL, 
    delta.basispar.upper=NULL, delta.basispar.init=NULL, 
    zeroprob.skillclasses=NULL, reduced.skillspace=TRUE , Z.skillspace=NULL,
    weights=rep(1, nrow(data)), rule="GDINA", progress=TRUE,
    progress.item=FALSE , ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{A required \eqn{N} times \eqn{J} data matrix 
    containing the binary responses, 0 or 1, of \eqn{N} respondents to \eqn{J}
    test items, where 1 denotes a correct anwer and 0 an incorrect one. The 
    \eqn{n}th row of the matrix represents the binary response pattern of respondent
    \eqn{n}.  \code{NA} values are allowed.  }
    
  \item{q.matrix}{A required binary \eqn{J} times \eqn{K} containing the 
    attributes not required or required, 0 or 1, to master the items.  The
    jth row of the matrix is a binary indicator vector indicating which 
    attributes are not required (coded by 0) and which attributes are 
    required (coded by 1) to master item \eqn{j}.  }

\item{conv.crit}{
    Convergence criterion for maximum absolut change in item parameters
        }
\item{dev.crit}{
    Convergence criterion for maximum absolut change in deviance
        }        
\item{maxit}{
    Maximum number of iterations
    }
\item{linkfct}{
    A string which indicates the link function for the GDINA model. 
    Options are \code{"identity"} (identity link),
    \code{"logit"} (logit link) and \code{"log"} (log link). The default is the 
    \code{"identity"} link. Note that the link function is chosen
    for the whole model (i.e. for all items).
}
\item{Mj}{
A list of design matrices and labels for each item. 
The definition of \code{Mj} follows the
defintion of \eqn{M_j} in de la Torre (2011). 
Please study the value \code{Mj} of the function in 
default analysis. See Example 3.
}
\item{group}{A vector of group identifiers for multiple group 
    estimation. Default is \code{NULL} (no multiple group estimation).
        }
\item{method}{Estimation method for item parameters as described 
    in de la Torre (2011). The default \code{"WLS"}
    weights probabilities attribute classes by a weighting 
    matrix \eqn{W_j} of expected frequencies,
    whereas the method \code{"ULS"} perform unweighted least squares
    estimation on expected frequencies.
    }
\item{delta.designmatrix}{
    A design matrix for restrictions on delta. See Example 4.
    }
\item{delta.basispar.lower}{
    Lower bounds for delta basis parameters.
}
  \item{delta.basispar.upper}{
    Upper bounds for delta basis parameters.
}
  \item{delta.basispar.init}{
    An optional vector of starting values for the basis parameters of delta. 
    This argument only applies when using a designmatrix for delta, 
    i.e. \code{delta.designmatrix} is not NULL.
}

\item{zeroprob.skillclasses}{an optional vector of integers which indicates which skill
  classes should have zero probability. Default is NULL 
    (no skill classes with zero probability).}
    

\item{reduced.skillspace}{Logical which indicates if the latent class skill space should be
        dimensionally reduced (see Xu & von Davier, 2008). Default is \code{TRUE}.
        The dimensional reduction is only well defined for more than three skills.
        The dimensional reduction is not yet implemented for multiple groups.
        If the argument \code{zeroprob.skillclasses} is not \code{NULL}, then
        \code{reduced.skillspace} is set to \code{FALSE}.}

\item{Z.skillspace}{A user specified design matrix for the skill space reduction
        as described in Xu and von Davier (2008). See in the Examples section for 
        applications. See Example 6.
                }

\item{weights}{
An optional vector of sample weights.
}
\item{rule}{
    A string or a vector of itemwise condensation rules. Allowed entries are \code{GDINA}, \code{DINA}, 
    \code{DINO} and \code{ACDM} (additive cognitive diagnostic model). The rule \code{GDINA1} applies
    only main effects in the GDINA model which is equivalent to \code{ACDM}. The rule \code{GDINA2} applies
    to all main effects and second-order interactions of the attributes.
    If \code{rule} is a string, the condensation rule applies to all items. If \code{rule} is a vector,
    condensation rules can be specified itemwise. The default is \code{GDINA} for all items.
}
  \item{progress}{
    Display progress on the \R console?
}
  \item{progress.item}{
    Display itemwise progress
        }
  \item{\dots}{
    Further arguments to be passed
        }
}
\details{
    The estimation is based on an EM algorithm as described in de la Torre (2011). 
    Item parameters are contained in the \code{delta} vector which is a list where
    the \eqn{j}th entry corresponds to item parameters of the \eqn{j}th item. 
    
    Assume that two skills \eqn{\alpha_1} and \eqn{\alpha_2} are required for 
    mastering item \eqn{j}. Then the GDINA model can be written as
    \deqn{ g [ P( X_{nj} = 1 | \alpha_n ) ] = \delta_{j0} + \delta_{j1} \alpha_{n1} +
            \delta_{j2} \alpha_{n2} + \delta_{j12} \alpha_{n1} \alpha_{n2}
        }
which is a two-way GDINA-model (the \code{rule="GDINA2"} specification) with a 
link function \eqn{g}. If the specification
\code{ACDM} is chosen, then \eqn{\delta_{j12}=0}. The DINA model (\code{rule="DINA"})
assumes \eqn{ \delta_{j1} = \delta_{j2} = 0}.
    }
\value{
An object of class \code{gdina} with following entries  
\item{coef}{Item parameters}
\item{delta}{Basis item parameters}
\item{se.delta}{Standard errors of basis item parameters}
\item{itemfit.rmsea}{The RMSEA item fit index (see \code{\link{itemfit.rmsea}}).
            }
\item{mean.rmsea}{Mean of RMSEA item fit indexes.
            }            
\item{loglike}{Log-likelihood}
\item{deviance}{Deviance}
\item{G}{Number of groups}
\item{N}{Sample size}
\item{AIC}{AIC}
\item{BIC}{BIC}
\item{CAIC}{CAIC}
\item{Npars}{Total number of parameters}
\item{Nipar}{Number of item parameters}
\item{Nskillpar}{Number of parameters for skill class distribution}
\item{Nskillclasses}{Number of skill classes}
\item{varmat.delta}{Covariance matrix of \eqn{\delta} item parameters}
\item{varmat.plaj}{}
\item{posterior}{Individual posterior distribution}
\item{like}{Individual likelihood}
\item{data}{Original data}
\item{q.matrix}{Used Q matrix}
\item{pattern}{Individual patterns, individual MLE and MAP classifications
    and their corresponding probabilities}
\item{attribute.patt}{Probabilities of skill classes}
\item{skill.patt}{Marginal skill probabilities}
\item{subj.pattern}{Individual subject pattern}
\item{attribute.patt.splitted}{Splitted attribute pattern}
\item{pjk}{Array of item response probabilities}
\item{Mj}{Design matrix \eqn{M_j} in GDINA algorithm (see de la Torre, 2011)}  
\item{Aj}{Design matrix \eqn{A_j} in GDINA algorithm (see de la Torre, 2011)}
\item{delta.designmatrix}{Designmatrix for item parameters}
\item{reduced.skillspace}{A logical if skillspace reduction was performed}
\item{Z.skillspace}{Design matrix for skillspace reduction}
\item{beta}{Parameters \eqn{\delta} for skill class representation}
\item{covbeta}{Standard errors of \eqn{\delta} parameters}
\item{model.type}{}
\item{iter}{Number of iterations}
\item{\dots}{}
    }
\references{
  de la Torre, J. (2011) The generalized {DINA} model framework. 
  \emph{Psychometrika}, \bold{76}, 179--199.    
  
  Xu, X. & von Davier, M. (2008). \emph{Fitting the structured general diagnostic
  model to NAEP data}. ETS Research Report ETS RR-08-27. Princeton, ETS.
    }
\author{
Alexander Robitzsch, Thomas Kiefer
    }
    
    
\note{    
    The function \code{\link{din}} does not allow for multiple group estimation.
    Use this \code{gdina} function instead and choose the appropriate \code{rule="DINA"} 
    as an argument.
    
    Standard error calculation in analyses which use sample weights or 
    designmatrix for delta parameters (\code{delta.designmatrix!=NULL}) is not yet 
    correctly implemented. Please use replication methods instead.
    
    
    }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
See also the \code{\link{din}} function (for DINA and DINO estimation).

For assessment of model fit see \code{\link{modelfit.cor.din}} and 
\code{\link{anova.gdina}}.

See \code{\link{sim.gdina}} for simulating the GDINA model.
}


\examples{
###################################################################
# EXAMPLE 1: Simulated DINA data
#    different condensation rules 
###################################################################
data(sim.dina)

#***
# Model 1: estimation of the GDINA model (identity link)
mod1 <- gdina( data = sim.dina ,  q.matrix = sim.qmatrix , maxit=700)
summary(mod1)

#***
# Model 2: estimation of the DINA model with gdina function
mod2 <- gdina( data = sim.dina ,  q.matrix = sim.qmatrix , rule="DINA")
summary(mod2)

#***
# Model 3: compare results with din function
mod2b <- din( data = sim.dina ,  q.matrix = sim.qmatrix , rule="DINA")
summary(mod2b)
cbind( mod2$coef , mod2b$coef )

#***
# Model 4: DINA model with logit link
mod4 <- gdina( data = sim.dina ,  q.matrix = sim.qmatrix , maxit= 20 , 
                rule="DINA" , linkfct = "logit" )
summary(mod4)

#***
# Model 5: DINA model log link
mod5 <- gdina( data = sim.dina ,  q.matrix = sim.qmatrix , maxit=100 , 
                    rule="DINA" , linkfct = "log" )
summary(mod5)

###################################################################
# EXAMPLE 2: Simulated DINO data
#    additive cognitive diagnosis model
#    with different link functions
###################################################################

#***
# Model 1: additive cognitive diagnosis model (ACDM; identity link)
mod1 <- gdina( data=sim.dino,  q.matrix=sim.qmatrix,  
                    rule="ACDM")
summary(mod1)

#***
# Model 2: ACDM logit link
mod2 <- gdina( data=sim.dino, q.matrix=sim.qmatrix,  
                    rule="ACDM", linkfct="logit" )
summary(mod2)

\dontrun{
#***
# Model 3: ACDM log link
mod3 <- gdina( data=sim.dino,  q.matrix=sim.qmatrix,  
                rule="ACDM", linkfct="log" )
summary(mod3)

#***
# Model 4: Different condensation rules per item
I <- 9      # number of items
rule <- rep( "GDINA" , I )
rule[1] <- "DINO"   # 1st item: DINO model
rule[7] <- "GDINA2" # 7th item: GDINA model with first- 
                    #           and second-order interactions
rule[8] <- "ACDM"   # 8ht item: additive CDM
rule[9] <- "DINA"   # 9th item: DINA model
mod4 <- gdina( data=sim.dino, q.matrix=sim.qmatrix, rule=rule )
summary(mod4) }

###################################################################
# EXAMPLE 3: Model with user-specified design matrices
###################################################################

# do a preliminary analysis and modify obtained design matrices
mod0 <- gdina( data = sim.dino ,  q.matrix = sim.qmatrix ,  maxit=1)

# extract default design matrices
Mj <- mod0$Mj
Mj.user <- Mj   # these user defined design matrices are modified.
#~~~
# For the second item, the following model should hold
# X1 ~ V2 + V2*V3
mj <- Mj[[2]][[1]]
mj.lab <- Mj[[2]][[2]]
mj <- mj[,-3]
mj.lab <- mj.lab[-3]
Mj.user[[2]] <- list( mj , mj.lab )
#    [[1]]
#        [,1] [,2] [,3]
#    [1,]    1    0    0
#    [2,]    1    1    0
#    [3,]    1    0    0
#    [4,]    1    1    1
#    [[2]]
#    [1] "0"   "1"   "1-2"    
#~~~
# For the eight item an equality constraint should hold
# X8 ~ a*V2 + a*V3 + V2*V3
mj <- Mj[[8]][[1]]
mj.lab <- Mj[[8]][[2]]
mj[,2] <- mj[,2] + mj[,3]
mj <- mj[,-3]
mj.lab <- c("0" , "1=2" , "1-2" )
Mj.user[[8]] <- list( mj , mj.lab )
Mj.user
mod <- gdina( data = sim.dino ,  q.matrix = sim.qmatrix ,
                    Mj = Mj.user ,  maxit=200 )
summary(mod)

\dontrun{
###################################################################
# EXAMPLE 4: Design matrix for delta parameters
###################################################################

#~~~
# estimate an initial model
mod0 <- gdina( data = sim.dino ,  q.matrix = sim.qmatrix , 
            rule="ACDM" , maxit=1)
# extract coefficients
c0 <- mod0$coef
I <- 9  # number of items
delta.designmatrix <- matrix( 0 , nrow= nrow(c0) , ncol = nrow(c0) )
diag( delta.designmatrix) <- 1
# set intercept of item 1 and item 3 equal to each other
delta.designmatrix[ 7 , 1 ] <- 1 ; delta.designmatrix[,7] <- 0
# set loading of V1 of item1 and item 3 equal
delta.designmatrix[ 8 , 2 ] <- 1 ; delta.designmatrix[,8] <- 0
delta.designmatrix <- delta.designmatrix[ , -c(7:8) ]       
                # exclude original parameters with indices 7 and 8

#***
# Model 1: ACDM with designmatrix
mod1 <- gdina( data = sim.dino ,  q.matrix = sim.qmatrix ,  rule="ACDM" , 
            delta.designmatrix = delta.designmatrix )
summary(mod1)            

#***
# Model 2: Same model, but with logit link instead of identity link function
mod2 <- gdina( data = sim.dino ,  q.matrix = sim.qmatrix ,  rule="ACDM" , 
            delta.designmatrix = delta.designmatrix , 
            maxit=100 , linkfct = "logit")
summary(mod2)}            

###################################################################
# SIMULATED EXAMPLE 5: Multiple group estimation
###################################################################

# simulate data
set.seed(9279)
N1 <- 200 ; N2 <- 100   # group sizes
I <- 10                 # number of items
q.matrix <- matrix(0,I,2)   # create Q matrix
q.matrix[1:7,1] <- 1 ; q.matrix[ 5:10,2] <- 1
# simulate first group
dat1 <- sim.din(N1, q.matrix=q.matrix , mean = c(0,0) )$dat
# simulate second group
dat2 <- sim.din(N2, q.matrix=q.matrix , mean = c(-.3 , -.7) )$dat
# merge data
dat <- rbind( dat1 , dat2 )
# group indicator 
group <- c( rep(1,N1) , rep(2,N2) )

# estimate GDINA model
mod <- gdina( data = dat , q.matrix = q.matrix ,  group= group)
summary(mod)

# estimate DINA model
mod2 <- gdina( data = dat , q.matrix = q.matrix , 
                group= group , rule="DINA")
summary(mod2)                       

\dontrun{
###################################################################
# EXAMPLE 6: User specified reduced skill space
###################################################################

#   Some correlations between attributes should be set to zero.
q.matrix <- expand.grid( c(0,1) , c(0,1) , c(0,1) , c(0,1) )
colnames(q.matrix) <- colnames( paste("Attr" , 1:4 ,sep=""))
q.matrix <- q.matrix[ -1 , ]
Sigma <- matrix( .5 , nrow=4 , ncol=4 )
diag(Sigma) <- 1
Sigma[3,2] <- Sigma[2,3] <- 0 # set correlation of attribute A2 and A3 to zero
dat <- sim.din( N=1000 , q.matrix = q.matrix , Sigma = Sigma)$dat

#~~~ Step 1: initial estimation
mod1a <- gdina( data=dat , q.matrix = q.matrix , maxit= 1 , rule="DINA")
# estimate also "full" model
mod1 <- gdina( data=dat , q.matrix = q.matrix , rule="DINA")

#~~~ Step2: modify designmatrix for reduced skillspace
Z.skillspace <- data.frame( mod1a$Z.skillspace )
# set correlations of A2/A4 and A3/A4 to zero
vars <- c("A2_A3","A2_A4") 
for (vv in vars){ Z.skillspace[,vv] <- NULL }

#~~~ Step 3: estimate model with reduced skillspace
mod2 <- gdina( data=dat , q.matrix = q.matrix , 
        Z.skillspace=Z.skillspace , rule="DINA")

#~~~ eliminate all covariances
Z.skillspace <- data.frame( mod1$Z.skillspace )
colnames(Z.skillspace)
Z.skillspace <- Z.skillspace[ , - 
	grep( "_" , colnames(Z.skillspace ) , fixed=TRUE)]
colnames(Z.skillspace)

mod3 <- gdina( data=dat , q.matrix = q.matrix , 
        Z.skillspace=Z.skillspace , rule="DINA")
summary(mod1); summary(mod2); summary(mod3)}
}



% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Cognitive Diagnosis Models}
\keyword{GDINA}
\keyword{DINA}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
