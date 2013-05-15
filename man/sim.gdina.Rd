\name{sim.gdina}
\alias{sim.gdina}
\alias{sim.gdina.prepare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation of the GDINA model
}
\description{
The function \code{sim.gdina.prepare} creates necessary design matrices
\code{Mj}, \code{Aj} and \code{necc.attr}. In most cases, only the list 
of item parameters \code{delta} must be modified by the user when
applying the simulation function \code{sim.gdina}. The distribution of latent
classes \eqn{\alpha} is represented by an underlying multivariate normal distribution
\eqn{\alpha^\ast} for which a mean vector \code{thresh.alpha} and a 
covariance matrix \code{cov.alpha} must be specified.
Alternatively, a matrix of skill classes \code{alpha}
can be given as an input.
}
\usage{
sim.gdina(n, q.matrix, delta, link = "identity", 
    thresh.alpha=NULL, cov.alpha=NULL, alpha=NULL,
    Mj, Aj, necc.attr)
sim.gdina.prepare( q.matrix )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of persons
}
  \item{q.matrix}{
Q matrix
}
  \item{delta}{
List with \eqn{J} entries where \eqn{J} is the number of items. 
Every list element corresponds to the parameter of an item.
}
  \item{link}{
Link function. Choices are \code{identity} (default), \code{logit} and \code{log}.
}
  \item{thresh.alpha}{
Vector of thresholds (means) of \eqn{\alpha^\ast}
}
  \item{cov.alpha}{
Covariance matrix of \eqn{\alpha^\ast}
}
  \item{alpha}{
Matrix of skill classes if they should not be simulated
}
  \item{Mj}{
Design matrix, see \code{\link{gdina}}
}
  \item{Aj}{
Design matrix, see \code{\link{gdina}}
}
  \item{necc.attr}{
List with \eqn{J} entries containing necessary attributes
for each item
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
The output of \code{sim.gdina} is a list with following entries:
  \item{data}{Simulated item responses}
  \item{alpha}{Data frame with simulated attributes}
  \item{q.matrix}{Used Q matrix}
  \item{delta}{Used delta item parameters}
  \item{Aj}{Design matrices Aj}
  \item{Mj}{Design matrices Mj}
  \item{link}{Used link function}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The function \code{sim.gdina.prepare} possesses the following values as output
in a list: \code{delta}, \code{necc.attr}, \code{Aj} and \code{Mj}.
%    res <- list( "data" = dat , "alpha" = alpha , "q.matrix" = q.matrix ,
%                        "delta" = delta , "Aj" = Aj , "Mj" = Mj , "link" = link )
}
\references{
  de la Torre, J. (2011) The generalized {DINA} model framework. \emph{Psychometrika}, \bold{76}, 179--199.    
}
%\author{
%Alexander Robitzsch, ...
%}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
For estimating the GDINA model see \code{\link{gdina}}.
}
\examples{
##############################################
# Example: Simulating the GDINA model

n <- 50             # number of persons
# define Q matrix
q.matrix <- matrix(  c(1,1,0 ,
                0,1,1,
                1,0,1,
                1,0,0,
                0,0,1,
                0,1,0,
                1,1,1,
                0,1,1,
                0,1,1) , ncol=3 , byrow=TRUE)
# thresholds for attributes alpha^\ast
thresh.alpha <- c( .65 , 0 , -.30 )
# covariance matrix for alpha^\ast
cov.alpha <- matrix(1,3,3)
cov.alpha[1,2] <- cov.alpha[2,1] <- .4
cov.alpha[1,3] <- cov.alpha[3,1] <- .6
cov.alpha[3,2] <- cov.alpha[2,3] <- .8

# prepare design matrix by applying sim.gdina.prepare function
rp <- sim.gdina.prepare( q.matrix )
delta <- rp$delta
necc.attr <- rp$necc.attr
Aj <- rp$Aj
Mj <- rp$Mj
# define delta parameters
# intercept - main effects - second order interactions - ...
str(delta)  # => modify the delta parameter list which contains only zeroes as default
##   List of 9
##    $ : num [1:4] 0 0 0 0
##    $ : num [1:4] 0 0 0 0
##    $ : num [1:4] 0 0 0 0
##    $ : num [1:2] 0 0
##    $ : num [1:2] 0 0
##    $ : num [1:2] 0 0
##    $ : num [1:8] 0 0 0 0 0 0 0 0
##    $ : num [1:4] 0 0 0 0
##    $ : num [1:4] 0 0 0 0
delta[[1]] <- c( .2 , .1 , .15 , .4 )
delta[[2]] <- c( .2 , .3 , .3 , -.2 )
delta[[3]] <- c( .2 , .2 , .2 , 0 )
delta[[4]] <- c( .15 , .6 )
delta[[5]] <- c( .1 , .7 )
delta[[6]] <- c( .25 , .65 )
delta[[7]] <- c( .25 , .1 , .1 , .1 , 0 , 0 , 0 , .25 )
delta[[8]] <- c( .2 , 0 , .3 , -.1 )
delta[[9]] <- c( .2 , .2 , 0 , .3 )

#******************************************
# Now, the "real simulation" is starting
sim.res <- sim.gdina( n=n , q.matrix =q.matrix , delta=delta , link = "identity" , 
            thresh.alpha=thresh.alpha , cov.alpha=cov.alpha ,
                Mj=Mj , Aj=Aj , necc.attr =necc.attr)
# sim.res$data      # data
# sim.res$alpha     # alpha

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{GDINA}
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
