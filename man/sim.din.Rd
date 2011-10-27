\name{sim.din}
\alias{sim.din}

\title{
  Data Simulation Tool for DINA, DINO and mixed DINA and DINO Data
}

\description{
  \code{sim.din} can be used to simulate dichotomous response data according to a CDM 
  model.  The model type DINA or DINO can be specified item wise.  The number of items,
  the sample size, and two parameters for each item, the slipping and guessing errors,
  can be set explicitly.  
}

\usage{
  sim.din(N, q.matrix, guess = rep(0.2, nrow(q.matrix)),
    slip = guess, mean = rep(0, ncol(q.matrix)), Sigma =                          
    diag(ncol(q.matrix)), rule = "DINA")
}

\arguments{
  \item{N}{a required numeric value specifying the number \eqn{N} of requested
    response patterns.  }

  \item{q.matrix}{a required binary \eqn{J} times \eqn{K} matrix describing which
    of the \eqn{K} attributes are required, coded by 1, and which attributes 
    are not required, coded by 0, to master the items.   }
    
  \item{guess}{ an optional vector of guessing parameters.  Default is 
    0.2 for each item.  }
  
  \item{slip}{ an optional vector of slipping parameters.  Default is 
    0.2 for each item.  }
  
  \item{mean}{a numeric vector of length \code{ncol(q.matrix)} indicating 
    the latent trait of the respondents.  Default is \code{rep(0, length = ncol(q.matrix))}.  
    That is, having a probability of \code{0.5} for possessing each of the attributes.  }

  \item{Sigma}{a matrix of dimension \code{ncol(q.matrix)} times \code{ncol(q.matrix)}
     specifying the covariance matrix of atrribute possessions.  Default is
     \code{diag( 1, ncol(q.matrix))}.  That is, the possession of the attributes
     is not assumed to be correlated.  }

  \item{rule}{an optional character string or vector of character strings 
  	specifying the model rule that is used.  The character strings must be 
  	of \code{"DINA"} or \code{"DINO"}.  If a vector of character strings is 
  	specified, implying an itemwise condensation rule, the vectore must 
  	be of length \eqn{J}, which is the number of used items.  The default is
    the condensation rule \code{"DINA"} for all items.  }
}

\value{
  A matrix of simulated dichotoumos response data according to the specified CDM model.  
}

\author{
  Alexander Robitzsch, Thomas Kiefer, Ann Cathrice George, Ali Uenlue 

  Maintainer: Alexander Robitzsch <a.robitzsch@bifie.at>,
              Thomas Kiefer <t.kiefer@bifie.at>,
              Ann Cathrice George <george@statistik.tu-dortmund.de>,
              Ali Uenlue <uenlue@statistik.tu-dortmund.de>  
}

\references{
  Rupp, A. A., Templin, J. L. and Henson, R. A. (2010) \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}. New York: The Guilford
  Press.
}

\seealso{
  \code{\link{Data-sim}} for artificial date set simulated with the help of this
  method; \code{\link{plot.din}}, the S3 method for plotting objects of
  the class \code{din}; \code{\link{summary.din}}, the S3
  method for summarizing objects of the class \code{din}, which
  creates objects of the class \code{summary.din};
  \code{\link{print.summary.din}}, the S3 method for printing
  objects of the class \code{summary.din}; \code{\link{din}},
  the main function for DINA and DINO parameter estimation,
  which creates objects of the class \code{din}.  See also
  \code{\link{CDM-package}} for general information about this package.  
}

\examples{
## simulate DINA and DINO data according to a tetrachoric correlation

# define Q-matrix for 4 items and 2 attributes
q.matrix <- matrix(c(1,0,0,1,1,1,1,1), ncol = 2, nrow = 4)

# Slipping parameters 
slip <- c(0.2,0.3,0.4,0.3)

# Guessing parameters
guess <- c(0,0.1,0.05,0.2)

set.seed(1567) # fix random numbers
dat1 <- sim.din(N = 200, q.matrix, slip, guess,
  # Possession of the attributes with high probability 
  mean = c(0.5,0.2), 
  # Possession of the attributes is weakly correlated
  Sigma = matrix(c(1,0.2,0.2,1), ncol=2), rule = "DINA")
head(dat1)

set.seed(15367) # fix random numbers
dat2 <- sim.din(N = 200, q.matrix, slip, guess, mean = 
  c(0.5,0.2), Sigma = matrix(c(1,0.2,0.2,1), ncol=2),
  rule = "DINO")
head(dat2)
}