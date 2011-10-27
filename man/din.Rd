\name{din}

\alias{din}
\concept{diagnosis models}
\concept{binary response data}

\title{
  Main Function for Parameter Estimation in Cognitive Diagnosis Models
}

\description{
	\code{din} provides parameter estimation for cognitive
	diagnosis models of the types ``DINA'', ``DINO'' and ``mixed DINA 
  and DINO''.  It is the main function of this package.  
}

\usage{
  din(data, q.matrix, conv.crit = 0.001, maxit = 100, 
	  constraint.guess = NULL, constraint.slip = NULL,
	  guess.init = rep(0.2, ncol(data)), slip.init = guess.init,
	  weights = rep(1, nrow(data)), rule = "DINA", progress = TRUE)
}

\arguments{
  \item{data}{a required \eqn{N} times \eqn{J} data matrix 
    containing the binary responses, 0 or 1, of \eqn{N} respondants to \eqn{J}
    test items, where 1 denotes a correct anwer and 0 an incorrect one. The 
    nth row of the matrix represents the binary response pattern of respondant
    n.  \code{NA} values are allowed.  }
    
  \item{q.matrix}{a required binary \eqn{J} times \eqn{K} containing the 
    attributes not required or required, 0 or 1, to master the items.  The
    jth row of the matrix is a binary indicator vector indicating which 
    attributes are not required (coded by 0) and which attributes are 
    required (coded by 1) to master item \eqn{j}.  }
    
  \item{conv.crit}{termination criterion of iterations in the parameter 
    estimation process.  Iteration ends if the maximal change in parameter
    estimates is below this value.  }
    
  \item{maxit}{maximal number of iterations in the estimation process.  }
    
  \item{constraint.guess}{an optional matrix of fixed guessing
    parameters.  The first column of this matrix indicates the numbers of the
    items whose guessing parameters are fixed and the second 
    column the values the guessing parameters are fixed to.  }
    
  \item{constraint.slip}{an optional matrix of fixed slipping
    parameters.  The first column of this matrix indicates the numbers of the 
    items whose guessing parameters are fixed and the second column 
    the values the guessing parameters are fixed to.  }
    
  \item{guess.init}{an optional initial vector of guessing parameters.  
    Guessing parameters are bounded between 0 and 1.  }
    
  \item{slip.init}{an optional initial vector of guessing parameters.  
    Slipping parameters are bounded between 0 and 1.  }
    
  \item{weights}{an optional vector of weights for the response pattern. 
  	Non-integer weights allow for different sampling schemes.  }
    
  \item{rule}{an optional character string or vector of character strings 
  	specifying the model rule that is used. The character strings must be 
  	of \code{"DINA"} or \code{"DINO"}.  If a vector of character strings is 
  	specified, implying an item wise condensation rule, the vector must 
  	be of length \eqn{J}, which is the number of items.  The default is 
    the condensation rule \code{"DINA"} for all items.  }
         
  \item{progress}{an optional logical indicating whether the function
    should print the progress of iteration in the estimation process.  } 
}

\details{
  In the CDM DINA (deterministic-input, noisy-and-gate; \cite{de la Torre and
  Douglas, 2004}) and DINO (deterministic-input, noisy-or-gate; \cite{Templin and
  Henson, 2006}) models endorsement probabilities are modeled 
  based on guessing and slipping parameters, given the different skill
  patterns.  The probability of respondent \eqn{n} for solving item \eqn{j} is
  calculated as a function of the respondent's latent response \eqn{\eta_{nj}}{eta_{nj}}
  and the guessing and slipping rates \eqn{g_j} and \eqn{s_j} for item
  \eqn{j} conditional on the respondent's skill pattern \eqn{\alpha_n}{alpha_n}:
  \deqn{P_j(\alpha_n) = P(X_{nj} = 1 | \alpha_n) = g_j^{(1- \eta_{nj})}
  (1 - s_j) ^{\eta_{nj}}.  }{P_j(alpha_n) = P(X_{nj} = 1 | alpha_n) = g_j^{(1- eta_{nj})}
  (1 - s_j) ^{eta_{nj}}.  }
  The respondent's latent response \eqn{\eta_{nj}}{eta_{nj}} is a binary number,
  0 or 1, indicating absence or presence of all (\code{rule = "DINO"}) 
  or at least one (\code{rule = "DINO"}) required skill(s) for 
  item \eqn{j}, respectively.  
  
  DINA and DINO parameter estimation is performed by
  maximization of the marginal likelihood of the data.  The
  a priori distribution of the skill vectors is a uniform distribution.  
  The implementation follows the EM algorithm by \cite{de la Torre (2009)}.  
  
  An additional condition in parameter estimation in DINA and
  DINO models is 
  	\deqn{g_j < 1 - s_j}
  for each item,
  that is, the chance of guessing an item is supposed to be smaller 
  than the chance of mastering the required skills for that item 
  and not slipping.  However, the EM algorithm
  needs not satisfy that constraint.  In that cases there will be a
  warning during the estimation algorithm.  Possible problem solving
  strategies are to adjust the convergence criteria \code{conv.crit},
  \code{maxit}, \code{guess.init} and \code{slip.init} or to put
  constraints on the guessing and slipping parameters (\code{constraint.guess}
  and \code{constraint.slip}) of the items that violate the additional condition.  

  The function \code{din} returns an object of the class
  \code{din} (see \sQuote{Value}), for which \code{plot},
  \code{print}, and \code{summary} methods are provided;
  \code{\link{plot.din}}, \code{\link{print.din}}, and
  \code{\link{summary.din}}, respectively.  
}

\value{
  \item{coef}{a dataframe giving for each item condensation rule, the 
    estimated guessing and slipping parameters and their standard errors.  
    All entries are rounded to 3 digits.  }
  \item{guess}{a dataframe giving the estimated guessing parameters
    and their standard errors for each item.  }
  \item{slip}{a dataframe giving the estimated slipping parameters
    and their standard errors for each item.  }
  \item{loglike}{a numeric giving the value of the maximized
    log likelihood.  }
  \item{AIC}{a numeric giving the AIC value of the model.  }
  \item{BIC}{a numeric giving the BIC value of the model.  }
  \item{posterior}{a matrix given the posterior skill distribution
    for all respondents.  The nth row of the matrix gives the probabilities for 
    respondent n to possess skills 1 to \code{K}.  }
  \item{like}{a matrix giving the values of the maximized likelihood
    for all respondents.  }
  \item{data}{the input matrix of binary response data.  }
  \item{q.matrix}{the input matrix of the required attributes.  }
  \item{pattern}{a matrix giving the frequency of observed response patterns stored
    in \code{item.}\cr\code{patt.split}, the attribute classes leading to highest endorsement 
    probability for the respective response pattern (\code{mle.est}) with the 
    corresponding posterior class probability (\code{mle.post}), the attribute 
    classes having the highest occurrence probability given the response pattern 
    (\code{map.est}) with the corresponding posterior class probability 
    (\code{mle.post}), and the estimated posterior for each response pattern.  }
  \item{attribute.patt}{a data frame giving the estimated occurrence
    probabilities of the skill classes and the expected frequency of the
    attribute classes given the model.  }
  \item{skill.patt}{a matrix given the population prevalences of the
    skills.  }
  \item{subj.pattern}{a vector of strings indicating the item response
    pattern for each subject.  }
  \item{attribute.patt.splitted}{a dataframe giving the response
    pattern of the respondents.  }
  \item{display}{a character giving the model specified under 
    \code{rule}.  }
  \item{item.patt.split}{a matrix giving the splitted response pattern.  }
  \item{item.patt.freq}{a numeric vector given the frequencies of the response
    pattern in \code{item.patt.split}.  }
}

\references{
  de la Torre, J. (2009) Dina model parameter estimation:
  A didactic. \emph{Journal of Educational and Behavioral
  Statistics}, \bold{34}, 115--130.
  
  de la Torre, J. and Douglas, J. (2004) Higher-order latent trait models 
  for cognitive diagnosis. \emph{Psychometrika}, \bold{69}, 333--353.    
  
  Rupp, A. A., Templin, J. and Henson, R. A. (2010) \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}.  New York: The Guilford
  Press.
  
  Templin, J. and Henson, R. (2006) Measurement of
  psychological disorders using cognitive diagnosis
  models. \emph{Psychological Methods}, \bold{11}, 287--305.
}

\author{
  Alexander Robitzsch \email{a.robitzsch@bifie.at}
  Based on EM Algorithm by J. de la Torre.
  
  Thomas Kiefer, Ann Cathrice George, Ali Uenlue 

  Maintainer: Alexander Robitzsch <a.robitzsch@bifie.at>,
              Thomas Kiefer <t.kiefer@bifie.at>,
              Ann Cathrice George <george@statistik.tu-dortmund.de>,
              Ali Uenlue <uenlue@statistik.tu-dortmund.de> 
}

\note{
  The calculation of standard errors using sampling weights which
  represent multistage sampling schemes is not correct.  Please use
  replication methods (like Jackknife) instead.  
}

\seealso{
  \code{\link{plot.din}}, the S3 method for plotting objects of
  the class \code{din}; \code{\link{print.din}}, the S3 method
  for printing objects of the class \code{din};
  \code{\link{summary.din}}, the S3 method for summarizing objects
  of the class \code{din}, which creates objects of the class
  \code{summary.din}; \code{\link{din}}, the main function for
  DINA and DINO parameter estimation, which creates objects of the class
  \code{din}.  See also \code{\link{CDM-package}} for general
  information about this package.  
}

\examples{
##
## (1) examples based on dataset fractions.subtraction.data
##

## dataset fractions.subtraction.data and corresponding Q-Matrix
fraction.subtraction.data
fraction.subtraction.qmatrix

## Misspecification in parameter specification for method din()
## leads to warnings and terminates estimation procedure. E.g.,

# See Q-Matrix specification
fractions.dina.warning1 <- din(data = fraction.subtraction.data,
  q.matrix = t(fraction.subtraction.qmatrix)) 
  
# See guess.init specification
fractions.dina.warning2 <- din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, guess.init = rep(1.2,
  ncol(fraction.subtraction.data)))
  
# See rule specification   
fractions.dina.warning3 <- din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = c(rep("DINA",
  10), rep("DINO", 9)))

## Parameter estimation of DINA model
# rule = "DINA" is default
fractions.dina <- din(data = fraction.subtraction.data,
  q.matrix = fraction.subtraction.qmatrix, rule = "DINA")
fractions.dina # see print.din

attributes(fractions.dina)
str(fractions.dina)	  

## For instance accessing the guessing parameters through
## assignment
fractions.dina$guess

## corresponding summaries, including diagnostic accuracies,
## summary of skill pattern distribution and information 
## criteria AIC and BIC
summary(fractions.dina)

## In particular, accessing detailed summary through assignment
detailed.summary.fs <- summary(fractions.dina)
str(detailed.summary.fs)

## Diagnostic accuracy of item 8 seems to low. This is also
## visualized in the first plot 
plot(fractions.dina)

## The reason therefore is a high guessing parameter
round(fractions.dina$guess[,1], 2)

## Set an upper boundary for the guessing parameter of 
## item 5, 8 and 9
fractions.dina.bound <- din(data = fraction.subtraction.data, 
  q.matrix = fraction.subtraction.qmatrix, constraint.guess =
  matrix(c(5,8,9, rep(0.2, 3)), ncol = 2))
fractions.dina.bound
detailed.summary.fs.bound <- summary(fractions.dina.bound)

## This improves the diagnostic accuracies
summary(detailed.summary.fs$IDI[1,])
summary(detailed.summary.fs.bound$IDI[1,])

## The second plot shows the expected (MAP) and observed skill 
## probabilities. The third plot visualizes the skill pattern
## occurrence probabilities; Only the 'highest' are labeled; it
## is obvious that the skill class '11111111' (all skills are
## mastered) is the most probable in this population. The fourth
## plot shows the skill probabilities conditional on response
## patterns; in this population the skills 3 and 6 seem to be
## mastered easier than the others. The fifth plot shows the
## skill probabilities conditional on a specified response
## pattern; it is shown whether a skill is mastered (above 
## .5+'uncertainty') unclassifiable (within the boundaries) or
## not mastered (below .5-'uncertainty'). In this case, the
## fifteenth respondent was chosen; if no response pattern is 
## specified, the plot will not be shown (of course)
pattern <- paste(fraction.subtraction.data[15,], collapse = "")

#uncertainty = 0.1, highest = 0.05 are default
plot(fractions.dina.bound, uncertainty = 0.1, highest = 0.05, 
  pattern = pattern)

##
## (2) examples based on dataset sim.dina
##

# DINA Model
d1 <- din(sim.dina, q.matr = sim.qmatrix, rule = "DINA",
  conv.crit = 0.01, maxit = 500, progress = TRUE)
summary(d1)

# Mixed DINA and DINO Model
d1b <- din(sim.dina, q.matr = sim.qmatrix, rule = 
  c(rep("DINA", 7), rep("DINO", 2)), conv.crit = 0.01,
  maxit = 500, progress = FALSE)
summary(d1b)

# DINO Model
d2 <- din(sim.dina, q.matr = sim.qmatrix, rule = "DINO",
  conv.crit = 0.01, maxit = 500, progress = FALSE)
summary(d2)

# Comparison of DINA and DINO estimates
lapply(list("guessing" = rbind("DINA" = d1$guess[,1],
  "DINO" = d2$guess[,1]), "slipping" = rbind("DINA" = 
  d1$slip[,1], "DINO" = d2$slip[,1])), round, 2)

# Comparison of the information criteria
c("DINA"=d1$AIC, "MIXED"=d1b$AIC, "DINO"=d2$AIC)

# following estimates:
d1$coef            # guessing and slipping parameter
d1$guess           # guessing parameter
d1$slip            # slipping parameter
d1$skill.patt      # probabilities for skills
d1$attribute.patt  # attribute pattern with probabilities
d1$subj.pattern    # pattern per subject

# posterior probabilities for every response pattern
d1$posterior       

##
## (3) examples based on dataset sim.dino
##

# DINO Estimation
d3 <- din(sim.dino, q.matr = sim.qmatrix, rule = "DINO",
  conv.crit = 0.005, progress = FALSE)

# Mixed DINA and DINO Model
d3b <- din(sim.dino, q.matr = sim.qmatrix, rule = 
  c(rep("DINA", 4), rep("DINO", 5)), conv.crit = 0.001, 
  progress = FALSE)
                        
# DINA Estimation
d4 <- din(sim.dino, q.matr = sim.qmatrix, rule = "DINA",
  conv.crit = 0.005, progress = FALSE)
            
# Comparison of DINA and DINO estimates
lapply(list("guessing" = rbind("DINO" = d3$guess[,1],
  "DINA" = d4$guess[,1]), "slipping" = rbind("DINO" = 
  d3$slip[,1], "DINA" = d4$slip[,1])), round, 2)

# Comparison of the information criteria
c("DINO"=d3$AIC, "MIXED"=d3b$AIC, "DINA"=d4$AIC)

##
## (4) example estimation with weights based on dataset sim.dina
##

# Here, a weighted maximum likelihood estimation is used 
# This could be useful for survey data.

# i.e. first 200 persons have weight 2, the other have weight 1
(weights <- c(rep(2, 200), rep(1, 200)))

d5 <- din(sim.dina, sim.qmatrix, rule = "DINA", conv.crit = 
  0.005, weights = weights, progress = FALSE)
        
# Comparison of the information criteria
c("DINA"=d1$AIC, "WEIGHTS"=d5$AIC)


##
## (5) example estimation within a Balanced Incomplete 
##     Block (BIB) Design generated on dataset sim.dina
##

# generate BIB data

# The next example shows that the din and nida functions
# work for (relatively arbitrary) missing value pattern

# Here, a missing by design is generated in the dataset dinadat.bib
sim.dina.bib <- sim.dina
sim.dina.bib[1:100, 1:3] <- NA
sim.dina.bib[101:300, 4:8] <- NA
sim.dina.bib[301:400, c(1,2,9)] <- NA


d6 <- din(sim.dina.bib, sim.qmatrix, rule = "DINA", 
  conv.crit = 0.0005, weights = weights, maxit=200)

d7 <- din(sim.dina.bib, sim.qmatrix, rule = "DINO",
  conv.crit = 0.005, weights = weights)

# Comparison of DINA and DINO estimates
lapply(list("guessing" = rbind("DINA" = d6$guess[,1],
  "DINO" = d7$guess[,1]), "slipping" = rbind("DINA" =
  d6$slip[,1], "DINO" = d7$slip[,1])), round, 2)
}

\keyword{Cognitive Diagnosis Models}
\keyword{DINA}
\keyword{DINO}