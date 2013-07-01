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
  din(data, q.matrix, skillclasses = NULL , 
      conv.crit = 0.001, dev.crit= 10^(-5) , maxit = 500, 
      constraint.guess = NULL, constraint.slip = NULL,
      guess.init = rep(0.2, ncol(data)), slip.init = guess.init,
      guess.equal = FALSE, slip.equal = FALSE, zeroprob.skillclasses = NULL,  
      weights = rep(1, nrow(data)), rule = "DINA", 
      wgt.overrelax = 0 , wgtest.overrelax = FALSE , param.history=FALSE , 
      progress = TRUE)
}


\arguments{
  \item{data}{a required \eqn{N} times \eqn{J} data matrix 
    containing the binary responses, 0 or 1, of \eqn{N} respondents to \eqn{J}
    test items, where 1 denotes a correct anwer and 0 an incorrect one. The 
    nth row of the matrix represents the binary response pattern of respondant
    n.  \code{NA} values are allowed.  }
    
  \item{q.matrix}{a required binary \eqn{J} times \eqn{K} containing the 
    attributes not required or required, 0 or 1, to master the items.  The
    jth row of the matrix is a binary indicator vector indicating which 
    attributes are not required (coded by 0) and which attributes are 
    required (coded by 1) to master item \eqn{j}.  }

  \item{skillclasses}{An optional matrix for determining the skill space.
    The argument can be used if a user wants less than \eqn{2^K} skill classes.    
  }
   
    
  \item{conv.crit}{termination criterion of iterations in the parameter 
    estimation process.  Iteration ends if the maximal change in parameter
    estimates is below this value.  }

  \item{dev.crit}{termination criterion of iterations in relative change in
        deviance.}


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
    
  \item{guess.equal}{an optional logical indicating if all guessing parameters are equal to each other.
            Default is FALSE.}

 \item{slip.equal}{an optional logical indicating if all slipping parameters are equal to each other.
            Default is FALSE.}
  
\item{zeroprob.skillclasses}{an optional vector of integers which indicates which skill classes should have zero probability. Default is NULL 
    (no skill classes with zero probability).}
 
     \item{weights}{an optional vector of weights for the response pattern. 
  	Non-integer weights allow for different sampling schemes.  }
    
  \item{rule}{an optional character string or vector of character strings 
  	specifying the model rule that is used. The character strings must be 
  	of \code{"DINA"} or \code{"DINO"}.  If a vector of character strings is 
  	specified, implying an item wise condensation rule, the vector must 
  	be of length \eqn{J}, which is the number of items.  The default is 
    the condensation rule \code{"DINA"} for all items.  }
         

\item{wgt.overrelax}{A parameter which is relevant when an overrelaxation
    algorithm is used}

\item{wgtest.overrelax}{A logical which indicates if the overrelexation 
    parameter being estimated during iterations}

\item{param.history}{Save parameter history?}
   
\item{progress}{an optional logical indicating whether the function
    should print the progress of iteration in the estimation process.  } 
}

\details{
  In the CDM DINA (deterministic-input, noisy-and-gate; \cite{de la Torre and
  Douglas, 2004}) and DINO (deterministic-input, noisy-or-gate; \cite{Templin and
  Henson, 2006}) models endorsement probabilities are modeled 
  based on guessing and slipping parameters, given the different skill
  classes.  The probability of respondent \eqn{n} (or corresponding respondents class \eqn{n}) 
  for solving item \eqn{j} is calculated as a function of the respondent's latent response \eqn{\eta_{nj}}{eta_{nj}}
  and the guessing and slipping rates \eqn{g_j} and \eqn{s_j} for item
  \eqn{j} conditional on the respondent's skill class \eqn{\alpha_n}{alpha_n}:
%  \deqn{P_j(\alpha_n) = P(X_{nj} = 1 | \alpha_n) = g_j^{(1- \eta_{nj})}
%  (1 - s_j) ^{\eta_{nj}}.  }{P_j(alpha_n) = P(X_{nj} = 1 | alpha_n) = g_j^{(1- eta_{nj})}
%  (1 - s_j) ^{eta_{nj}}.  }
   \deqn{ P(X_{nj} = 1 | \alpha_n) = g_j^{(1- \eta_{nj})}(1 - s_j) ^{\eta_{nj}}.  }
  The respondent's latent response (class) \eqn{\eta_{nj}}{eta_{nj}} is a binary number,
  0 or 1, indicating absence or presence of all (\code{rule = "DINO"}) 
  or at least one (\code{rule = "DINO"}) required skill(s) for 
  item \eqn{j}, respectively.  
  
  DINA and DINO parameter estimation is performed by
  maximization of the marginal likelihood of the data.  The
  a priori distribution of the skill vectors is a uniform distribution.  
  The implementation follows the EM algorithm by \cite{de la Torre (2009)}.  
  
  A monotonicity condition in parameter estimation in DINA and
  DINO models is 
  	\deqn{g_j < 1 - s_j}
  for each item.
  That is, the probability of guessing an item without possessing the required
  skills is supposed to be smaller than the probability of not slipping the item 
  when possessing all the required skills for that item.  However, the EM algorithm
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
  \item{IDI}{a matrix giving the item discrimination
    index (IDI; Lee, de la Torre & Park, 2012) for each item \eqn{j}
    \deqn{ IDI_j = 1 - s_j - g_j , }{ IDI_j = 1 - s_j - g_j , }
    where a high IDI corresponds to good test items
    which have both low guessing and slipping rates.  Note that
    a negative IDI contradicts the monotonicity condition 
    \eqn{g_j < 1 - s_j}.  See \code{\link{din}} for help.  }
\item{itemfit.rmsea}{The RMSEA item fit index (see \code{\link{itemfit.rmsea}}).
            }
\item{mean.rmsea}{Mean of RMSEA item fit indexes.
            }            
  \item{loglike}{a numeric giving the value of the maximized
    log likelihood.  }
  \item{AIC}{a numeric giving the AIC value of the model.  }
  \item{BIC}{a numeric giving the BIC value of the model.  }
  \item{Npars}{number of estimated parameters}  
  \item{posterior}{a matrix given the posterior skill distribution
    for all respondents.  The nth row of the matrix gives the probabilities for 
    respondent n to possess any of the \code{2^K} skill classes.  }
  \item{like}{a matrix giving the values of the maximized likelihood
    for all respondents.  }
  \item{data}{the input matrix of binary response data.  }
  \item{q.matrix}{the input matrix of the required attributes.  }
  \item{pattern}{a matrix giving the skill classes leading to highest endorsement 
    probability for the respective response pattern (\code{mle.est}) with the 
    corresponding posterior class probability (\code{mle.post}), the attribute 
    classes having the highest occurrence posterior probability given the 
    response pattern (\code{map.est}) with the corresponding posterior class 
    probability (\code{map.post}), and the estimated posterior for each 
    response pattern (\code{pattern}).  }
  \item{attribute.patt}{a data frame giving the estimated occurrence
    probabilities of the skill classes and the expected frequency of the
    attribute classes given the model.  }
  \item{skill.patt}{a matrix given the population prevalences of the
    skills.  }
  \item{subj.pattern}{a vector of strings indicating the item response
    pattern for each subject.  }
  \item{attribute.patt.splitted}{a dataframe giving the skill class 
    of the respondents.  }
  \item{display}{a character giving the model specified under 
    \code{rule}.  }
  \item{item.patt.split}{a matrix giving the splitted response pattern.  }
  \item{item.patt.freq}{a numeric vector given the frequencies of the response
    pattern in \code{item.patt.split}.  }
}

\references{
  de la Torre, J. (2009) DINA model parameter estimation:
  A didactic. \emph{Journal of Educational and Behavioral
  Statistics}, \bold{34}, 115--130.
  
  de la Torre, J. and Douglas, J. (2004) Higher-order latent trait models 
  for cognitive diagnosis. \emph{Psychometrika}, \bold{69}, 333--353.    

  Lee, Y.-S., de la Torre, J. and Park, Y. S. (2012). Relationships between cognitive diagnosis, CTT, and IRT indices: An empirical investigation.
    \emph{Asia Pacific Educational Research}, \bold{13}, 333-345.

  Rupp, A. A., Templin, J. and Henson, R. A. (2010) \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}.  New York: The Guilford
  Press.
  
  Templin, J. and Henson, R. (2006) Measurement of
  psychological disorders using cognitive diagnosis
  models. \emph{Psychological Methods}, \bold{11}, 287--305.
}

%\author{
%  Alexander Robitzsch \email{a.robitzsch@bifie.at}
%  Based on EM Algorithm by J. de la Torre.
%  
%  Thomas Kiefer, Ann Cathrice George, Ali Uenlue 
%}

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
  \code{din}.  
  
  See the \code{\link{gdina}} function for the estimation of
  the generalized DINA (GDINA) model.
  
  For assessment of model fit see \code{\link{modelfit.cor.din}} and 
  \code{\link{anova.din}}.
  
  See also \code{\link{CDM-package}} for general
  information about this package.  
  
  See the \pkg{NPCD} package for joint maximum likelihood estimation
  of the DINA, DINO and NIDA model.
}

\examples{
##
## (1) examples based on dataset fractions.subtraction.data
##

## dataset fractions.subtraction.data and corresponding Q-Matrix
head(fraction.subtraction.data)
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
attributes(fractions.dina)
str(fractions.dina)	  

## For instance assessing the guessing parameters through
## assignment
fractions.dina$guess

## corresponding summaries, including IDI,
## most frequent skill classes and information 
## criteria AIC and BIC
summary(fractions.dina)

## In particular, assessing detailed summary through assignment
detailed.summary.fs <- summary(fractions.dina)
str(detailed.summary.fs)

## Item discrimination index of item 8 is too low. This is also
## visualized in the first plot 
plot(fractions.dina)

## The reason therefore is a high guessing parameter
round(fractions.dina$guess[,1], 2)

## Fix the guessing parameters of items 5, 8 and 9 equal to .20
# define a constraint.guess matrix
constraint.guess <-  matrix(c(5,8,9, rep(0.2, 3)), ncol = 2)
fractions.dina.fixed <- din(data = fraction.subtraction.data, 
  q.matrix = fraction.subtraction.qmatrix, 
  constraint.guess=constraint.guess)

## The second plot shows the expected (MAP) and observed skill 
## probabilities. The third plot visualizes the skill class
## occurrence probabilities; Only the 'top.n.skill.classes' most frequent 
## skill classes are labeled; it
## is obvious that the skill class '11111111' (all skills are
## mastered) is the most probable in this population. The fourth
## plot shows the skill probabilities conditional on response
## patterns; in this population the skills 3 and 6 seem to be
## mastered easier than the others. The fifth plot shows the
## skill probabilities conditional on a specified response
## pattern; it is shown whether a skill is mastered (above 
## .5+'uncertainty') unclassifiable (within the boundaries) or
## not mastered (below .5-'uncertainty'). In this case, the
## 527th respondent was chosen; if no response pattern is 
## specified, the plot will not be shown (of course)
pattern <- paste(fraction.subtraction.data[527, ], collapse = "")
plot(fractions.dina, pattern = pattern, display.nr = 5)

#uncertainty = 0.1, top.n.skill.classes = 6 are default
plot(fractions.dina.fixed, uncertainty = 0.1, top.n.skill.classes = 6, 
  pattern = pattern)

\dontrun{  
##
## (2) examples based on dataset sim.dina
##

# DINA Model
d1 <- din(sim.dina, q.matr = sim.qmatrix, rule = "DINA",
  conv.crit = 0.01, maxit = 500, progress = TRUE)
summary(d1)

# DINA model with hierarchical skill classes (Hierarchical DINA model)
# 1st step:  estimate an initial full model to look at the indexing
#    of skill classes
d0 <- din(sim.dina, q.matr = sim.qmatrix, maxit=1)
d0$attribute.patt.splitted
#      [,1] [,2] [,3]
# [1,]    0    0    0
# [2,]    1    0    0
# [3,]    0    1    0
# [4,]    0    0    1
# [5,]    1    1    0
# [6,]    1    0    1
# [7,]    0    1    1
# [8,]    1    1    1
#
# In this example, following hierarchical skill classes shall only allowed:
# 000, 001, 011, 111
# We define therefore a vector of indices for skill classes with
# zero probabilities (see entries in the rows of the matrix d0$attribute.patt.splitted above)
zeroprob.skillclasses <- c(2,3,5,6)     # classes 100, 010, 110, 101
# estimate the hierarchical DINA model
d1a <- din(sim.dina, q.matr = sim.qmatrix, zeroprob.skillclasses =zeroprob.skillclasses )
summary(d1a)

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
d1$attribute.patt  # skill classes with probabilities
d1$subj.pattern    # pattern per subject

# posterior probabilities for every response pattern
d1$posterior       

# Equal guessing parameters
d2a <- din( data = sim.dina , q.matrix = sim.qmatrix ,  
            guess.equal = TRUE , slip.equal = FALSE )
d2a$coef

# Equal guessing and slipping parameters
d2b <- din( data = sim.dina , q.matrix = sim.qmatrix ,  
            guess.equal = TRUE , slip.equal = TRUE )
d2b$coef


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
  d6$slip[,1], "DINO" = d7$slip[,1])), round, 2) }
}

\keyword{Cognitive Diagnosis Models}
\keyword{DINA}
\keyword{DINO}