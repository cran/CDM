\name{Data-sim}

\alias{Data-sim}
\alias{sim.dina}
\concept{sim dina}
\alias{sim.dino}
\concept{sim dino}
\alias{sim.qmatrix}
\concept{sim qmatrix}

\title{
  Artificial Data: DINA and DINO
}

\docType{data}

\description{
  Artificial data: dichotomously coded fictitious answers of 400 respondents 
  to 9 items assuming 3 underlying attributes.  
}

\usage{
  data(sim.dina)
  data(sim.dino)
  data(sim.qmatrix)
}

\format{
  The \code{sim.dina} and \code{sim.dino} data sets include dichotomous 
  answers of \eqn{N=400} respondents to \eqn{J=9} items, thus they are 
  \eqn{400} times \eqn{9} data matrices.  For both data sets \eqn{K=3}
  attributes are assumed to underlie the process of responding, stored 
  in \code{sim.qmatrix}.  
  
  The \code{sim.dina} data set is simulated according to the DINA condensation
  rule, whereas the \code{sim.dino} data set is simulated according to the 
  DINO condensation rule.  The slipping errors for the items 1 to 9 in both 
  data sets are \code{0.20, 0.20, 0.20, 0.20, 0.00, 0.50, 0.50, 0.10, 0.03} 
  and the guessing errors are \code{0.10, 0.125, 0.15, 0.175, 0.2, 0.225, 
  0.25, 0.275, 0.3}.  The attributes are assumed to be mastered with expected 
  probabilities of \code{-0.4 , 0.2, 0.6}, respectively.  The correlation of 
  the attributes is \code{0.3} for attributes 1 and 2, \code{0.4} for 
  attributes 1 and 3 and \code{0.1} for attributes 2 and 3.  
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

\examples{
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

# The next example shows that the din function
# works for (relatively arbitrary) missing value pattern

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

\keyword{datasets}
