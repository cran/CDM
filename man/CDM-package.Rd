\name{CDM-package}

\alias{CDM-package}
\concept{cognitive diagnosis modeling}
\docType{package}

\title{
  Cognitive Diagnosis Modeling: The R Package CDM
}

\description{
  Cognitive diagnosis modeling (CDM) is restricted latent class modeling for
  inferring from respondents' item answers fine-grained and individualized
  diagnostic information about multiple latent attributes or competencies
  (e.g., reading or mathematical comprehension skills).  The package
  \code{CDM} provides functions and example data for cognitive diagnosis
  modeling with the deterministic-input, noisy-and-gate (DINA) and
  deterministic-input and noisy-or-gate (DINO) in \R.
%  and the multiple group
%  generalized DINA (GDINA) models in \R.  
}

\details{
  \tabular{ll}{
    Package: \tab \code{CDM}\cr
    Type: \tab Package\cr
    Version: \tab 1.1-0\cr
    Date: \tab 2012-06-xx\cr
    License: \tab \acronym{GPL} (>= 2)
  }
  Cognitive diagnosis models (CDMs) are restricted latent class models.  
  They represent model-based classification approaches, which aim at
  assigning respondents to different attribute profile groups.  The latent
  classes correspond to the possible attribute profiles, and the
  conditional item parameters model atypical response behavior in the sense
  of slipping and guessing errors.  The core CDMs in particular differ in
  the utilized condensation rule, conjunctive / non-compensatory versus
  disjunctive / compensatory, where in the model structure these two
  types of response error parameters enter and what restrictions are
  imposed on them.  The confirmatory character of CDMs is apparent in the
  Q-matrix, which can be seen as an operationalization of the latent
  concepts of an underlying theory.  The Q-matrix allows incorporating
  qualitative prior knowledge and typically has as its rows the items and
  as the columns the attributes, with entries 1 or 0, depending on whether
  an attribute is measured by an item or not, respectively.  

  CDMs as compared to common psychometric models (e.g., IRT) contain
  categorical instead of continuous latent variables.  The results of
  analyses using CDMs differ from the results obtained under continuous
  latent variable models.  CDMs estimate in a direct manner the
  probabilistic attribute profile of a respondent, that is, the
  multivariate vector of the conditional probabilities for possessing the
  individual attributes, given her / his response pattern.  Based on these
  probabilities, simplified deterministic attribute profiles can be
  derived, showing whether an individual attribute is essentially possessed
  or not by a respondent.  As compared to alternative two-step
  discretization approaches, which estimate continuous scores and discretize
  the continua based on cut scores, with CDMs the classification error can
  generally be reduced.  

  The package \code{CDM} implements parameter estimation procedures for the 
  two core CDMs DINA and DINO (e.g.,\cite{de la Torre and
  Douglas, 2004}, \cite{Junker and Sijtsma, 2001} and \cite{Templin and
  Henson, 2006}), and tools for analyzing data under the
  two models.  These and related concepts are explained in detail in the
  book about diagnostic measurement and CDMs by
  \cite{Rupp, Templin, and Henson (2010)}, and in such survey articles as
  \cite{DiBello, Roussos, and Stout (2007)} and
  \cite{Rupp and Templin (2008)}.  

  The package \code{CDM} is implemented based on the S3 system.  It comes
  with a namespace and consists of one external function (a function the
  package exports): the main function of this package, \code{\link{din}}.  The
  package obtains a utility method for the simulation of artificial data based
  on a CDM model (\code{\link{sim.din}}).  It also contains seven internal functions 
  (functions not exported by the package): this are \code{plot}, \code{print}, and 
  \code{summary} methods for objects of the class \code{din} (\code{\link{plot.din}},
  \code{\link{print.din}}, \code{\link{summary.din}}), a \code{print} method for 
  objects of the class \code{summary.din} (\code{\link{print.summary.din}}), 
  and three functions for checking the input format and computing intermediate 
  information (\link{CDM-internal}).  The features of the package \code{CDM} are 
  illustrated with an accompanying real dataset and Q-matrix 
  (\code{\link{fraction.subtraction.data}} and \code{\link{fraction.subtraction.qmatrix}}) 
  and artificial examples (\code{\link{Data-sim}}).  
%    ADD SOMETHING ABOUT INCLUDED GDINA FRAMEWORK.
}

\author{
  Alexander Robitzsch, Thomas Kiefer, Ann Cathrice George, Ali Uenlue 

  Maintainer: Alexander Robitzsch <a.robitzsch@bifie.at>
}

\references{
  de la Torre, J. and Douglas, J. (2004) Higher-order latent trait models 
  for cognitive diagnosis. \emph{Psychometrika}, \bold{69}, 333--353.

%  de la Torre, J. (2011) The generalized {DINA} model framework. \emph{Psychometrika}, \bold{76}, 179--199.    

  DiBello, L. V., Roussos, L. A. and Stout, W. F. (2007) Review of
  cognitively diagnostic assessment and a summary of psychometric models.
  In C. R. Rao and S. Sinharay (Eds.), \emph{Handbook of Statistics},
  Vol. 26 (pp. 979--1030). Amsterdam: Elsevier.

  Junker, B. W. and Sijtsma, K. (2001) Cognitive assessment models with few  
  assumptions, and connections with nonparametric item response theory.
  \emph{Applied Psychological Measurement}, \bold{25}, 258--272.
    
  Rupp, A. A. and Templin, J. (2008) Unique characteristics of
  diagnostic classification models: A comprehensive review of the current
  state-of-the-art. \emph{Measurement: Interdisciplinary Research and
  Perspectives}, \bold{6}, 219--262.
  
  Rupp, A. A., Templin, J. and Henson, R. A. (2010) \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}. New York: The Guilford
  Press.
  
  Templin, J. and Henson, R. (2006) Measurement of
  psychological disorders using cognitive diagnosis
  models. \emph{Psychological Methods}, \bold{11}, 287--305.
}

\keyword{package}