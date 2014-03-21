\name{CDM-package}

\alias{CDM-package}
\concept{cognitive diagnosis modeling}
\docType{package}

\title{
  Cognitive Diagnosis Modeling: The \R Package \pkg{CDM}
}

\description{
Functions for cognitive diagnosis modeling
    and multidimensional item response modeling for dichotomous
    and polytomous data. This package enables the estimation of the
    DINA and DINO model, the multiple group (polytomous) GDINA model,
    the multiple choice DINA model and the general diagnostic model (GDM) 
    which contains the multidimensional linear compensatory item response 
    model as a special case.
}

\details{
  \tabular{ll}{
    Package: \tab \pkg{CDM}\cr
    Type: \tab Package\cr
    Version: \tab 3.1\cr
    Publication Year: \tab 2014\cr
    License: \tab \acronym{GPL} (>= 2) \cr
    URL: \tab \url{https://sites.google.com/site/alexanderrobitzsch/software} \cr            
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
  two core CDMs DINA and DINO (e.g.,de la Torre &
  Douglas, 2004; Junker & Sijtsma, 2001; Templin &
  Henson, 2006; the generalized DINA model for dichotomous attributes 
  (GDINA, de la Torre, 2011) and for polytomous attributes
  (pGDINA, Chen & de la Torre, 2013); 
  the general diagnostic model (GDM, von Davier, 2008) and its extension
  to the multidimensional latent class IRT model (Bartolucci, 2007),
  and tools for analyzing data under the models. 
  These and related concepts are explained in detail in the
  book about diagnostic measurement and CDMs by
  Rupp, Templin and Henson (2010), and in such survey articles as
  DiBello, Roussos and Stout (2007) and
  Rupp and Templin (2008).  

  The package \code{CDM} is implemented based on the S3 system.  It comes
  with a namespace and consists of several external functions (functions the
  package exports). 
  The package obtains a utility method for the simulation of artificial data based
  on a CDM model (\code{\link{sim.din}}).  It also contains seven internal functions 
  (functions not exported by the package): this are \code{plot}, \code{print}, and 
  \code{summary} methods for objects of the class \code{din} (\code{\link{plot.din}},
  \code{\link{print.din}}, \code{\link{summary.din}}), a \code{print} method for 
  objects of the class \code{summary.din} (\code{\link{print.summary.din}}), 
  and three functions for checking the input format and computing intermediate 
  information.  The features of the package \code{CDM} are 
  illustrated with an accompanying real dataset and Q-matrix 
  (\code{\link{fraction.subtraction.data}} and \code{\link{fraction.subtraction.qmatrix}}) 
  and artificial examples (\code{\link{Data-sim}}).  
}

\author{
  Alexander Robitzsch, Thomas Kiefer, Ann Cathrice George, Ali Uenlue 

  Maintainer: Alexander Robitzsch <a.robitzsch@bifie.at>
}

\seealso{
    See also the \pkg{ACTCD} and \pkg{NPCD} packages for nonparametric cognitive 
    diagnostic models.     
        }

%    In \pkg{NPCD} joint maximum likelihood estimation 
%    of several cognitive diagnostic models are implemented.        
        
\references{
  Bartolucci, F. (2007). A class of multidimensional IRT models for testing 
  unidimensionality and clustering items. \emph{Psychometrika}, \bold{72}, 141-157.

  Chen, J., & de la Torre, J. (2013). 
  A general cognitive diagnosis model for expert-defined polytomous attributes.
  \emph{Applied Psychological Measurement}, \bold{37}, 419-437.  
  
  de la Torre, J., & Douglas, J. (2004). Higher-order latent trait models 
  for cognitive diagnosis. \emph{Psychometrika}, \bold{69}, 333--353.

  de la Torre, J. (2011). The generalized {DINA} model framework. 
  \emph{Psychometrika}, \bold{76}, 179--199.    

  DiBello, L. V., Roussos, L. A., & Stout, W. F. (2007). Review of
  cognitively diagnostic assessment and a summary of psychometric models.
  In C. R. Rao and S. Sinharay (Eds.), \emph{Handbook of Statistics},
  Vol. 26 (pp. 979--1030). Amsterdam: Elsevier.

  Junker, B. W., & Sijtsma, K. (2001). Cognitive assessment models with few  
  assumptions, and connections with nonparametric item response theory.
  \emph{Applied Psychological Measurement}, \bold{25}, 258--272.
    
  Rupp, A. A., & Templin, J. (2008). Unique characteristics of
  diagnostic classification models: A comprehensive review of the current
  state-of-the-art. \emph{Measurement: Interdisciplinary Research and
  Perspectives}, \bold{6}, 219--262.
  
  Rupp, A. A., Templin, J., & Henson, R. A. (2010). \emph{Diagnostic
  Measurement: Theory, Methods, and Applications}. New York: The Guilford
  Press.
  
  Templin, J., & Henson, R. (2006). Measurement of
  psychological disorders using cognitive diagnosis
  models. \emph{Psychological Methods}, \bold{11}, 287--305.

  von Davier, M. (2008). A general diagnostic model applied to 
  language testing data. \emph{British Journal
  of Mathematical and Statistical Psychology}, \bold{61}, 287-307.
}

\examples{
##   
##   **********************************
##   ** CDM 2.5-16 (2013-11-29)      **
##   ** Cognitive Diagnostic Models  **
##   **********************************
##   
}

\keyword{package}
\section{\R Function Versions }{
\emph{anova.din__1.05.R}, 
\emph{calc_posterior__1.01.R}, 
\emph{cdi.kli__0.03.R}, \cr
\emph{cdm.est.calc.accuracy__2.09.R}, 
\emph{check.input__1.02.R}, 
\emph{coef__0.02.R}, \cr
\emph{din.deterministic__1.01.R}, 
\emph{din.deterministic_alg__0.06.R}, \cr
\emph{din.validate.qmatrix__1.03.R}, 
\emph{din__2.12.R}, 
\emph{equivalent.dina__1.01.R}, \cr
\emph{equivalent.skillclasses__0.04.R}, 
\emph{gdd__0.02.R}, 
\emph{gdina.dif__0.03.R}, 
\emph{gdina.dif_aux__0.03.R}, \cr
\emph{gdina.postproc__0.02.R}, 
\emph{gdina.wald__0.02.R}, 
\emph{gdina__8.74.R}, 
\emph{gdina_aux__0.01.R}, \cr
\emph{gdina_designmatrices__0.01.R}, 
\emph{gdina_hogdina_alg__1.06.R}, \cr
\emph{gdina_reduced_skillspace__0.03.R}, 
\emph{gdm__8.14.R}, 
\emph{gdm_algorithm__7.20.R}, \cr
\emph{gdm_postproc__3.01.R}, 
\emph{gdm_preproc__2.10.R}, 
\emph{ideal.response.pattern__0.03.R}, \cr
\emph{itemfit.rmsea__0.12.R}, 
\emph{itemfit.sx2__1.12.R}, 
\emph{itemfit.sx2_aux__1.13.R}, \cr
\emph{logLik_CDM__0.03.R}, 
\emph{mcdina.est.item__0.07.R}, 
\emph{mcdina.postproc__0.08.R}, \cr
\emph{mcdina.simul__0.02.R}, 
\emph{mcdina__0.52.R}, 
\emph{mcdina_alg_cppcall__0.01.R}, \cr
\emph{mcdina_prepare__0.18.R}, 
\emph{modelfit.cor.din__2.07.R}, 
\emph{modelfit.cor__1.14.R}, \cr
\emph{modelfit.cor2__3.07.R}, 
\emph{plot.din__2.12.R}, 
\emph{plot.gdina__0.01.R}, 
\emph{print.din__1.02.R}, \cr
\emph{print.summary.din__1.04.R}, 
\emph{rowMaxs__1.05.R}, 
\emph{rowProds__1.01.R}, 
\emph{sequential.items__0.02.R}, \cr
\emph{sim.din__1.02.R}, 
\emph{sim.gdina__2.02.R}, 
\emph{skill.cor__1.06.R}, \cr
\emph{skillspace.approximation__0.02.R}, 
\emph{skillspace.hierarchy__0.05.R}, 
\emph{summary.din__1.07.R}, \cr
\emph{summary.gdina__1.13.R}, 
\emph{summary.gdm__1.09.R}, 
\emph{summary.mcdina__0.04.R}, 
\emph{zzz__1.09.R}, 
}
\section{\emph{Rcpp} Function Versions }{
\emph{calc_posterior.c}, 
\emph{cdm_kli_id_c.cpp}, 
\emph{din.deterministic.devcrit_c.cpp}, \cr
\emph{din.jml.devcrit_c.cpp}, 
\emph{gdd__c.cpp}, 
\emph{modelfit_cor2_c.cpp}, \cr
\emph{probs_multcat_items_counts_c.cpp}, 
}
\section{\emph{Rd} Documentation Versions }{
\emph{anova.din__1.14.Rd}, 
\emph{cdi.kli__0.04.Rd}, 
\emph{CDM-package__2.25.Rd}, \cr
\emph{cdm.est.class.accuracy__1.11.Rd}, 
\emph{coef__0.09.Rd}, 
\emph{Data-sim__1.12.Rd}, 
\emph{data.cdm__0.05.Rd}, \cr
\emph{data.dcm__0.05.Rd}, 
\emph{data.dtmr__0.07.Rd}, 
\emph{data.ecpe__0.07.Rd}, 
\emph{data.fraction1__0.09.Rd}, \cr
\emph{data.fraction2__0.13.Rd}, 
\emph{data.hr__0.06.Rd}, 
\emph{data.jang__0.06.Rd}, 
\emph{data.melab__0.06.Rd}, \cr
\emph{data.mg__0.14.Rd}, 
\emph{data.pgdina__0.12.Rd}, 
\emph{data.sda6__0.05.Rd}, 
\emph{data.Students__0.08.Rd}, \cr
\emph{data.timss03.G8.su__0.03.Rd}, 
\emph{data.timss07.G4.lee__0.03.Rd}, \cr
\emph{din.deterministic__0.09.Rd}, 
\emph{din.equivalent.class__0.14.Rd}, \cr
\emph{din.validate.qmatrix__1.12.Rd}, 
\emph{din__2.15.Rd}, 
\emph{equivalent.dina__1.10.Rd}, \cr
\emph{fraction.subtraction.data__2.01.Rd}, 
\emph{fraction.subtraction.qmatrix__1.03.Rd}, \cr
\emph{gdd__0.08.Rd}, 
\emph{gdina.dif__0.07.Rd}, 
\emph{gdina.wald__0.07.Rd}, 
\emph{gdina__2.37.Rd}, 
\emph{gdm__4.10.Rd}, \cr
\emph{ideal.response.pattern__0.04.Rd}, 
\emph{itemfit.rmsea__1.06.Rd}, 
\emph{itemfit.sx2__1.09.Rd}, \cr
\emph{logLik__0.05.Rd}, 
\emph{mcdina__0.10.Rd}, 
\emph{modelfit.cor__1.34.Rd}, 
\emph{plot.din__2.07.Rd}, \cr
\emph{print.din__1.06.Rd}, 
\emph{print.summary.din__1.04.Rd}, 
\emph{sequential.items__0.05.Rd}, \cr
\emph{sim.din__2.02.Rd}, 
\emph{sim.gdina__1.14.Rd}, 
\emph{skill.cor__2.02.Rd}, \cr
\emph{skillspace.approximation__0.05.Rd}, 
\emph{skillspace.hierarchy__0.09.Rd}, \cr
\emph{summary.din__2.02.Rd}, 
}
