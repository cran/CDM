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
    the multiple choice DINA model, the general diagnostic model (GDM), 
    the multidimensional linear compensatory item response model and 
    the structured latent class model (SLCA).
}

\details{
  \tabular{ll}{
    Package: \tab \pkg{CDM}\cr
    Type: \tab Package\cr
    Version: \tab 4.8\cr
    Publication Year: \tab 2016\cr
    License: \tab \acronym{GPL} (>= 2) \cr           
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
  DINA and DINO model (e.g.,de la Torre &
  Douglas, 2004; Junker & Sijtsma, 2001; Templin &
  Henson, 2006; the generalized DINA model for dichotomous attributes 
  (GDINA, de la Torre, 2011) and for polytomous attributes
  (pGDINA, Chen & de la Torre, 2013); 
  the general diagnostic model (GDM, von Davier, 2008) and its extension
  to the multidimensional latent class IRT model (Bartolucci, 2007),
  the structure latent class model (Formann, 1992),
  and tools for analyzing data under the models. 
  These and related concepts are explained in detail in the
  book about diagnostic measurement and CDMs by
  Rupp, Templin and Henson (2010), and in such survey articles as
  DiBello, Roussos and Stout (2007) and
  Rupp and Templin (2008).  

  The package \code{CDM} is implemented based on the S3 system.  It comes
  with a namespace and consists of several external functions (functions the
  package exports). 
  The package contains a utility method for the simulation of artificial data based
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

  Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>
}

\seealso{
    See also the \pkg{ACTCD} and \pkg{NPCD} packages for nonparametric cognitive 
    diagnostic models.     
    
    See \pkg{dcmr} for MCMC based estimation methods. See \pkg{dina}
    for estimating the DINA model with a Gibbs sampler.
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

Formann, A. K. (1992). Linear logistic latent class analysis for polytomous data. 
\emph{Journal of the American Statistical Association}, \bold{87}, 476-486.  
  
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
\emph{anova.din__1.10.R}, 
\emph{calc_posterior__1.01.R}, 
\emph{cdi.kli__0.05.R}, \cr
\emph{cdm.est.calc.accuracy__2.15.R}, 
\emph{check.input__1.07.R}, 
\emph{coef__0.03.R}, 
\emph{confint.din__0.06.R}, \cr
\emph{din.deterministic__1.01.R}, 
\emph{din.deterministic_alg__0.06.R}, 
\emph{din.partable__0.14.R}, \cr
\emph{din.validate.qmatrix__1.04.R}, 
\emph{din__2.28.R}, 
\emph{entropy.lca__0.08.R}, \cr
\emph{equivalent.dina__1.02.R}, 
\emph{equivalent.skillclasses__0.06.R}, 
\emph{gdd__0.02.R}, \cr
\emph{gdina.dif__1.04.R}, 
\emph{gdina.dif_aux__1.09.R}, 
\emph{gdina.partable__0.02.R}, \cr
\emph{gdina.postproc__0.03.R}, 
\emph{gdina.wald__0.16.R}, 
\emph{gdina.wald_aux__0.05.R}, 
\emph{gdina__8.934.R}, \cr
\emph{gdina_aux__0.01.R}, 
\emph{gdina_designmatrices__0.03.R}, 
\emph{gdina_hogdina_alg__1.08.R}, \cr
\emph{gdina_reduced_skillspace__0.04.R}, 
\emph{gdm__8.33.R}, 
\emph{gdm_algorithm__8.09.R}, \cr
\emph{gdm_postproc__3.05.R}, 
\emph{gdm_preproc__2.12.R}, 
\emph{ideal.response.pattern__0.03.R}, \cr
\emph{IRT.anova__0.02.R}, 
\emph{IRT.compareModels__0.12.R}, 
\emph{IRT.data__0.04.R}, \cr
\emph{IRT.derivedParameters__0.02.R}, 
\emph{IRT.expectedCounts__0.05.R}, 
\emph{IRT.factor.scores__0.04.R}, \cr
\emph{IRT.IC__0.03.R}, 
\emph{IRT.irfprob__0.04.R}, 
\emph{IRT.irfprobPlot__1.04.R}, 
\emph{IRT.itemfit__0.02.R}, \cr
\emph{IRT.jackknife.gdina__0.13.R}, 
\emph{IRT.jackknife__0.01.R}, 
\emph{IRT.likelihood__0.07.R}, \cr
\emph{IRT.modelfit__0.08.R}, 
\emph{IRT.parameterTable__0.02.R}, 
\emph{IRT.posterior__0.01.R}, \cr
\emph{IRT.predict__1.14.R}, 
\emph{IRT.repDesign__0.07.R}, 
\emph{IRT.se.din__0.04.R}, 
\emph{IRT.se__0.01.R}, \cr
\emph{itemfit.rmsea__0.17.R}, 
\emph{itemfit.sx2__3.06.R}, 
\emph{itemfit.sx2_aux__3.05.R}, \cr
\emph{jkestimates__0.04.R}, 
\emph{label_significance_level__0.01.R}, 
\emph{logLik_CDM__0.07.R}, \cr
\emph{mcdina.est.item__0.07.R}, 
\emph{mcdina.postproc__0.08.R}, 
\emph{mcdina.simul__0.10.R}, 
\emph{mcdina__0.59.R}, \cr
\emph{mcdina_alg_cppcall__0.01.R}, 
\emph{mcdina_prepare__0.18.R}, 
\emph{modelfit.cor.din__2.10.R}, \cr
\emph{modelfit.cor__1.17.R}, 
\emph{modelfit.cor2__3.18.R}, 
\emph{numerical_Hessian__0.06.R}, 
\emph{osink__1.02.R}, \cr
\emph{personfit.appropriateness__1.05.R}, 
\emph{plot.din__2.14.R}, 
\emph{plot.gdina__0.02.R}, \cr
\emph{plot.gdm__0.06.R}, 
\emph{plot.slca__0.02.R}, 
\emph{predict.CDM__0.02.R}, 
\emph{print.din__2.03.R}, \cr
\emph{print.gdina__0.03.R}, 
\emph{print.gdm__0.02.R}, 
\emph{print.mcdina__0.03.R}, 
\emph{print.slca__0.03.R}, \cr
\emph{print.summary.din__1.14.R}, 
\emph{rowMaxs__1.05.R}, 
\emph{rowProds__1.01.R}, 
\emph{rrum.param__0.01.R}, \cr
\emph{sequential.items__1.01.R}, 
\emph{sim.din__1.04.R}, 
\emph{sim.gdina__2.05.R}, 
\emph{skill.cor__1.13.R}, \cr
\emph{skillspace.approximation__0.02.R}, 
\emph{skillspace.hierarchy__0.08.R}, \cr
\emph{slca.algorithm__1.15.R}, 
\emph{slca.postprocess__0.04.R}, 
\emph{slca__1.36.R}, 
\emph{summary.din__1.15.R}, \cr
\emph{summary.gdina.wald__0.01.R}, 
\emph{summary.gdina__1.23.R}, 
\emph{summary.gdm__1.17.R}, \cr
\emph{summary.mcdina__0.11.R}, 
\emph{summary.slca__1.16.R}, 
\emph{vcov.din__1.54.R}, \cr
\emph{vcov.loglike.din__0.01.R}, 
\emph{WaldTest__0.02.R}, 
\emph{zzz__1.11.R}, 
}
\section{\emph{Rcpp} Function Versions }{
\emph{calc_posterior__1.01.c}, 
\emph{cdm_kli_id_c__2.02.cpp}, \cr
\emph{din.deterministic.devcrit_c__2.02.cpp}, 
\emph{din.jml.devcrit_c__2.02.cpp}, 
\emph{gdd_c__2.02.cpp}, \cr
\emph{irt_predict_c__2.02.cpp}, 
\emph{itemfit_sx2_calc_scoredistribution_cdm__2.01.cpp}, \cr
\emph{modelfit_cor2_c__2.02.cpp}, 
\emph{probs_multcat_items_counts_c__2.03.cpp}, \cr
\emph{slca_cfunctions__2.02.cpp}, 
}
\section{\emph{Rd} Documentation Versions }{
\emph{anova.din__1.21.Rd}, 
\emph{cdi.kli__0.10.Rd}, 
\emph{CDM-package__2.52.Rd}, \cr
\emph{cdm.est.class.accuracy__1.12.Rd}, 
\emph{coef__0.15.Rd}, 
\emph{Data-sim__1.12.Rd}, 
\emph{data.cdm__0.06.Rd}, \cr
\emph{data.dcm__0.16.Rd}, 
\emph{data.dtmr__0.10.Rd}, 
\emph{data.ecpe__0.13.Rd}, 
\emph{data.fraction1__0.11.Rd}, \cr
\emph{data.fraction2__0.14.Rd}, 
\emph{data.hr__0.08.Rd}, 
\emph{data.jang__0.09.Rd}, 
\emph{data.melab__0.08.Rd}, \cr
\emph{data.mg__0.18.Rd}, 
\emph{data.pgdina__0.13.Rd}, 
\emph{data.sda6__0.06.Rd}, 
\emph{data.Students__0.12.Rd}, \cr
\emph{data.timss03.G8.su__0.04.Rd}, 
\emph{data.timss07.G4.lee__0.04.Rd}, \cr
\emph{data.timss11.G4.AUT__0.06.Rd}, 
\emph{din.deterministic__0.14.Rd}, \cr
\emph{din.equivalent.class__0.16.Rd}, 
\emph{din.validate.qmatrix__1.16.Rd}, 
\emph{din__2.29.Rd}, \cr
\emph{entropy.lca__0.16.Rd}, 
\emph{equivalent.dina__1.11.Rd}, 
\emph{fraction.subtraction.data__2.01.Rd}, \cr
\emph{fraction.subtraction.qmatrix__1.03.Rd}, 
\emph{gdd__0.08.Rd}, 
\emph{gdina.dif__0.10.Rd}, \cr
\emph{gdina.wald__0.18.Rd}, 
\emph{gdina__2.54.Rd}, 
\emph{gdm__4.25.Rd}, 
\emph{ideal.response.pattern__0.04.Rd}, \cr
\emph{IRT.anova__0.02.Rd}, 
\emph{IRT.compareModels__0.11.Rd}, 
\emph{IRT.data__0.04.Rd}, \cr
\emph{IRT.expectedCounts__0.03.Rd}, 
\emph{IRT.factor.scores__0.05.Rd}, 
\emph{IRT.IC__0.05.Rd}, \cr
\emph{IRT.irfprob__0.07.Rd}, 
\emph{IRT.irfprobPlot__0.08.Rd}, 
\emph{IRT.itemfit__0.03.Rd}, \cr
\emph{IRT.jackknife__0.13.Rd}, 
\emph{IRT.likelihood__0.06.Rd}, 
\emph{IRT.modelfit__0.12.Rd}, \cr
\emph{IRT.parameterTable__0.02.Rd}, 
\emph{IRT.repDesign__0.11.Rd}, 
\emph{itemfit.rmsea__1.07.Rd}, \cr
\emph{itemfit.sx2__1.24.Rd}, 
\emph{logLik__0.12.Rd}, 
\emph{mcdina__0.18.Rd}, 
\emph{modelfit.cor__1.48.Rd}, \cr
\emph{numerical_Hessian__0.12.Rd}, 
\emph{osink__0.05.Rd}, 
\emph{personfit.appropriateness__0.13.Rd}, \cr
\emph{plot.din__2.07.Rd}, 
\emph{predict__0.12.Rd}, 
\emph{print.summary.din__1.04.Rd}, \cr
\emph{sequential.items__0.07.Rd}, 
\emph{sim.din__2.04.Rd}, 
\emph{sim.gdina__1.15.Rd}, 
\emph{skill.cor__2.05.Rd}, \cr
\emph{skillspace.approximation__0.06.Rd}, 
\emph{skillspace.hierarchy__0.12.Rd}, 
\emph{slca__1.47.Rd}, \cr
\emph{summary.din__2.04.Rd}, 
\emph{vcov__0.26.Rd}, 
\emph{WaldTest__0.06.Rd}, 
}
