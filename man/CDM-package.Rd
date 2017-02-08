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
  
  See George et al. (2016) for an overview and some computational details
  of the \pkg{CDM} package.
}

\author{
  Alexander Robitzsch, Thomas Kiefer, Ann Cathrice George, Ali Uenlue 

  Maintainer: Alexander Robitzsch <robitzsch@ipn.uni-kiel.de>
}

\seealso{
    See the \pkg{GDINA} package for comprehensive functions for the 
    GDINA model.  

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
  
George, A. C., Robitzsch, A., Kiefer, T., Gross, J., & Uenlue, A. (2016).
The R package CDM for cognitive diagnosis models.
\emph{Journal of Statistical Software, 74}(2), 1-24.
  
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
\emph{abs_approx__0.02.R}, 
\emph{anova.din__1.11.R}, 
\emph{calc_posterior__1.03.R}, 
\emph{cdi.kli__0.07.R}, \cr
\emph{cdm.est.calc.accuracy__2.17.R}, 
\emph{check.input__1.08.R}, 
\emph{coef__0.05.R}, 
\emph{confint.din__0.07.R}, \cr
\emph{dataframe_summary__0.04.R}, 
\emph{deltaMethod__0.03.R}, 
\emph{din.deterministic__1.02.R}, \cr
\emph{din.deterministic_alg__0.09.R}, 
\emph{din.partable__0.15.R}, 
\emph{din.validate.qmatrix__1.05.R}, \cr
\emph{din__2.30.R}, 
\emph{entropy.lca__0.10.R}, 
\emph{equivalent.dina__1.03.R}, \cr
\emph{equivalent.skillclasses__0.07.R}, 
\emph{gdd__0.08.R}, 
\emph{gdina.dif__1.12.R}, 
\emph{gdina.dif_aux__1.32.R}, \cr
\emph{gdina.partable__0.07.R}, 
\emph{gdina.postproc__0.07.R}, 
\emph{gdina.wald__0.22.R}, \cr
\emph{gdina.wald_aux__0.12.R}, 
\emph{gdina__9.023.R}, 
\emph{gdina_designmatrices__0.12.R}, \cr
\emph{gdina_hogdina_alg__1.10.R}, 
\emph{gdina_mstep_item_ml__0.17.R}, \cr
\emph{gdina_mstep_item_ml_rrum__0.24.R}, 
\emph{gdina_mstep_item_ml_rrum2__1.03.R}, \cr
\emph{gdina_mstep_item_uls__0.05.R}, 
\emph{gdina_proc_noninvariance_multiple_groups__0.02.R}, \cr
\emph{gdina_proc_sequential_items__0.02.R}, 
\emph{gdina_reduced_skillspace__0.06.R}, \cr
\emph{gdina_se_itemwise__0.09.R}, 
\emph{gdm__8.34.R}, 
\emph{gdm_algorithm__8.10.R}, 
\emph{gdm_postproc__3.06.R}, \cr
\emph{gdm_preproc__2.13.R}, 
\emph{ideal.response.pattern__0.05.R}, 
\emph{IRT.anova__0.03.R}, \cr
\emph{IRT.compareModels__0.13.R}, 
\emph{IRT.data__0.05.R}, 
\emph{IRT.derivedParameters__0.03.R}, \cr
\emph{IRT.expectedCounts__0.06.R}, 
\emph{IRT.factor.scores__0.05.R}, 
\emph{IRT.IC__0.04.R}, \cr
\emph{IRT.irfprob__0.05.R}, 
\emph{IRT.irfprobPlot__1.05.R}, 
\emph{IRT.itemfit__0.03.R}, \cr
\emph{IRT.jackknife.gdina__0.14.R}, 
\emph{IRT.jackknife__0.02.R}, 
\emph{IRT.likelihood__0.08.R}, \cr
\emph{IRT.modelfit__0.10.R}, 
\emph{IRT.parameterTable__0.03.R}, 
\emph{IRT.posterior__0.02.R}, \cr
\emph{IRT.predict__1.16.R}, 
\emph{IRT.repDesign__0.08.R}, 
\emph{IRT.RMSD__0.14.R}, 
\emph{IRT.se.din__0.05.R}, \cr
\emph{IRT.se__0.02.R}, 
\emph{item_by_group__0.12.R}, 
\emph{itemfit.rmsea__0.22.R}, 
\emph{itemfit.sx2__3.07.R}, \cr
\emph{itemfit.sx2_aux__3.07.R}, 
\emph{jkestimates__0.05.R}, 
\emph{label_significance_level__0.02.R}, \cr
\emph{logLik_CDM__0.08.R}, 
\emph{logpars2rrumpars__0.02.R}, 
\emph{mad_aux__0.02.R}, 
\emph{mcdina.est.item__0.08.R}, \cr
\emph{mcdina.postproc__0.10.R}, 
\emph{mcdina.simul__0.11.R}, 
\emph{mcdina__0.63.R}, \cr
\emph{mcdina_alg_cppcall__0.03.R}, 
\emph{mcdina_prepare__0.20.R}, 
\emph{md_aux__2.02.R}, \cr
\emph{modelfit.cor.din__2.11.R}, 
\emph{modelfit.cor__1.18.R}, 
\emph{modelfit.cor2__3.23.R}, \cr
\emph{numerical_Hessian__0.15.R}, 
\emph{osink__1.04.R}, 
\emph{personfit.appropriateness__1.06.R}, \cr
\emph{plot.din__2.15.R}, 
\emph{plot.gdina__0.03.R}, 
\emph{plot.gdm__0.07.R}, 
\emph{plot.slca__0.03.R}, \cr
\emph{predict.CDM__0.03.R}, 
\emph{print.din__2.04.R}, 
\emph{print.gdina__0.04.R}, 
\emph{print.gdm__0.03.R}, \cr
\emph{print.mcdina__0.04.R}, 
\emph{print.slca__0.04.R}, 
\emph{print.summary.din__1.22.R}, \cr
\emph{RcppExports__0.01.R}, 
\emph{rmsd_chisquare__0.06.R}, 
\emph{rmsea_aux__2.04.R}, 
\emph{rowMaxs__1.07.R}, \cr
\emph{rowProds__1.02.R}, 
\emph{rrum.param__0.02.R}, 
\emph{rrumpars2logpars__0.02.R}, \cr
\emph{sequential.items__1.03.R}, 
\emph{sim.din__1.05.R}, 
\emph{sim.gdina__2.06.R}, 
\emph{skill.cor__1.14.R}, \cr
\emph{skillspace.approximation__0.03.R}, 
\emph{skillspace.hierarchy__0.10.R}, \cr
\emph{slca.algorithm__1.17.R}, 
\emph{slca.postprocess__0.05.R}, 
\emph{slca__1.39.R}, \cr
\emph{solve_add_ridge__0.02.R}, 
\emph{squeeze.cdm__0.03.R}, 
\emph{summary.din__1.18.R}, \cr
\emph{summary.gdina.wald__0.02.R}, 
\emph{summary.gdina__1.27.R}, 
\emph{summary.gdm__1.18.R}, \cr
\emph{summary.IRT.RMSD__0.08.R}, 
\emph{summary.IRT.RMSD_print_statistics__0.03.R}, \cr
\emph{summary.mcdina__0.12.R}, 
\emph{summary.slca__1.17.R}, 
\emph{summary_sink__0.02.R}, 
\emph{vcov.din__1.55.R}, \cr
\emph{vcov.loglike.din__0.02.R}, 
\emph{WaldTest__0.08.R}, 
\emph{zzz__1.12.R}, 
}
\section{\emph{Rcpp} Function Versions }{
\emph{calc_posterior__1.01.c}, 
\emph{cdm_kli_id_c__3.04.cpp}, \cr
\emph{din.deterministic.devcrit_c__3.02.cpp}, 
\emph{gdd_c__3.01.cpp}, 
\emph{init__0.02.c}, \cr
\emph{irt_predict_c__3.01.cpp}, 
\emph{itemfit_sx2_calc_scoredistribution_cdm__3.01.cpp}, \cr
\emph{modelfit_cor2_c__3.02.cpp}, 
\emph{probs_multcat_items_counts_c__3.02.cpp}, \cr
\emph{RcppExports__0.03.cpp}, 
\emph{slca_cfunctions__3.01.cpp}, 
}
\section{\emph{Rd} Documentation Versions }{
\emph{abs_approx__0.03.Rd}, 
\emph{anova.din__1.22.Rd}, 
\emph{cdi.kli__0.11.Rd}, 
\emph{CDM-package__2.68.Rd}, \cr
\emph{cdm.est.class.accuracy__1.13.Rd}, 
\emph{coef__0.16.Rd}, 
\emph{Data-sim__1.13.Rd}, 
\emph{data.cdm__0.14.Rd}, \cr
\emph{data.dcm__0.18.Rd}, 
\emph{data.dtmr__0.11.Rd}, 
\emph{data.ecpe__0.15.Rd}, 
\emph{data.fraction1__0.12.Rd}, \cr
\emph{data.fraction2__0.15.Rd}, 
\emph{data.hr__0.10.Rd}, 
\emph{data.jang__0.10.Rd}, 
\emph{data.melab__0.10.Rd}, \cr
\emph{data.mg__0.20.Rd}, 
\emph{data.pgdina__0.14.Rd}, 
\emph{data.sda6__0.07.Rd}, 
\emph{data.Students__0.13.Rd}, \cr
\emph{data.timss03.G8.su__0.05.Rd}, 
\emph{data.timss07.G4.lee__0.05.Rd}, \cr
\emph{data.timss11.G4.AUT__0.07.Rd}, 
\emph{deltaMethod__0.05.Rd}, 
\emph{din.deterministic__0.16.Rd}, \cr
\emph{din.equivalent.class__0.17.Rd}, 
\emph{din.validate.qmatrix__1.20.Rd}, 
\emph{din__2.33.Rd}, \cr
\emph{entropy.lca__0.17.Rd}, 
\emph{equivalent.dina__1.12.Rd}, 
\emph{fraction.subtraction.data__2.03.Rd}, \cr
\emph{fraction.subtraction.qmatrix__1.04.Rd}, 
\emph{gdd__0.10.Rd}, 
\emph{gdina.dif__0.13.Rd}, \cr
\emph{gdina.wald__0.22.Rd}, 
\emph{gdina__2.993.Rd}, 
\emph{gdm__4.27.Rd}, 
\emph{ideal.response.pattern__0.05.Rd}, \cr
\emph{IRT.anova__0.03.Rd}, 
\emph{IRT.compareModels__0.12.Rd}, 
\emph{IRT.data__0.05.Rd}, \cr
\emph{IRT.expectedCounts__0.04.Rd}, 
\emph{IRT.factor.scores__0.06.Rd}, 
\emph{IRT.IC__0.06.Rd}, \cr
\emph{IRT.irfprob__0.08.Rd}, 
\emph{IRT.irfprobPlot__0.10.Rd}, 
\emph{IRT.itemfit__0.04.Rd}, \cr
\emph{IRT.jackknife__0.14.Rd}, 
\emph{IRT.likelihood__0.10.Rd}, 
\emph{IRT.modelfit__0.13.Rd}, \cr
\emph{IRT.parameterTable__0.03.Rd}, 
\emph{IRT.repDesign__0.13.Rd}, 
\emph{IRT.RMSD__0.10.Rd}, \cr
\emph{item_by_group__0.12.Rd}, 
\emph{itemfit.rmsea__1.10.Rd}, 
\emph{itemfit.sx2__1.26.Rd}, 
\emph{logLik__0.13.Rd}, \cr
\emph{mcdina__0.20.Rd}, 
\emph{modelfit.cor__1.50.Rd}, 
\emph{numerical_Hessian__0.18.Rd}, 
\emph{osink__0.07.Rd}, \cr
\emph{personfit.appropriateness__0.14.Rd}, 
\emph{plot.din__2.08.Rd}, 
\emph{predict__0.13.Rd}, \cr
\emph{print.summary.din__1.05.Rd}, 
\emph{sequential.items__0.12.Rd}, 
\emph{sim.din__2.05.Rd}, \cr
\emph{sim.gdina__1.17.Rd}, 
\emph{skill.cor__2.06.Rd}, 
\emph{skillspace.approximation__0.07.Rd}, \cr
\emph{skillspace.hierarchy__0.15.Rd}, 
\emph{slca__1.54.Rd}, 
\emph{summary.din__2.05.Rd}, \cr
\emph{summary_sink__0.04.Rd}, 
\emph{vcov__0.27.Rd}, 
\emph{WaldTest__0.07.Rd}, 
}
