## File Name: slca_calc_prob.R
## File Version: 0.06


#############################################################
# Rcpp function for calculating probabilities
slca_calc_prob <- function( XdesM, dimXdes, Xlambda )
{
    res <- cdm_rcpp_slca_calc_probs( XdesM=XdesM, dimXdes=dimXdes, Xlambda=Xlambda )
    I <- dimXdes[1]
    maxK <- dimXdes[2]
    TP <- dimXdes[3]
    probs <- array( res, dim=c( I, maxK, TP ))
    return(probs)
}

.slca.calc.prob <- slca_calc_prob
