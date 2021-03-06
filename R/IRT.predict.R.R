## File Name: IRT.predict.R.R
## File Version: 0.09



########################################################################
# function for predicting item responses based on posterior
IRT.predict.R <- function( object, dat, group=1 )
{

    resp <- dat
    irf1 <- IRT.irfprob( object )
    irf1[ is.na(irf1) ] <- 0
    N <- nrow(resp)
    I <- ncol(resp)
    TP <- dim(irf1)[3]
    K <- dim(irf1)[2]
    if ( length( dim(irf1) )==4 ){
        # handle case with group-wise item response functions
        irf1 <- irf1[,,,group]
    }
    pred <- array( 0, dim=c(N,TP,I) )
    dimnames(pred)[[3]] <- colnames(resp)
    var1 <- pred

    # category-wise predictions
    pred.categ <- array( 0, dim=c(N,K,TP,I) )
    dimnames(pred.categ)[[4]] <- colnames(resp)

    #-------------------------
    for (ii in 1:I){
        # ii <- 32
        v1 <- rep(0,N)
        kk <- 1
        irf.ii <- matrix( irf1[ii,kk,], nrow=N, ncol=TP, byrow=TRUE )
        pred.categ[,kk,,ii] <- irf.ii
        for (kk in 2:K){
            irf.ii <- matrix( irf1[ii,kk,], nrow=N, ncol=TP, byrow=TRUE )
            p1 <- irf.ii
            pred.categ[,kk,,ii] <- p1
            v1 <- (kk-1) * p1 + v1
        }
        pred[,,ii] <- v1
        ind.ii <- which( is.na(resp[,ii]) )
        if ( length(ind.ii) > 0 ){
            pred[ ind.ii,,ii ] <- NA
            pred.categ[ ind.ii, 1:K,,ii ] <- NA
        }

        for (kk in 1:K){
            var1[,,ii ] <- var1[,,ii] + pred.categ[, kk,, ii] * ( ( kk-1 ) - pred[,,ii] )^2
        }
    }

    #----------------------
    # compute residuals
    resp1 <- array( 0, dim=c(N,TP,I) )
    for (tt in 1:TP){
        resp1[,tt,] <- resp
    }
    resid1 <- resp1 - pred
    sresid1 <- resid1 / sqrt( var1 )

    # output
    res <- list( "expected"=pred, "probs.categ"=pred.categ,
                    "variance"=var1, "residuals"=resid1, "stand.resid"=sresid1 )
    return(res)
}
#######################################################################


