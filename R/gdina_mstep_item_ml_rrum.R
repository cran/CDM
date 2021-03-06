## File Name: gdina_mstep_item_ml_rrum.R
## File Version: 0.527

#####################################################
# GDINA M-step item parameters
gdina_mstep_item_ml_rrum <- function(
        pjjj, Ilj.ast, Rlj.ast, eps, avoid.zeroprobs,
        Mjjj, invM.list, linkfct, rule, method,
        iter, delta.new, max.increment, fac.oldxsi,
        jj, delta, rrum.model, delta.fixed,
        mstep_iter, mstep_conv, devchange, optimizer="optim" )
{
    eps2 <- eps
    delta_jj <- delta[[jj]]
    delta_jj <- stats::qlogis( logpars2rrumpars(delta_jj) )
    converged <- FALSE
    ii <- 0
    max_increment <- max.increment
    eps <- 1E-6
    eps <- 1E-3
    Rlj.ast <- Rlj.ast + .005
    Ilj.ast <- Ilj.ast + .05
    #*** log-likelihood function
    ll_FUN <- function(x){
        delta_jj <- x
        delta_jj <- rrumpars2logpars( stats::plogis(delta_jj) )
        irf1 <- ( Mjjj %*% delta_jj )[,1]
        irf1 <- exp(irf1)
        irf1 <- cdm_squeeze( irf1, c(eps,1-eps) )
        ll <- - sum( Rlj.ast * log(abs(irf1)) +
                            ( Ilj.ast - Rlj.ast ) * log( abs(1 - irf1 ) ) )
        return(ll)
    }

    ll_new <- ll0 <- ll_FUN(delta_jj)

    avoid_increasing_likelihood <- FALSE
    if (optimizer!="CDM"){
        converged <- TRUE
    }


    #*************************************
    #*** optimization in R
    while ( ! converged ){
        delta_jj0 <- delta_jj
        ll0 <- ll_new
        #*** Newton step
        res1 <- numerical_Hessian(par=delta_jj, h=1E-4,
                    FUN=ll_FUN, gradient=TRUE, hessian=TRUE )
        hessian <- - res1$hessian
        eps0 <- 1E-4
        hessian <- hessian + diag(eps0, nrow(hessian) )
        delta_change <- ( solve(hessian) %*% res1$grad )[,1]
        while( max(abs(delta_change)) > max_increment ){
                delta_change <- ifelse( abs(delta_change) > max_increment,
                                    delta_change /2, delta_change )
        }
        delta_jj <- delta_jj + delta_change
        ll_new <- ll_FUN(delta_jj)
        if ((ll_new > ll0)&(avoid_increasing_likelihood)){
            tt <- .5
            iterate <- TRUE
            iter_max <- 10
            vv <- 1
            while (iterate){
                delta_jj <- delta_jj0 + tt*delta_change
                ll_new <- ll_FUN(delta_jj)
                tt <- tt / 2
                if ( ll_new <=ll0){
                    iterate <- FALSE
                }
                vv <- vv + 1
                if (vv > iter_max){
                    iterate <- FALSE
                    converged <- TRUE
                }
            }
        }

        ii <- ii + 1
        decr <- max( abs( delta_jj - delta_jj0) )
        if ( ii >=mstep_iter ){  converged <- TRUE    }
        if ( decr < mstep_conv ){  converged <- TRUE }
    }

    #******* optimization using optim
    if (optimizer=="optim"){
        mod <- stats::optim(par=delta_jj, fn=ll_FUN, method="L-BFGS-B",
                    control=list(maxit=mstep_iter))
        delta_jj <- mod$par
    }

    #*** retransform
    delta_jj <- stats::plogis( delta_jj )
    delta_jj <- rrumpars2logpars( delta_jj )
    delta.new[[jj]] <- delta_jj
    if ( (fac.oldxsi > 0 ) & (iter>3)){
        fac.oldxsi1 <- fac.oldxsi * ( devchange >=0 )
        delta.new[[jj]] <- fac.oldxsi1*delta[[jj]] + ( 1 - fac.oldxsi1 ) * delta.new[[jj]]
    }

    # fix delta parameter here!!
    if ( ! is.null( delta.fixed ) ){
        delta.fixed.jj <- delta.fixed[[jj]]
        if ( ! is.na( delta.fixed.jj)[1] ){
            delta.new[[jj]] <- delta.fixed.jj
        }
    }
    #*** output
    res <- list( delta.new=delta.new     )
    return(res)
}
######################################################


