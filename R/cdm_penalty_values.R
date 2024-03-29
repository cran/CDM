## File Name: cdm_penalty_values.R
## File Version: 0.202

cdm_penalty_values <- function(x, regular_type, regular_lam, regular_tau=NULL,
        regular_alpha=NULL)
{
    penalty <- 0
    if (regular_type=="scad"){
        penalty <- cdm_penalty_values_scad( x=x, lambda=regular_lam )
    }
    if (regular_type=="lasso"){
        penalty <- cdm_penalty_values_lasso( x=x, lambda=regular_lam )
    }
    if (regular_type=="ridge"){
        penalty <- cdm_penalty_values_ridge( x=x, lambda=regular_lam )
    }
    if (regular_type=="elnet"){
        penalty <- cdm_penalty_values_elnet( x=x, lambda=regular_lam,
                            alpha=regular_alpha )
    }
    if (regular_type=="scadL2"){
        penalty <- cdm_penalty_values_scadL2( x=x, lambda=regular_lam,
                            alpha=regular_alpha )
    }
    if (regular_type=="tlp"){
        penalty <- cdm_penalty_values_tlp_approximation( x=x, lambda=regular_lam,
                            tau=regular_tau )
    }
    if (regular_type=="mcp"){
        penalty <- cdm_penalty_values_mcp( x=x, lambda=regular_lam )
    }
    return(penalty)
}
