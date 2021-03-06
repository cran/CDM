## File Name: reglca_calc_individual_posterior.R
## File Version: 0.116

reglca_calc_individual_posterior <- function(class_probs, p.xi.aj, N, nclasses, weights, W,
            G, ind_groups, N_groups, min_class_probs=1e-3)
{
    #--- single group
    if (G==1){
        p.aj.xi <- cdm_matrix2( class_probs, nrow=N ) * p.xi.aj
        p.aj.xi <- p.aj.xi / rowSums( p.aj.xi )
        class_probs <- colSums( p.aj.xi * weights / W )
        class_probs <- reglca_bound_classprobs(class_probs=class_probs,
                            min_class_probs=min_class_probs)
    }
    #--- multiple groups
    if (G>1){
        p.aj.xi <- matrix(1, nrow=N, ncol=nclasses)
        for (gg in 1:G){
            p.aj.xi[ ind_groups[[gg]], ] <- cdm_matrix2( class_probs[,gg], nrow=N_groups[gg] )
        }
        p.aj.xi <- p.aj.xi * p.xi.aj
        p.aj.xi <- p.aj.xi / rowSums( p.aj.xi )
        for (gg in 1:G){
            ind_gg <- ind_groups[[gg]]
            weights_gg <- weights[ind_gg]
            class_probs[,gg] <- colSums( p.aj.xi[ind_gg,] * weights_gg / W[gg] )
            class_probs[,gg] <- reglca_bound_classprobs(class_probs=class_probs[,gg],
                            min_class_probs=min_class_probs)
        }
    }
    #---- OUTPUT
    res <- list( p.aj.xi=p.aj.xi, class_probs=class_probs)
    return(res)
}
