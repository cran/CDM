
	
##########################################
# auxiliary function
rmsd_chisquare <- function( n.ik , pi.k , probs , eps=10^(-30) ){
	# probs ... [ classes , items , categories ]
	# n.ik ... [ classes , items , categories , groups ]	
	# N.ik ... [ classes , items , categories]	
	N.ik <- n.ik[,,,1]
	G <- base::dim(n.ik)[4]
	pitot <- pi.k[,1]
	eps <- 1E-10
	if (G>1){ 
		for (gg in 2:G ){
			N.ik <- N.ik + n.ik[,,,gg]
			pitot <- pitot + pi.k[,gg]
		}
	}

	#*** extract maximum number of categories
	maxK <- base::apply( N.ik , base::c(2,3) , base::sum , na.rm=TRUE )
	maxK <- base::rowSums( maxK > eps )
	
	# calculate summed counts
	N.ik_tot <- base::array( 0 , dim=dim(N.ik) )
	N.ik_tot[,,1] <- N.ik[,,1,drop=FALSE]
	K <- base::dim(N.ik)[3]			
	for (kk in 2:K){
		N.ik_tot[,,1] <- N.ik_tot[,,1,drop=FALSE] + N.ik[,,kk,drop=FALSE] 
	}

	for (kk in 2:K){	
		N.ik_tot[,,kk] <- N.ik_tot[,,1] 
	}
	E.ik <- N.ik_tot*probs
	#--- calculate chi square
	chisq_stat0 <- ( N.ik - E.ik )^2 / E.ik
	chisq_stat <- chisq_stat0[,,1]
	for (kk in 2:K){
		chisq_stat <- chisq_stat + chisq_stat0[,,kk]
	}
	chisq_stat <- base::colSums(chisq_stat)
		# divide it by maxK?
	base::return(chisq_stat)
}

