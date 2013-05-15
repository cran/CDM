
###########################################################
# RMSEA Item fit
itemfit.rmsea <- function( n.ik , pi.k , probs ){
	# probs ... [ classes , items , categories ]
	# n.ik ... [ classes , items , categories , groups ]	
	# N.ik ... [ classes , items , categories]		
	N.ik <- n.ik[,,,1]
	G <- dim(n.ik)[4]
	pitot <- pi.k[,1]
	if (G>1){ 
		for (gg in 2:G ){
			N.ik <- N.ik + n.ik[,,,gg]
			pitot <- pitot + pi.k[,gg]
				}
			}
	# calculate summed counts
	N.ik_tot <- array( 0 , dim=dim(N.ik) )
	N.ik_tot[,,1] <- N.ik[,,1,drop=FALSE]
	K <- dim(N.ik)[3]			
	for (kk in 2:K){
		N.ik_tot[,,1] <- N.ik_tot[,,1,drop=FALSE] + N.ik[,,kk,drop=FALSE] 
					}

	for (kk in 2:K){	N.ik_tot[,,kk] <- N.ik_tot[,,1] }
	# calculate itemwise statistics
	p.ik_observed <- N.ik / N.ik_tot
	p.ik_observed[ is.na( p.ik_observed ) ] <- 0
	# define class weights 
	pi.k_tot <- array( 0 , dim=dim(p.ik_observed) )
	for (kk in 1:K){
		pi.k_tot[,,kk] <- matrix( pitot , nrow= dim(pi.k_tot)[1] , ncol=dim(pi.k_tot)[2] , byrow=T )
				}
	# calculate statistics
	dist.item <- pi.k_tot * ( p.ik_observed - probs )^2			
	h1 <- dist.item[,,1]
	for (kk in 2:K){ h1 <- h1 + dist.item[,,kk] }
	itemfit.rmsea <- sqrt( colSums( h1 ) )
	return(itemfit.rmsea)
	}