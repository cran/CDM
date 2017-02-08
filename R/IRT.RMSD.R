
IRT.RMSD <- function( object )
{
	CALL <- match.call()
	mod <- object
	mod_counts <- IRT.expectedCounts(mod)
	
	mod_irfprob <- IRT.irfprob(mod)
	G <- attr(mod_counts, "G" )
	I <- dim(mod_counts)[1]
	#*** create matrix with results
	RMSD <- matrix( NA , nrow=I , ncol=G+1)
	RMSD <- as.data.frame(RMSD)
	colnames(RMSD) <- c("item" , paste0("Group" , 1:G) )
	RMSD$item <- dimnames(mod_counts)[[1]]
	chisquare_stat <- MD <- MAD <- RMSD
	RMSD$WRMSD <- NA
	# sample sizes per group
	weight_group <- matrix( NA , nrow=I , ncol=G )
	for (gg in 1:G){
		# gg <- 1
		mc_gg <- apply( mod_counts[,,,gg] , c(1) , sum )
		weight_group[,gg] <- as.vector( mc_gg )
	}
	weight_group <- t( apply( weight_group , 1 , FUN = function(ww){ 
							ww / sum(ww) } ) )
	# weight_group <- weight_group / sum( weight_group )

	#*** extract objects
	
	for (gg in 1:G){
		# gg <- 1	
		pi.k <- attr(mod_irfprob, "prob.theta")[ , gg , drop=FALSE ]
		probs <- aperm( mod_irfprob , perm= c(3,1,2) )
		n.ik <- aperm( mod_counts , perm= c(3,1,2,4) )[,,,gg,drop=FALSE]
		#*** chi square calculation
		chisquare_stat[,gg+1] <- rmsd_chisquare( n.ik=n.ik , pi.k = pi.k , probs = probs )
		#*** RMSD calculation
		RMSD[,gg+1] <- rmsea_aux( n.ik=n.ik , pi.k = pi.k , probs = probs )
		#*** MD calculation
		MD[,gg+1] <- md_aux( n.ik=n.ik , pi.k = pi.k , probs = probs )
		#*** MAD calculation
		MAD[,gg+1] <- mad_aux( n.ik=n.ik , pi.k = pi.k , probs = probs )
		
	}
	
	M1 <- rowSums( RMSD[,2:(G+1) ]^2 * weight_group )
    RMSD$WRMSD <- sqrt( M1 )
	
	if ( G==1 ){
		RMSD$WRMSD <- NULL
	}
	
	#*** summaries of statistics
	RMSD_summary <- dataframe_summary( dfr = RMSD , exclude_index=1 , 
						labels = colnames(RMSD)[-1] )

	MD_summary <- dataframe_summary( dfr = MD , exclude_index=1 , 
						labels = colnames(MD)[-1] )

	MAD_summary <- dataframe_summary( dfr = MAD , exclude_index=1 , 
						labels = colnames(MAD)[-1] )
							
	#*** output
	res <- list( MD = MD , RMSD = RMSD , MAD = MAD , 
					chisquare_stat = chisquare_stat , 
					CALL = CALL , G = G ,
					RMSD_summary = RMSD_summary , MD_summary = MD_summary,
					MAD_summary = MAD_summary)
	class(res) <- "IRT.RMSD"
	return(res)
}
