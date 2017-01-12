
IRT.RMSD <- function( object )
{
	CALL <- base::match.call()
	mod <- object
	mod_counts <- IRT.expectedCounts(mod)
	
	mod_irfprob <- IRT.irfprob(mod)
	G <- base::attr(mod_counts, "G" )
	I <- base::dim(mod_counts)[1]
	#*** create matrix with results
	RMSD <- base::matrix( NA , nrow=I , ncol=G+1)
	RMSD <- base::as.data.frame(RMSD)
	base::colnames(RMSD) <- base::c("item" , paste0("Group" , 1:G) )
	RMSD$item <- base::dimnames(mod_counts)[[1]]
	chisquare_stat <- MD <- MAD <- RMSD
	RMSD$WRMSD <- NA
	# sample sizes per group
	weight_group <- base::matrix( NA , nrow=I , ncol=G )
	for (gg in 1:G){
		# gg <- 1
		mc_gg <- base::apply( mod_counts[,,,gg] , c(1) , base::sum )
		weight_group[,gg] <- as.vector( mc_gg )
	}
	weight_group <- base::t( base::apply( weight_group , 1 , FUN = function(ww){ 
							ww / sum(ww) } ) )
	# weight_group <- weight_group / base::sum( weight_group )

	#*** extract objects
	
	for (gg in 1:G){
		# gg <- 1	
		pi.k <- base::attr(mod_irfprob, "prob.theta")[ , gg , drop=FALSE ]
		probs <- base::aperm( mod_irfprob , perm= base::c(3,1,2) )
		n.ik <- base::aperm( mod_counts , perm= base::c(3,1,2,4) )[,,,gg,drop=FALSE]
		#*** chi square calculation
		chisquare_stat[,gg+1] <- rmsd_chisquare( n.ik=n.ik , pi.k = pi.k , probs = probs )
		#*** RMSD calculation
		RMSD[,gg+1] <- rmsea_aux( n.ik=n.ik , pi.k = pi.k , probs = probs )
		#*** MD calculation
		MD[,gg+1] <- md_aux( n.ik=n.ik , pi.k = pi.k , probs = probs )
		#*** MAD calculation
		MAD[,gg+1] <- mad_aux( n.ik=n.ik , pi.k = pi.k , probs = probs )
		
	}
	
	M1 <- base::rowSums( RMSD[,2:(G+1) ]^2 * weight_group )
    RMSD$WRMSD <- base::sqrt( M1 )
	
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
	res <- base::list( MD = MD , RMSD = RMSD , MAD = MAD , 
					chisquare_stat = chisquare_stat , 
					CALL = CALL , G = G ,
					RMSD_summary = RMSD_summary , MD_summary = MD_summary,
					MAD_summary = MAD_summary)
	base::class(res) <- "IRT.RMSD"
	base::return(res)
}