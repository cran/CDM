##########################################################
# differential item functioning in the GDINA model
# a Wald test is used for testing item-wise DIF
gdina.dif <- function( object ){
	ocontrol <- object$control
	G <- object$G
	J <- ncol(object$dat)
	delta.group <- as.list(1:G)
	varmat.group <- as.list(1:G)
	ocoef <- object$coef
	for (gg in 1:G){  # gg <- 1
		res.gg <- gdina.dif.aux( ocontrol , gg=gg )    
		delta.group[[gg]] <- res.gg$delta
		varmat.group[[gg]] <- res.gg$varmat.delta
				}
	ndj <- res.gg$ndj
				
	# expanded delta vectors and design matrix
	Rdesign <- varmat_all <- delta_all <- as.list(1:J)
	difstats <- data.frame( "item" = colnames(object$dat) , "X2" = NA , "df" = NA)  

	for (jj in 1:J){          
		# jj <- 7           
		nj <- ndj[[jj]]
		delta.jj <- rep(NA , nj*G )
		varmat.jj <- matrix(0 , nj*G , nj*G )
		Rdesign.jj <- matrix(0,nj*(G-1) , nj*G )
		for (gg in 1:G){
			# gg <- 1
			delta.jj[  1:nj + nj*(gg-1)  ] <- delta.group[[gg]][[jj]]
			varmat.jj[ 1:nj + nj*(gg-1) , 1:nj + nj*(gg-1) ] <- varmat.group[[gg]][[jj]]            
			if (gg <G){
				for (vv in 1:nj){
						Rdesign.jj[ vv + nj*(gg-1) , vv + nj*(gg-1) ] <- 1
						Rdesign.jj[ vv + nj*(gg-1) , vv + nj*(gg) ] <- -1
								}
						}
		ocoef[ ocoef$itemno ==	jj , paste0("est_Group",gg	) ] <- delta.group[[gg]][[jj]]
		ocoef[ ocoef$itemno ==	jj , paste0("se_Group",gg	) ] <- sqrt( diag(varmat.group[[gg]][[jj]] ))
						}
		varmat_all[[jj]] <- varmat.jj
		delta_all[[jj]] <- delta.jj            
		Rdesign[[jj]] <- Rdesign.jj            
		# calculate test value
		d0 <- Rdesign.jj %*% delta.jj
		# calculate variance matrix
		ivm <- solve( Rdesign.jj %*% varmat.jj %*% t(Rdesign.jj ) )
		difstats[jj,"X2"] <- ( t(d0) %*% ivm %*% d0 )[1,1]
		difstats[jj,"df"] <- nrow(Rdesign.jj)
			}    
	rownames(ocoef) <- paste0(ocoef$item,"_", ocoef$partype)
	difstats$p <- 1 - pchisq( difstats$X2 , df=difstats$df )
	difstats$p.holm <- p.adjust( difstats$p )               
	res <- list("difstats"=difstats , "coef" = ocoef , 
			"delta_all" = delta_all ,
			"varmat_all" = varmat_all )
	class(res) <- "gdina.dif"
	return(res)
	}
##########################################################

summary.gdina.dif <- function(object,...){
		stats <- object$difstats
		for (vv in 2:ncol(stats) ){ 
			stats[,vv] <- round(stats[,vv],4) 
				}		
		print(stats)
			}