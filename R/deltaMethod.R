###############################################################
deltaMethod <- function( derived.pars , est, Sigma , h=1E-5 ){
	#***
	ND <- base::length(derived.pars)
	#** select h parameters according to size of parameters
	abs_par <- base::abs(est)	
	hvec <- h * base::ifelse( abs_par > 1 , abs_par , 1 )
	NP <- base::length(est)
	#** create design matrix
	M0 <- base::matrix( est , nrow=1 , ncol=NP)
	M1 <- base::diag(hvec)	
	
	M1 <- M0[ base::rep(1,NP) , ] + M1
	M2 <- base::as.data.frame( base::rbind( M0 , M1 ) )
	base::colnames(M2) <- base::names(est)
	#--- loop over parameters
	A <- base::matrix( NA , nrow=ND , ncol=NP)
	base::rownames(A) <- base::names(derived.pars)
	base::colnames(A) <- base::names(est)
	derived.est <- base::rep( NA , ND)
	base::names(derived.est) <- base::names(derived.pars)

	for (dd in 1:ND){
		#dd <- 1
		Md <- stats::model.matrix(derived.pars[[dd]] , M2 )
		if ( base::ncol(Md) > 1 ){
			Md <- Md[,2]
		}
		A[ dd , ] <- ( Md[-1] - Md[1] ) / hvec
		derived.est[dd] <- Md[1]
	}
	#--- covariance matrix
	derived.Sigma <- A %*% Sigma %*% base::t(A)
	#--- univariate tests
	se <- base::sqrt( base::diag(derived.Sigma) )
	univarTest <- base::data.frame(
		"parm" = base::names(derived.pars) ,
		"est" = derived.est , "se" = se ,
		"t" = derived.est / se , 
		"p" = 2 * stats::pnorm( - base::abs( derived.est / se) )
			)
	base::rownames(univarTest) <- NULL	
	#--- multivariate test
	R <- base::diag(ND)
	wt <- WaldTest( delta=derived.est , vcov = derived.Sigma , R = R , nobs = NA)	
	#--- output
	res <- base::list( 
			"coef" = derived.est , 
			"vcov" = derived.Sigma ,
			"se" = se ,
			"A" = A ,
			"univarTest" = univarTest , 
			"WaldTest" = wt
				)
	base::return(res)
}
###############################################################