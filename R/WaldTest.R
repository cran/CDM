#################################################
# helper function Wald test
WaldTest <- function( delta , vcov , R , nobs , cvec = NULL , eps=1E-10 ){
	NR <- base::nrow(R)
	if ( base::is.null(cvec) ){
		cvec <- base::rep( 0 , NR )
	}
	Rdii <- R %*% delta - cvec
	v1 <- R %*% vcov %*% base::t(R)
	base::diag(v1) <- base::diag(v1) * ( 1 + eps )	
	stat <- ( base::t( Rdii ) %*% base::solve( v1 ) %*% Rdii )[1,1]
	stats <- base::list()
	stats["X2"] <- stat
	stats["df"] <- NR
	stats["p"] <- stats::pchisq( stat , df = NR , lower.tail=FALSE)
	l1 <- stats$X2 / stats$df - 1
	l1 <- if ( l1 < 0 ){ 0 } else { base::sqrt(l1 / ( nobs - 1 ))	}
	stats["RMSEA"] <- l1
	base::return(stats)
}
##############################################				