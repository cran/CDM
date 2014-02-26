
gdina.wald <- function( object ){
	varmat.delta <- object$varmat.delta
	delta <- object$delta
	rule <- object$control$rule
	if ( mean(rule=="GDINA") < 1 ){
	   stop("Specify a full GDINA model for performing a Wald test.\n")
					}
	Mj <- object$control$Mj
	q.matrix <- object$q.matrix
	I <- nrow(q.matrix)
	dat <- object$dat
	stats <- matrix( NA , nrow= I  , ncol=10)
	colnames(stats) <- c( paste0("DINA" , c("_X2" , "_df" , "_p", "_sig" ,  "_RMSEA") ) ,
			paste0("ACDM" , c("_X2" , "_df" , "_p" , "_sig" , "_RMSEA") ) )

	for (ii in 1:I){
		delta.ii <- delta[[ii]]
		var.delta.ii <- varmat.delta[[ii]]
		# number of attributes
		Kii <- sum( q.matrix[ii,] )
		if (Kii>1){
			# Test for DINA model
			Dii <- length(delta.ii)
			R <- matrix( 0 , nrow=Dii-2 , ncol=Dii)
			for (vv in 2:(Dii-1) ){    R[vv-1,vv] <- 1 }
			Rdii <- R %*% delta.ii 
			stat <- ( t( Rdii ) %*% solve( R %*% var.delta.ii %*% t(R) ) %*% Rdii )[1,1]
			stats[ii,"DINA_X2"] <- stat
			stats[ii,"DINA_df"] <- nrow(R)
			stats[ii,"DINA_p"] <- 1 - pchisq( stat , df = nrow(R) )
			l1 <- stats[ii,"DINA_X2"] / stats[ii,"DINA_df"] - 1
			l1 <- ifelse( l1 <0 , 0 , l1 / ( sum( 1 - is.na(dat[,ii] ) ) - 1 ) )
			stats[ii,"DINA_RMSEA"] <- sqrt(l1)
		   
			# Test for ACDM
			R <- matrix( 0 , nrow=Dii-(Kii+1) , ncol=Dii)
			for (vv in 1:(nrow(R)) ){  
			vv1 <- vv + ( Kii +1 )
			R[vv,vv1] <- 1 
			}
			
			Rdii <- R %*% delta.ii 
			v1 <- R %*% var.delta.ii %*% t(R)
			diag(v1) <- diag(v1)+1*10^(-10)
			stats[ii,"ACDM_X2"] <- stat <- ( t( Rdii ) %*% solve( v1 ) %*% Rdii )[1,1]
			stats[ii,"ACDM_df"] <- nrow(R)
			stats[ii,"ACDM_p"] <- 1 - pchisq( stat , df = nrow(R) )
			l1 <- stats[ii,"ACDM_X2"] / stats[ii,"ACDM_df"] - 1
			l1 <- ifelse( l1 <0 , 0 , l1 / ( sum( 1 - is.na(dat[,ii] ) ) - 1 ) )
			stats[ii,"ACDM_RMSEA"] <- sqrt(l1)
				}        
		}

	stats <- data.frame( "item" = colnames(dat) , "NAttr" = rowSums(q.matrix) , stats )
	stats$DINA_sig <- ""
	stats$DINA_sig <- ifelse( stats$DINA_p < .05 , "*"  , stats$DINA_sig )
	stats$DINA_sig <- ifelse( stats$DINA_p < .01 , "**"  , stats$DINA_sig )
	stats$DINA_sig[ is.na(stats$DINA_sig) ] <- ""
	stats$ACDM_sig <- ""
	stats$ACDM_sig <- ifelse( stats$ACDM_p < .05 , "*"  , stats$ACDM_sig )
	stats$ACDM_sig <- ifelse( stats$ACDM_p < .01 , "**"  , stats$ACDM_sig )
	stats$ACDM_sig[ is.na(stats$ACDM_sig) ] <- ""
	res <- list("stats"=stats)
	class(res) <- "gdina.wald"
	return(res)
	}

summary.gdina.wald <- function(object,...){
		stats <- object$stats
		cn <- colnames(stats)
		cn <- cn[-1]
		cn <- cn[ - grep("_sig" , cn) ]
		for (vv in cn){ stats[,vv] <- round(stats[,vv],4) }		
		print(stats)
			}
		