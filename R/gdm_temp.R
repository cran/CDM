


###########################################################################
# estimation of b parameters
.gdm.est.b <- function(probs, n.ik, N.ik, I, K, G,b,b.constraint,
	max.increment,a,thetaDes,Qmatrix,TP,TD,msteps,convM ,
	centerintercepts ){		
 	max.increment <- 1
	iter <- 1
	parchange <- 1
	b00 <- b
	while( ( iter <= msteps ) & ( parchange > convM)  ){
		b0 <- b
		probs <- .gdm.calc.prob( a,b,thetaDes,Qmatrix,I,K,TP,TD)								
		d2.b <- d1.b <- matrix( 0 , nrow=I,ncol=K)				
		for (kk in 2:(K+1)){
			for (gg in 1:G){
				d1.b[,kk-1] <- d1.b[,kk-1] - rowSums( t(n.ik[,,kk,gg]) - t(N.ik[,,gg]) * probs[,kk,] )
				d2.b[,kk-1] <- d2.b[,kk-1]  + rowSums( t(N.ik[,,gg]) * ( 1 - probs[,kk,] ) * probs[,kk,] )
							}
						}		
#		increment <- - d1.b / d2.b 
		increment <-  - d1.b / ( abs( d2.b + 10^(-10) ) )
		increment[ is.na(increment) ] <- 0		
		increment <- ifelse(abs(increment)> max.increment, 
					sign(increment)*max.increment , increment )						
# print(increment)					
#        increment.temp <- diff.temp*abs(1/( deriv.temp + 10^(-20) ) )  
#        ci <- ceiling( abs(increment) / ( abs( max.increment ) + 10^(-10) ) )
 #       increment <- ifelse( abs(increment)> max.increment  , 
 #                                   increment /(2*ci) , increment)										
		max.increment <- max(abs(increment)) / .98
		b <- b + increment
		se.b <- sqrt( 1 / abs( d2.b+10^(-10)) )
		if ( ! is.null( b.constraint) ){
			b[ b.constraint[,1:2,drop=FALSE] ] <- b.constraint[,3,drop=FALSE]
			se.b[ b.constraint[,1:2,drop=FALSE] ] <- 0		
				}
		# centerintercepts
		if ( centerintercepts) {
		   if (TD==1){
				b <- b - mean(b)		
						}
			if (TD > 1){		
				for (dd in 1:TD){
					ind.dd <- which( Qmatrix[,dd,1] > 0 )
					m1 <- sum( b[ind.dd,] ) / ( ncol(b) * length(ind.dd) )	
					b[ind.dd,] <- b[ind.dd,] - 	m1
							}
						  }
						}				
		iter <- iter + 1
		parchange <- max( abs(b0-b))
# cat(iter,parchange , "\n" )
			}
		max.increment <- max( abs( b - b00 ))
		res <- list("b" = b , "se.b" = se.b , "max.increment.b"=max.increment)
		}


###########################################
# estimation of a
# Q matrix [1:I , 1:TD , 1:K]
# thetaDes [TP,TD]
# n.ik [ TP , I , K+1 , G ]
# N.ik [ TP , I , G ]
# probs [I , K+1 , TP ]
.gdm.est.a2 <- function(probs, n.ik, N.ik, I, K, G,a,a.constraint,TD,
				Qmatrix,thetaDes,TP, max.increment ,
				b , msteps , convM , centerslopes ){
	iter <- 1
	parchange <- 1
	a00 <- a
	while( ( iter <= msteps ) & ( parchange > convM )  ){
		a0 <- a
		probs <- .gdm.calc.prob( a,b,thetaDes,Qmatrix,I,K,TP,TD)
		# 1st derivative
		d2.b <- d1.b <- array( 0 , dim=c(I , TD ) )
		for (td in 1:TD){
			for (gg in 1:G){
				for (kk in 2:(K+1)){
					v1 <- colSums( n.ik[,,kk,gg] * Qmatrix[  , td , kk-1 ] * thetaDes[ , td ] )
					v2 <- N.ik[,,gg] * matrix( Qmatrix[,td,kk-1] , nrow=TP , ncol=I) * thetaDes[,td] * 
							t( probs[,kk,] )
					v2 <- colSums(v2)
					d1.b[  , td] <- d1.b[,td] + v1 - v2
							}
						}	
						}
		# 2nd derivative
		for (td in 1:TD){
			for (ii in 1:I){
				v1 <- l0 <- 0
				for (gg in 1:G){
				for (kk in 2:(K+1) ){		# kk <- 2
					v1 <- v1 + N.ik[,ii,gg] * as.vector( ( Qmatrix[ii,td,kk-1] * 
							thetaDes[ , td ] )^2 * t( probs[ii,kk,] ) )
					l0 <- l0 + as.vector ( Qmatrix[ii,td,kk-1] * thetaDes[ , td ]  * t( probs[ii,kk,] ) )
								}
							}				
				d2.b[ii,td] <- sum(v1) - sum( l0^2 * N.ik[,ii,gg] )
					}
					}
			increment <-  d1.b / abs( d2.b + 10^(-10) )
			increment[ is.na(increment) ] <- 0		
			increment <- ifelse(abs(increment)> max.increment, 
						   sign(increment)*max.increment , increment )	
	#		ci <- ceiling( abs(increment) / ( abs( max.increment ) + 10^(-10) ) )
	#        increment <- ifelse( abs(increment)> max.increment  , 
	#                                  increment /(2*ci) , increment)					
			a[,,1] <- a[,,1] + increment
			se.a <- sqrt( 1 / abs( d2.b + 10^(-10) ) )
			if (K>1){ for (kk in 2:K){ a[,,kk] <- a[,,1] }	 }	
			if ( ! is.null( a.constraint) ){
				a[ a.constraint[,1:3,drop=FALSE] ] <- a.constraint[,4,drop=FALSE]
				se.a[ a.constraint[,1:3,drop=FALSE] ] <- 0			
				increment[ a.constraint[,1:2,drop=FALSE] ] <- 0			
					}		
			if (centerslopes){
			  if (TD>1){
				m11 <- t( colSums( a[,,1] ) / colSums( Qmatrix ) )	
				a[,,1] <- a[,,1] / m11[ rep(1,I) , ]
						}
			  if (TD==1){
				m11 <- t( colSums( a ) / colSums( Qmatrix ) )	
				a <- a / m11[ rep(1,I) , ]
						}						
						}
			parchange <- max( abs(a-a0))
			iter <- iter + 1
			}	# end iter
		max.increment <- max(abs(a-a00))
		res <- list( "a" = a , "se.a" = se.a , "max.increment.a" = max.increment)
		return(res)
		}		