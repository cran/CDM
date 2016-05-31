
#####################################################
# GDINA M-step item parameters
gdina_mstep_item_uls <- function( 
         pjjj , Ilj.ast , Rlj.ast , eps , avoid.zeroprobs , 
		 Mjjj , invM.list , linkfct , rule , method ,
		 iter , delta.new, max.increment , fac.oldxsi,
		 jj , delta , rrum.model, delta.fixed , devchange
			){
			
		eps2 <- eps

		if (linkfct == "logit" ){ 
				# pjjj[ pjjj > 1-eps ] <- 1 - eps
				# pjjj[ pjjj < eps ] <- eps
				pjjj <- squeeze.cdm( pjjj , c(eps,1-eps) )
				pjjj <- stats::qlogis( pjjj ) 
				# maxval <- 5 ;  pjjj <- squeeze.cdm( pjjj , c(-maxval , maxval ) )
								}
		#*****
		if (linkfct == "log" ){ 
					# pjjj[ pjjj < eps ] <- eps
					pjjj <- squeeze.cdm( pjjj , c(eps,10) )
					pjjj <- log( pjjj ) 				
									}
								
		Wj <- diag( Ilj.ast )		
		
		if ( avoid.zeroprobs ){
			 ind <- which( Ilj.ast  < 10^(-10)  )
			 if ( length(ind) > 0 ){
				 Wj <- diag( Ilj.ast[-ind] )
				 Mjjj <- Mjjj[ - ind , ]
				 pjjj <- pjjj[ - ind  ]
						}
					}
		
	    if ( ( rule[jj] == "GDINA" )| ( method == "ULS" ) ){ 
				invM <- invM.list[[jj]] 
#				delta.jj <- invM %*% t(Mjjj) %*% pjjj				
				delta.jj <- invM %*% crossprod(Mjjj ,pjjj)
							} else { 
#				invM <- solve( t(Mjjj) %*% Wj %*% Mjjj + diag( rep( eps2 , ncol(Mjjj) )) )
				invM <- solve( crossprod(Mjjj , Wj ) %*% Mjjj + diag( rep( eps2 , ncol(Mjjj) )) )				
#				delta.jj <- invM %*% t(Mjjj) %*% Wj %*% pjjj
				delta.jj <- tcrossprod( invM , Mjjj ) %*% Wj %*% pjjj
								}
		djj <- delta.jj[,1]
		djj.change <- djj - delta[[jj]]
		if (linkfct == "identity" & (iter > 3) ){ 
#		if ( (iter > 3) ){ 
			step.change <- .20
# 			djj.change <- ifelse( abs(djj.change) > step.change ,
#									step.change*sign(djj.change) , djj.change )
 			djj.change <- ifelse( abs(djj.change) > step.change ,
									djj.change / 2 , djj.change )
							}

									
			djj <- delta[[jj]] + djj.change
		if ( linkfct == "identity"){
				if ( sum(djj) > 1 ){ 	djj <- djj / ( sum( djj ) )  }											
									}

		#######################################################################
		iter_min <- 10
#		if (linkfct == "log" & iter > iter_min ){ 
#			if ( rule[jj] == "ACDM" ){
#				if ( sum( djj ) > 0 ){
#					djj <- djj - sum(djj )
#									}
#								}
#								}				
								
								
		djj <- ifelse ( is.na(djj) , delta[[jj]] , djj )
		
#		if ( fac.oldxsi > 0 & (iter > 1 ) ){ 
#				djj <- ( 1 - fac.oldxsi ) * djj + fac.oldxsi * djj_old[[jj]]
#				djj_old[[jj]] <- djj				
#								}
		
		# control
        djj.change <- djj - delta[[jj]]		
		while( max(abs(djj.change)) > max.increment ){
#					djj.change <- djj.change / 2 
				djj.change <- ifelse( abs(djj.change) > max.increment , djj.change / 2 , djj.change )
						}
		djj <- delta[[jj]] + djj.change						

		if ( rrum.model & (iter > 10) ){

	#---
	#  RRUM parametrization
	#  log( P(X=1) ) = b0 + b1*alpha1 + b2 * alpha2 
	#  RRUM:
	#  P(X=1) = pi * r1^( 1- alpha1) * r2^(1-alpha2)
	#  => log( P(X=1) ) = log[ pi * r1 * r2 * r1^(-alpha1) * r2^(-alpha2) ]
	#                   = log( pi ) + log(r1) + log(r2) + -log(r1)*alpha1 + -log(r2) * alpha2
	#  => b1 = -log(r1) and r1 = exp( -b1 )
	#  => log(pi) = b0 + b1 + b2 and pi = exp( b0 + b1 + b2 )		
			
			d1 <- djj
# d01 <- d1	
#			d1 <- ifelse( d1 < 0 , .1 , d1 )						
	        sum_d1 <- sum(d1)
			if ( sum_d1 > 0 ){
                d1 <- d1 - sum(d1)
							}

			d1_samp <- stats::runif( length(d1) , 0 , .01 )
			d1[-1] <- ifelse( d1[-1] < 0 , d1_samp[-1] , d1[-1] )						
							
	        sum_d1 <- sum(d1)
			if ( sum_d1 > 0 ){
                d1 <- d1 - sum(d1)
							}														
			djj <- d1										
#			d1 <- djj[-1] 
#			d1 <- ifelse( d1 < 0 , 0.01 , d1 )
#			djj[-1] <- d1			
#			if ( djj[1] > 0 ){
#				djj[1] <- 0
#								}						


						}


		delta.new[[jj]] <- djj
		if ( (fac.oldxsi > 0 ) & (iter>3)){
		    fac.oldxsi1 <- fac.oldxsi * ( devchange >= 0 )
			delta.new[[jj]] <- fac.oldxsi1*delta[[jj]] + ( 1 - fac.oldxsi1 ) * delta.new[[jj]]
						}

		# fix delta parameter here!!
		if ( ! is.null( delta.fixed ) ){
			delta.fixed.jj <- delta.fixed[[jj]]
			if ( ! is.na( delta.fixed.jj)[1] ){
					delta.new[[jj]] <- delta.fixed.jj
									}
							}			
			
			
		res <- list( 
				delta.new = delta.new 
					)	
		return(res)	
												
			}
######################################################			
			
			
