#######################################
# attach all elements in an environment 
  
.attach.environment <- function( res , envir ){
#	e1 <- environment()
	CC <- length(res)
	for (cc in 1:CC){
		assign( names(res)[cc] , res[[cc]] , envir=envir )		
					}
			}

#################################################
# define Q matrix
.gdm.Qmatrix <- function(Qmatrix,irtmodel,I,TD,K,a){
	# Q matrix [1:I , 1:TD , 1:K]
	if ( is.null(Qmatrix) ){
		Qmatrix <- array( 1 , dim=c(I,TD,K) )	
			# modify it possibly
		if (K>1 & ( irtmodel != "2PLcat" ) ){
			for (kk in 2:K){Qmatrix[,,kk] <- kk*Qmatrix[,,1] }
				}				
		if ( irtmodel=="2PLcat"){
			for (kk in 2:(K) ){
				a[,,kk] <- kk * a[,,kk]
				}
			}
		}
	res <- list("Qmatrix" = Qmatrix , "a" = a)
	return(res)
		}			
			
##########################################
# Theta design matrix
.gdm.thetadesign <- function( theta.k , thetaDes , Qmatrix ){
	
	D <- 1  # default dimension 1
	####################
	# definition of theta.k
	if ( ! is.null(Qmatrix) ){
		D <- ncol(Qmatrix)
			if ( length( dim(Qmatrix))==2 ){ 
				Q1 <- array( 0 , dim=c(dim(Qmatrix),1) )
				Q1[,,1] <- Qmatrix
				Qmatrix <- Q1
						}
						}		
#	if ( is.vector(theta.k) ){
	if ( is.numeric(theta.k) ){
			theta.k <- matrix( theta.k , ncol=1 )
			if (D>1){
				th1 <- as.list(1:D)
				for (dd in 1:D){ th1[[dd]] <- theta.k }
				theta.k <- th1
					}
						}	
	if ( is.list( theta.k) ){
			tk <- theta.k
			theta.k <- expand.grid( theta.k )
			colnames(theta.k) <- names(tk)
								}
							
	theta.k <- as.matrix(theta.k)					
	D <- ncol(theta.k)											
	if ( is.null( colnames(theta.k) ) ){
		colnames(theta.k) <- paste0("F",1:D)
				}

				
	##############################										
														
	if ( is.null(thetaDes) ){
		# thetaDes [TP,TD]
		TD <- D
		thetaDes <- matrix( theta.k , ncol=TD )
		colnames(theta.k) -> colnames(thetaDes)
					}
#	if (is.null( colnames(thetaDes) ) ){
#		colnames(thetaDes) <- paste0( "F" , 1:TD )
#					}
	TP <- nrow(thetaDes)	
	TD <- ncol(thetaDes)
	res <- list("D"=D , "TD"=TD , "TP"=TP,"theta.k"=theta.k,
		"thetaDes"=thetaDes , "Qmatrix"=Qmatrix )
	return(res)
		}

########################################################
# create delta design matrix
.gdm.create.delta.designmatrix <- function( delta.designmatrix , 
		TP , D , theta.k , skill.levels,G){
	delta.designmatrix <- rep(1,TP)
	for (dd in 1:D){
		# dd <- 1
		for ( pp in 1:(min( skill.levels[dd]-1 ,3) ) ){
			delta.designmatrix <- cbind( delta.designmatrix , theta.k[,dd]^pp )
						}
					}
	if (D>1){
		for (dd1 in 1:(D-1) ){				
			for (dd2 in (dd1+1):D) {					
				delta.designmatrix <- cbind( delta.designmatrix , theta.k[,dd1]*theta.k[,dd2] )		
								}
							}		
						}
	delta <- matrix(0,ncol(delta.designmatrix),G)
	covdelta <- NULL
			
	res <- list( "delta" = delta , "covdelta" = covdelta , 
		"delta.designmatrix" = delta.designmatrix )
	return(res)
		}
		
		
###############################################
# constraints for item parameters
.gdm.constraints.itempars <- function( b.constraint , a.constraint , 
	K , TD , Qmatrix , a ){
	for (kk in 1:K){
	  for( td in 1:TD){
		# kk <- 1 ; td <- 1
			ind.kk <- which( Qmatrix[ ,td , kk] == 0 )
			a[ ind.kk , td , kk ] <- 0
			if ( length( ind.kk) > 0 ){
				a1 <- cbind( ind.kk  , td , kk , 0 )
				a.constraint <- rbind( a.constraint , a1 )
							}
						}
					}
	if ( ! is.null( a.constraint) ){
		a.constraint <- as.matrix( a.constraint )
					}
    res <- list( "a.constraint" = a.constraint , "b.constraint"=b.constraint , "a"=a)			
	return(res)
		}

##########################################################
# constraints on item parameters
.gdm.constraints.itempars2 <- function( b.constraint , a.constraint , 
	K , TD ,I , dat ){
	K.item <- apply( dat , 2 , max )	
	for (ii in 1:I){	# ii <- 1
	K.ii <- K.item[ii]	
	if ( K.ii < K ){
		for ( kk in (K.ii+1):K){
			b.constraint <- rbind( b.constraint , cbind( ii , kk , -99999 ) )
		   for (td in 1:TD){
				a.constraint <- rbind( a.constraint , cbind( ii , td , kk , -99999 ) )
							}
							}
						}
				}
	res <- list("K.item"=K.item , "a.constraint"=a.constraint ,
			"b.constraint" = b.constraint )
	return(res)
	}
###############################################################			