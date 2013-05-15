
#############################################################
# Model fit for din object
modelfit.cor.din <- function( dinobj ){
    mod <- dinobj
    data <- as.matrix( mod$data )
    posterior <- mod$posterior
    probs <- mod$pjk
	data <- as.matrix(data )	
    res <- modelfit.cor( data , posterior , probs ) 
	return(res)	
                        }
##############################################################



#############################################################################
modelfit.cor <-
function( data , posterior , probs ){
	K <- max( apply( data , 2 , max , na.rm=T ) )
	if ( K>1 ){ stop("modelfit.cor only allows for dichotomous data\n") }
    data.resp <- 1 - is.na(data)
    data[ is.na(data) ] <- 9
    data1 <- data*data.resp
    I <- ncol(data)
    # calculate counts (ignore weights here!!) 
    n11 <- t(  ( data==1) * data.resp ) %*% ( ( data==1) * data.resp )
    n10 <- t(  ( data==1) * data.resp ) %*% ( ( data==0) * data.resp )
    n01 <- t(  ( data==0) * data.resp ) %*% ( ( data==1) * data.resp )
    n00 <- t(  ( data==0) * data.resp ) %*% ( ( data==0) * data.resp )
    
    p1 <- colMeans(  ( data==1) * data.resp ) 
    # p0 <- colMeans(  ( data==0) * data.resp ) 
    
    # expected counts
    exp1 <- rep(NA, I )
    for (ii in 1:I){
        # ii <- 1
        pr.ii1 <- matrix( probs[ii,2,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )
        p3ii <-  pr.ii1 * posterior
        exp1[ii] <- sum( rowSums( p3ii ) * data.resp[,ii ] ) / sum( data.resp[,ii] )
                }
    
    #********************************
    # covariances 
    
    ip <- itempairs <- t( combn(I,2 ) )
    colnames(itempairs) <- c("item1" , "item2" )
    itempairs <- as.data.frame( itempairs )            
    itempairs$n11 <- n11[ ip ]
    itempairs$n10 <- n10[ ip ]
    itempairs$n01 <- n01[ ip ]
    itempairs$n00 <- n00[ ip ]
	itempairs$n <- rowSums( itempairs[ , c("n11","n10", "n01","n00") ] )
    itempairs$Exp00 <- itempairs$Exp01 <- itempairs$Exp10 <- itempairs$Exp11 <- NA
    itempairs$corExp <- itempairs$corObs <- NA
    
    m1 <- matrix( c(1,1,1,0,0,1,0,0) , 4 , 2 , byrow=T )
    
	# define further quantities
	itempairs$X2 <- NA
#	itempairs$G2 <- NA	
	itempairs$RESIDCOV <- NA		
	itempairs$Q3 <- NA			
	
    for (ii in 1:(I-1) ){
        for (jj in (ii+1):I){
    # ii <- 1
    # jj <- 2
        diijj <- data.resp[,ii ]*data.resp[,jj ]
        ii1 <- which ( itempairs$item1 == ii &  itempairs$item2 == jj )
        pr.ii1 <- matrix( probs[ii,2,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )
        pr.jj1 <- matrix( probs[jj,2,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )    
        p3ii <-  pr.ii1 * pr.jj1 * posterior
        itempairs[ii1,"Exp11"] <- sum( rowSums( p3ii ) * diijj )
    
        pr.ii1 <- matrix( probs[ii,2,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )
        pr.jj1 <- matrix( probs[jj,1,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )    
        p3ii <-  pr.ii1 * pr.jj1 * posterior
        itempairs[ii1,"Exp10"] <- sum( rowSums( p3ii ) * diijj )
    
        pr.ii1 <- matrix( probs[ii,1,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )
        pr.jj1 <- matrix( probs[jj,2,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )    
        p3ii <-  pr.ii1 * pr.jj1 * posterior
        itempairs[ii1,"Exp01"] <- sum( rowSums( p3ii ) * diijj )
    
        pr.ii1 <- matrix( probs[ii,1,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )
        pr.jj1 <- matrix( probs[jj,1,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )    
        p3ii <-  pr.ii1 * pr.jj1 * posterior
        itempairs[ii1,"Exp00"] <- sum( rowSums( p3ii ) * diijj )
        
        itempairs[ii1, "corObs"]  <-   .corr.wt( x = m1[,1,drop=FALSE] ,  y = m1[,2,drop=FALSE] , 
            w = as.numeric( itempairs[ii1,c("n11","n10","n01","n00") ] ) )
    
        itempairs[ii1, "corExp"]  <-   .corr.wt( x = m1[,1,drop=FALSE] ,  y = m1[,2,drop=FALSE] , 
            w = as.numeric( itempairs[ii1,c("Exp11","Exp10","Exp01","Exp00") ] ) )
		#***
		# Q3 statistic
        pr.ii1 <- matrix( probs[ii,2,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )
        pr.jj1 <- matrix( probs[jj,2,] , nrow= nrow(data) , ncol= dim(probs)[3] , byrow=T )    
        p3ii <-  pr.ii1 * posterior
		expii <- rowSums( p3ii )
        p3jj <-  pr.jj1 * posterior
		expjj <- rowSums( p3jj )
		# calculate residuals
		data.res <- data[, c(ii,jj) ] - cbind( expii , expjj )
		data.res <- data.res[ diijj == 1 , ]
		itempairs[ii1,"Q3"] <- cor(data.res)[1,2]
# print( data.res[1:20,] , digits=4 )
# print( head( itempairs ) )
# stop("here")
		
                            }
                }
    
	##############################
	itempairs$X2 <- ( itempairs$n00 - itempairs$Exp00 )^2 / itempairs$Exp00 +
					( itempairs$n10 - itempairs$Exp10 )^2 / itempairs$Exp10	+
					( itempairs$n01 - itempairs$Exp01 )^2 / itempairs$Exp01	+
					( itempairs$n11 - itempairs$Exp11 )^2 / itempairs$Exp11						
	# G2
#	itempairs$G2 <- itempairs$n00 * log( itempairs$Exp00 / max( itempairs$n00 , .01 ) ) +
#					itempairs$n01 * log( itempairs$Exp01 / max( itempairs$n01 , .01 ) ) +
#					itempairs$n10 * log( itempairs$Exp10 / max( itempairs$n10 , .01 ) ) +
#					itempairs$n11 * log( itempairs$Exp11 / max( itempairs$n11 , .01 ) ) 
#    itempairs$G2 <- -2*itempairs$G2					
	
	itempairs$RESIDCOV <- ( itempairs$n11 * itempairs$n00 - itempairs$n10 * itempairs$n01 ) / itempairs$n^2 -
				( itempairs$Exp11 * itempairs$Exp00 - itempairs$Exp10 * itempairs$Exp01 ) / itempairs$n^2	
	##############################
	# labels
    itempairs$item1 <- colnames(data)[ itempairs$item1 ]
    itempairs$item2 <- colnames(data)[ itempairs$item2 ]

	modelfit <- data.frame( "est" = c( 
			mean( abs( itempairs$corObs - itempairs$corExp ) ) ,
			mean( itempairs$X2 ) , # mean( itempairs$G2) ,
			mean( 100*abs(itempairs$RESIDCOV ) ) ,
			mean( abs( itempairs$Q3 ) )
						) )
	rownames(modelfit) <- c("MADcor" , "MX2" , # "MG2",
				"100*MADRESIDCOV" , "MADQ3" )
    
    "pfit" <- data.frame( "item" = colnames(data) , "pObs" = p1 , "pExp" = exp1 )
	print( round(modelfit,5) , digits=5 )   
#    cat("MAD Correlation (Observed minus Expected)" , round( MADcor , 4 ) , "\n" )    
    res <- list( "modelfit" = modelfit , "itempairs" = itempairs , "pfit" = pfit )
    return(res)
    }

#######################################################################	
	


.corr.wt <- function( x, y, w = rep(1,length(x))) {
#  stopifnot(length(x) == dim(y)[2] )
  w <- w / sum(w)
  # Center x and y, using the weighted means
  x <- x - sum(x * w)
  ty <- y - sum( y * w)
  # Compute the variance
  vx <- sum(w * x * x)
  vy <- sum(w * ty * ty)
  # Compute the covariance
  vxy <- sum(ty * x * w)
  # Compute the correlation
  vxy / sqrt(vx * vy)
}
