


########################################################################
# CDM classification accuracy
cdm.est.class.accuracy <- function( cdmobj , n.sims=0 , seed=987){ 
	set.seed(seed)
	# original data
	data <- cdmobj$data
    # likelihood
    p.xi.aj <- cdmobj$like
    # class probabilities
    class.prob <- cdmobj$attribute.patt$class.prob
    # MLE (orginal data)
    c1 <- .est.class.accuracy( p.xi.aj = p.xi.aj , 
                est.class = cdmobj$pattern$mle.est ,
                class.prob= class.prob)
    # MAP (original data)
    c2 <- .est.class.accuracy( p.xi.aj = p.xi.aj , 
                est.class = cdmobj$pattern$map.est ,
                class.prob= class.prob)
    dfr <- rbind( c1 , c2 )
    rownames(dfr) <- c("MLE" , "MAP" )
	#***********************************
	# simulated classification
	
	if ( sum(is.na(data) ) > 0 & n.sims > 0 ){ n.sims <- nrow(data) }
	if ( n.sims > 0 ){
		# splitted attribute pattern
		attr.splitted <- cdmobj$attribute.patt.splitted
		# attribute classes 
		ac <- cdmobj$attribute.patt
		rownames(attr.splitted) <- rownames(ac)
		class.prob <- ac$class.prob
		CC <- nrow(ac)
		# sample patterns
		classes.sim <- sample( 1:CC , prob= class.prob , size=n.sims , replace=TRUE)
		alpha.sim <- attr.splitted[ classes.sim , ,drop=FALSE]
		# simulate according to the din model
		guess0 <- cdmobj$guess[,1]
		slip0 <- cdmobj$slip[,1]
		# simulated data (first data set)
		simdata1 <- sim.din(N=n.sims, q.matrix=cdmobj$q.matrix , 
					guess = guess0 , slip=slip0 ,  rule = cdmobj$rule ,alpha=alpha.sim)
		# simulated data (second data set)
		simdata2 <- sim.din(N=n.sims, q.matrix=cdmobj$q.matrix , 
					guess = guess0 , slip=slip0 ,  rule = cdmobj$rule ,alpha=alpha.sim)
		# handle missings
		d1 <- simdata1[[1]]
		d1[ as.matrix(is.na(data)) ] <- NA
		simdata1[[1]] <- d1
		d1 <- simdata2[[1]]
		d1[ is.na(data) ] <- NA
		simdata2[[1]] <- d1
		# classification of students according
		# fixed item parameters
		I <- length(guess0)
		mod1 <- din( simdata1[[1]] , q.matrix=cdmobj$q.matrix , 
					constraint.guess = cbind(1:I , guess0) , constraint.slip=cbind(1:I , slip0 ) ,  
					rule = cdmobj$rule , progress=FALSE , skillclasses= attr.splitted ,
					zeroprob.skillclasses=cdmobj$zeroprob.skillclasses)
		mod2 <- din( simdata2[[1]] , q.matrix=cdmobj$q.matrix , 
					constraint.guess = cbind(1:I , guess0) , constraint.slip=cbind(1:I , slip0 ) ,  
					rule = cdmobj$rule , progress=FALSE , skillclasses = attr.splitted  ,
					zeroprob.skillclasses=cdmobj$zeroprob.skillclasses)      			
		# compare MLE estimates
		dfr1 <- data.frame( "true" = paste( rownames(alpha.sim)  ) )
		dfr1$mle.simdata1 <- paste( mod1$pattern$mle.est )
		dfr1$mle.simdata2 <- paste( mod2$pattern$mle.est )
		# classification consistency
		dfr11 <- data.frame( "P_c_sim" = mean( dfr1[,2] == dfr1[,3]  ) )
		dfr11$P_a_sim <-  mean( dfr1[,1] == dfr1[,3]  ) 
		# compare MAP estimates
		dfr1 <- data.frame( "true" = paste( rownames(alpha.sim)  ) )
		dfr1$mle.simdata1 <- paste( mod1$pattern$map.est )
		dfr1$mle.simdata2 <- paste( mod2$pattern$map.est )
		dfr12 <- data.frame( "P_c_sim" = mean( dfr1[,2] == dfr1[,3]  ) )
		dfr12$P_a_sim <-  mean( dfr1[,1] == dfr1[,3]  ) 
		dfr11 <- rbind( dfr11 , dfr12 )
		rownames(dfr11) <- c( "MLE" , "MAP" )
		dfr <- cbind( dfr , dfr11 )
		}
	dfr <- dfr[ , order( paste(colnames(dfr)))  ]
    print( dfr , digits=3 )
    invisible(dfr)
        }
########################################################################




#####################################################################################
# estimate classification accuracy
.est.class.accuracy <- function( p.xi.aj , est.class , class.prob ){
    m0 <- matrix(  colSums( p.xi.aj ) , nrow=nrow(p.xi.aj) , ncol=ncol(p.xi.aj) , 
        byrow=TRUE )
    p.xi.aj <- p.xi.aj  / m0
	
    # calculate class index
    est.class.index <- match( est.class , colnames(p.xi.aj ) )
    # calculate formula (5)
    CC <- ncol( p.xi.aj )
    # classification probability matrix
    class.prob.matrix2 <- class.prob.matrix1 <- matrix( NA , CC , CC )
    for (aa in 1:CC){
    for (cc in 1:CC){
    # aa <- 1 ;  cc <- 2
    class.prob.matrix1[cc,aa] <- sum( p.xi.aj[ est.class.index == cc , aa ] )^2
    class.prob.matrix2[cc,aa] <- sum( p.xi.aj[ est.class.index == cc , aa ] )
        }}
    # classification consistency
    P_c <- sum( colSums( class.prob.matrix1 ) * class.prob )
    # marginal classification accuracy
    P_a <- sum( diag( class.prob.matrix2 ) * class.prob )
    res <- data.frame( "P_c" = P_c , "P_a" = P_a )
    return(res)
    }
#####################################################################################


