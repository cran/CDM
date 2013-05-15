

################################################################################
# Main function for parameter estimation in cognitive diagnosis models         #
################################################################################

gdina <-
function( data, q.matrix, conv.crit = 0.0001, 
					dev.crit = .1 , maxit = 1000,
					linkfct = "identity" , Mj = NULL , 
					group = NULL , 
					method = "WLS" , 
#					weight.matrix = TRUE , 
					delta.designmatrix = NULL , 
					delta.basispar.lower = NULL , 
					delta.basispar.upper = NULL , 					
					delta.basispar.init = NULL , 
					zeroprob.skillclasses = NULL , 
					reduced.skillspace=TRUE , 
					Z.skillspace = NULL , 
                    weights = rep( 1, nrow( data ) ),  rule = "GDINA", 
                    progress = TRUE , 
					progress.item = FALSE , 
					...
						){
                    
# data: a required matrix of binary response data, whereas the items are in the columns 
#       and the response pattern in the rows. NA values are allowed.
#
# q.matrix: a required binary matrix describing which attributes are required, coded by 1,
#       and which attributes are not required, coded by 0, to master the items, whereas the
#       attributes are in the columns and the items in the rows.
#
# method: WLS (using W matrix) or ULS (without a W matrix) estimation
#
# conv.crit: termination criterion of the iterations defined as the maximum change in parameter
#       estimates. Iteration ends if maximal parameter change is below this value.
#
# maxit: maximal number of iterations.
#
# zeroprob.skillclasses:  an optional vector of integers which indicates which skill classes should have
#							zero probability
#
# weights: an optional vector of weights for the response pattern. Noninteger weights allow for different
#       sampling schemes.
#
# weight.matrix: use weighting matrix in least squares estimation

# rule: an optional character string or vector of character strings specifying the model rule that is used. 
#       The character strings must be of "DINA" or "DINO". If a vector of character strings is specified, 
#       implying an itemwise condensation rule, the vector must be of length ncol(data). The default is the 
#       condensation rule "DINA" for all items.
#		See help: DINA, DINO, ACDM (=GDINA1), GDINA1, GDINA2
#		The saturated specification GDINA is the default.
#
# progress: an optional logical indicating whether the function should print the progress of iteration.


    cat("---------------------------------------------------------------------------------\n")
		d1 <- packageDescription("CDM")
		cat( paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="") , "\n" )		


########################################################
# add item and attribute labels	if necessary
########################################################

	if ( is.null( colnames( data ) ) ){
			colnames(data) <- paste( "Item" , seq(1,ncol(data)) , sep="")
						}
	if ( is.null( colnames( q.matrix ) ) ){
			colnames(q.matrix) <- paste( "Attr" , seq(1,ncol(q.matrix)) , sep="")
						}

	
################################################################################
# check consistency of input (data, q.matrix, ...)                             #
################################################################################

#    clean <- check.input(data, q.matrix, conv.crit, maxit, constraint.guess,
#        constraint.slip, guess.init, slip.init, weights, rule, progress)   

#    if (is.character(clean)) return(clean)
		dat.items <- data
	
		# check of admissible rules
		admiss.rules <- c("GDINA" , "ACDM" , "DINA" , "DINO" ,
							"GDINA1" , "GDINA2" )
		i1 <- which( ! ( rule %in% admiss.rules ) )
		if ( length(i1) > 0 ){
			cat("The following rules are not implemented in gdina: ")
			cat( paste( unique( rule[i1] ) , collapse= " " ) , "\n" )
			stop("Change your argument 'rule'")
				}
							
	
#    dat.items <- clean$data; q.matrix <- clean$q.matrix; conv.crit <- clean$conv.crit;
#    maxit <- clean$maxit; constraint.guess <- clean$constraint.guess; 
#    constraint.slip <- clean$constraint.slip; guess.init <- clean$guess.init;
#    slip.init <- clean$slip.init; weights <- clean$weights; rule <- clean$rule;
#    progress <- clean$progress    

################################################################################
# model specification: DINA, DINO or itemwise specification of DINA or DINO    #
################################################################################

#****
# include specifications here
#****
	r1 <- "GDINA Model"

################################################################################
# multiple group estimation
################################################################################

	
	G <- 1
	if ( ! is.null( group) ){
		groups <- sort( unique( group) )
		G <- length(groups)	
		group2 <- match( group , groups )
		groupre <- FALSE
		if ( any( group != group2 ) ){
				group <- group2
				groupre <- TRUE
								}
							}	
							
################################################################################
# display on R console                                                         #
################################################################################

    disp <- r1      
    cat(disp,"\n")
	cat( " Link function:" , linkfct , "\n")
	if (G>1){ 
		cat(" Multiple group estimation with",G,"groups\n")
		if (groupre){ cat( "  Renumbered group identifier from 1 to",G,"\n") }
			}
    s1 <- Sys.time()
    cat( "**", paste(s1), "\n"   )
    cat("---------------------------------------------------------------------------------\n")
    flush.console()

################################################################################
# definition of model parameters                                               # 
################################################################################

    I <- nrow(dat.items)   # number of persons
    J <- ncol(dat.items)   # number of items
    K <- ncol(q.matrix)       # number of attributes
    L <- 2^K               # number of latent class pattern of attributes
    dat.items <- as.matrix( dat.items)
    q.matrix <- as.matrix( q.matrix)
            
	if ( length(rule) == 1){ 
			rule <- rep( rule , J )
						}


# a0 <- Sys.time()
						
################################################################################
# Initialization and missing data handling                                     #
################################################################################

    # initialize guessing and slipping parameters
    # without constraints, the default is set equal to .2 for all items
#    guess <- guess.init ; slip <- slip.init
    
    # missing data is coded by 9
    resp <- 1 - is.na(dat.items)
    dat.items[ resp == 0 ] <- 9
    
    # standardize weights such that the sum of defined weights is equal to the number of rows in the data frame
    weights <- nrow(dat.items)*weights / sum(weights )
	

# vv <- "init" ; a1 <- Sys.time() ; cat( vv , a1-a0 , "\n") ; a0 <- Sys.time()
# print("a200")
	
################################################################################
# calculate item response patterns                                             #
################################################################################

    # string with item response patterns
#    item.patt.subj <- sapply( 1:I, FUN = function(ii){ paste( dat.items[ ii, ], collapse="" )  } )
	 item.patt.subj <- dat.items[,1]
	 for (jj in 2:J){
		item.patt.subj <- paste( item.patt.subj , dat.items[,jj] , sep="")
					}	
    # calculate frequency of each item response pattern
    item.patt <- table( item.patt.subj )
	
    # sort item response pattern according to their absolute frequencies
    six <- sort( item.patt, index.return=F, decreasing=T)
    # define data frame 'item.patt' with item response pattern and its frequency (weight)
    item.patt <- cbind( "pattern" = rownames(six), "freq" = as.numeric(as.vector( six ) ) )

    # calculate weighted frequency for each item response pattern
	if (G== 1){ 
#		item.patt[,2] <- sapply( 1:( nrow(item.patt) ), FUN = function(kk){
#							sum( weights * ( item.patt[ kk, 1] == item.patt.subj  ) )
#							} )  
		h1 <- rowsum( weights , item.patt.subj )	
		item.patt[,2] <- h1[ match( item.patt[,1] , rownames(h1) ) , 1]
							
		item.patt.freq <- as.numeric(item.patt[,2])
				}
	if (G > 1){
		item.patt.freq <- matrix( 0 , nrow(item.patt) , G )
		for (gg in 1:G){ 
#			item.patt[,2] <- sapply( 1:( nrow(item.patt) ), FUN = function(kk){
#								sum( weights * ( item.patt[ kk, 1] == item.patt.subj  ) * (group == gg ) )
# 								} )  
		h1 <- rowsum( weights * (group == gg ), item.patt.subj )	
		item.patt[,2] <- h1[ match( item.patt[,1] , rownames(h1) ) , 1]
							
			item.patt.freq[,gg] <- as.numeric(item.patt[,2])
						}
			 }

# vv <- "item response patterns" ; a1 <- Sys.time() ; cat( vv , a1-a0 , "\n") ; a0 <- Sys.time()
# print("a300")
		 
################################################################################ 
# generate all attribute patterns                                              #
################################################################################

    attr.patt <- matrix( rep( 0, K*L) , ncol=K)
    h1 <- 2

	if (K>1){ 
		for(ll in 1:(K-1) ){
			lk <- combn( 1:K, ll ) 
			lk		
			for ( jj in 1:( ncol(lk) ) ){ 
				attr.patt[ h1, lk[,jj] ] <- 1
				h1 <- h1 + 1
				}
			}
		attr.patt[ L, ] <- rep( 1, K )
			}
	 if (K==1){ attr.patt[,K] <- c(0,1) }

    # combine all attributes in an attribute pattern as a string
    attr.patt.c <- apply( attr.patt, 1, FUN = function(ll){ paste(ll,collapse="" ) } )

	# create designmatrix for reduced skill space
    if ( K < 4 | ( ! is.null( zeroprob.skillclasses ) ) | G > 1 ){ 
			reduced.skillspace <- FALSE 
				}
	if ( ! is.null(Z.skillspace) ){ 
			reduced.skillspace <- TRUE 
			Z.skillspace <- as.matrix(Z.skillspace)
			}
	Z <- NULL ; covbeta <- NULL ; beta <- NULL
	ncolZ <- nrow(attr.patt)-1
	if ( reduced.skillspace ){
		A <- attr.patt
		# combinations
		kombis <- combn( K , 2 )	
		KK <- ncol(kombis)
		B <- NULL
		for (kk in 1:KK){
			B <- cbind( B , attr.patt[ , kombis[1,kk] ] * attr.patt[ , kombis[2,kk] ] )
					}
		 Z <- cbind( 1 , A , B )
		 ncolZ <- ncol(Z)
     	v1 <- c("Int" ,  paste("A",1:K , sep="") ) 		 
		v1 <- c(v1,apply( kombis , 2 , FUN = function(ll){ 
			paste( paste( "A" , ll , sep="") , collapse="_" ) } ))
		colnames(Z) <- v1	
		if ( ! is.null(Z.skillspace) ){ 
				Z <- Z.skillspace
						}
		 ncolZ <- ncol(Z)						
			}

# print("a400")			
# vv <- "attribute patterns" ; a1 <- Sys.time() ; cat( vv , a1-a0 , "\n") ; a0 <- Sys.time()			

################################################################################
# uniform prior distribution of all latent class patterns                      #
################################################################################

	if (G== 1){ 
		attr.prob <- rep( 1/L, L )
			} else {
		attr.prob <- matrix( 1/L , L , G )
					}

################################################################################
# create design matrices 
################################################################################	

	Mj.userdefined <- TRUE
	if ( is.null(Mj) ){ 
			Mj.userdefined <- FALSE
			Mj <- as.list( rep("noe" , J ) ) 
					}
	# Notation for Mj and Aj follows De La Torre (2011)
	Aj <- NULL
	Nattr.items <- rowSums(q.matrix)
	# list of necessary attributes per item
	necc.attr <- as.list( rep(NA,J) )
	# list of rows in attr.patt which correspond to attribute classes 
		# for one item
	attr.items <- NULL
	# list of indices of attribute patterns which should be
	#    aggregated for each item
	aggr.attr.patt <- NULL
	for (jj in 1:J){ 	# loop over items jj

		nj1 <- necc.attr[[jj]] <- which( q.matrix[jj,] > 0 )
		if ( length(nj1)==0 ){ 
				stop( paste("Q matrix row " , jj , " has only zero entries\n" , sep="") ) 
						}	
		Aj1 <- Aj[[jj]] <- .create.Aj( Nattr.items[jj] )
		if ( ! Mj.userdefined ){ 
				Mj[[jj]] <- .create.Mj( Aj[[jj]] , rule = rule[jj] )
						}
 		l1 <- as.list( 1 )
		l2 <- rep(0,L)
		for (zz in seq(1,nrow(Aj1)) ){ 
			#	zz <- 1
 			Aj1zz <- outer( rep(1,nrow(attr.patt)) , Aj1[zz,] )
			l1[[zz]] <- which( rowMeans( attr.patt[ , nj1 ] == Aj1zz  ) == 1)
			l2[ l1[[zz]] ] <- zz
						}
		attr.items[[jj]] <- l1
		aggr.attr.patt[[jj]] <- l2
						}	# end item jj
	# indices for Mj
	Mj.index <- matrix( 0 , J , 6 )
	for (jj in 1:J){
			Mj.index[jj,1] <- ncol( Mj[[jj]][[1]] )	
			Mj.index[jj,4] <- nrow( Aj[[jj]])
					}
	Mj.index[,3] <- cumsum( Mj.index[,1] )
	Mj.index[,2] <- c(1,Mj.index[-J,3] + 1 )
	Mj.index[,6] <- cumsum( Mj.index[,4] )	
	Mj.index[,5] <- c(1,Mj.index[-J,6] + 1 )	
	# compute designmatrix of aggregation of pattern
	aggr.patt.designmatrix <- matrix( 0 , L , max(Mj.index[,6]) )
	for (jj in 1:J){
#		jj <- 2
		Mj.index.jj <- Mj.index[jj,]
		for (vv in seq(1,Mj.index.jj[4]) ){
			aggr.patt.designmatrix[ , Mj.index.jj[5] - 1 + vv ] <- 1  * ( aggr.attr.patt[[jj]] == vv )
								}				
				}

				
###############################################################################
# initial item parameters
###############################################################################

	delta <- NULL
	#****
	# identity link
	if (linkfct == "identity" ){ 	
		for ( jj in 1:J){
			N1jj <- ncol(Mj[[jj]][[1]])
			l1 <- rep(0,N1jj)
			l1[1] <- .2
#			if ( FALSE ){
#				Qjj <- sum(q.matrix[jj,])
#				l1[ intersect( seq(2,Qjj),1:N1jj) ] <- .7 / Qjj
#						} else {
#			l1[N1jj] <- .6
			l1[2:N1jj] <- rep( .6 / (N1jj - 1) , N1jj - 1 )
#						}
			delta[[jj]] <- l1
						}
					}
	#*****
	# logit link
	if (linkfct == "logit" ){ 	
		for ( jj in 1:J){
			N1jj <- ncol(Mj[[jj]][[1]])
			l1 <- rep(0,N1jj)
			l1[1] <- -1
			l1[N1jj] <- 1
			delta[[jj]] <- l1
						}
					}
	#*****
	# log link
	if (linkfct == "log" ){ 	
		for ( jj in 1:J){
			N1jj <- ncol(Mj[[jj]][[1]])
			l1 <- rep(0,N1jj)
			l1[1] <- -1.5
			l1[N1jj] <- .75
			delta[[jj]] <- l1
						}
					}	
	###########################
	# import inits delta basis parameter
	if ( ! is.null( delta.basispar.init ) ){
		u.delta <- delta.designmatrix %*% delta.basispar.init
		for (jj in 1:J){
			delta[[jj]] <- u.delta[ seq( Mj.index[jj,2] , Mj.index[jj,3] ) , 1]
						}
					}		
    #----------------------------------------
	# compute inverse matrices for least squares estimation
	invM.list <- list( 1:J )
	for (jj in 1:J){
		Mjjj <- Mj[[jj]][[1]]
		invM.list[[jj]] <- solve( t(Mjjj) %*% Mjjj	)
				}

# print("a700")

 #vv <- "Mj / Aj" ; a1 <- Sys.time() ; cat( vv , a1-a0 , "\n") ; a0 <- Sys.time()			

				
################################################################################
# some prelimaries for EM algorithm                                            #  
################################################################################

    # split item response pattern in a data frame with items as columns
    spl <- sapply( as.vector(item.patt[,1]), FUN = function(ii){ strsplit( ii, split = NULL) } ) 
    item.patt.split <- matrix( rep( 0, length(spl) * J ), ncol=J )
    for (ll in 1:length(spl) ){
        item.patt.split[ ll, ] <- as.numeric( spl[[ll]] )
        }

    # response pattern matrix: each observed entry corresponds to a 1, each unobserved entry to a 0
    resp.patt <- 1* ( item.patt.split != 9 )

    # number of item response patterns
    IP <- nrow(item.patt.split)           
   
    iter <- 1 # Iteration number
    likediff <- 1 # Difference in likelihood estimates
    loglike <- 0 # init for log-Likelihood
    
    # init value for maximum parameter change in likelihood maximization
    max.par.change <- 1000
    devchange <- 1000
	
	# response patterns
	cmresp <- colMeans( resp.patt )
    some.missings <- if( mean(cmresp) < 1){ TRUE } else { FALSE }
	
    # calculations for expected counts
	# response indicator list
    resp.ind.list <- list( 1:J )
	for (i in 1:J){ resp.ind.list[[i]] <- which( resp.patt[,i] == 1)  }

# print("a800")
	
	# this matrix is needed for computing R.lj
	if (G==1 ){
		ipr <- item.patt.split * item.patt.freq*resp.patt
				}
       
    disp <- "...........................................................\n"		

# print("B000")


# vv <- "item patt" ; a1 <- Sys.time() ; cat( vv , a1-a0 , "\n") ; a0 <- Sys.time()			


################################################################################
# BEGIN OF THE ITERATION LOOP                                                  #
################################################################################
    
    while ( ( iter <= maxit ) & 
				( ( max.par.change > conv.crit ) | ( devchange > dev.crit  ) )
					){
#a0 <- Sys.time()
################################################################################
# STEP I:                                                                      #
# calculate P(X_i | alpha_l):                                                  # 
# probability of each item response pattern given an attribute pattern         #
################################################################################

	if ( progress ){
		  cat(disp)	
		  cat("Iteration" , iter , "   " , paste( Sys.time() ) , "\n" )	   
				}

		pj1 <- matrix( 0 , nrow = J , ncol = L )
		# calculate P(X_j | alpha_l )
		for (jj in 1:J){
#			jj <- 3
			ajj <- ( aggr.attr.patt[[jj]] )
			mjjj <- Mj[[jj]][[1]][ ajj , ]
			djj <- matrix( delta[[jj]] , L , length(delta[[jj]]) , byrow=TRUE )
			pj1[jj,] <- rowSums( mjjj * djj )
			if (linkfct == "logit"){
				pj1[jj,] <- plogis( pj1[jj,] )
									}
			if (linkfct == "log"){
				pj1[jj] <- exp( pj1[jj,] )
									}
									}
    # restrict probabilities in calculations									
	eps <- 10^(-10)
	pj1[ pj1 < 0 ] <- eps
	pj1[ pj1 > 1] <- 1 - eps	
	
# cat( "\n Step 1a (calculate P(X_j|alpha_l)\n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1									
									
	pjM <- array( NA , dim=c(J,2,L) )
	pjM[,1,] <- 1 - pj1
	pjM[,2,] <- pj1
	h1 <- matrix( 1 , nrow=IP , ncol=L )
	
    res.hwt <- calc_posterior.v2(rprobs= pjM , gwt=h1 , resp=item.patt.split , 
								 nitems= J , 
                                 resp.ind.list=resp.ind.list , normalization=FALSE , 
                                 thetasamp.density= NULL , snodes=0 )	
    p.xi.aj <- res.hwt[["hwt"]]  			

	if ( ! is.null(zeroprob.skillclasses) ){
		p.xi.aj[ , zeroprob.skillclasses ] <- 0
								}	
	
# cat( "\n Step 1 (calc likelihood)\n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1	
	
################################################################################
# STEP II:                                                                     #
# calculate P(  \alpha_l | X_i ):                                              #
# posterior probability of each attribute pattern given the item response pattern
################################################################################

                                           
    # posterior probabilities  P( \alpha_l | X_i ) 
	if (G== 1){ 
		p.aj.xi <- outer( rep(1,IP), attr.prob ) * p.xi.aj 
			 } else {
			 p.aj.xi <- array( 0 , c( IP , L , G ) )
		for (gg in 1:G){
			p.aj.xi[,,gg] <- outer( rep(1,IP), as.vector(attr.prob[,gg]) ) * p.xi.aj
						}
				}
			 
	if (G == 1){ 
		if ( ! is.null( zeroprob.skillclasses ) ){
			p.aj.xi[ , zeroprob.skillclasses ] <- 0 
						}
		p.aj.xi <- p.aj.xi / rowSums( p.aj.xi )
		# calculate marginal probability P(\alpha_l) for attribute alpha_l
		if (! reduced.skillspace ){
			attr.prob <- colSums( p.aj.xi * item.patt.freq / I )
							}
				}
    if ( G > 1 ){ 					
			if ( ! is.null( zeroprob.skillclasses ) ){
			 for (gg in 1:G){ 
					p.aj.xi[ , zeroprob.skillclasses , gg ] <- 0 
							}
						}	
		for( gg in 1:G){
			p.aj.xi[,,gg] <- p.aj.xi[,,gg] / rowSums( p.aj.xi[,,gg] )
			Igg <- sum( item.patt.freq[,gg] )
			attr.prob[,gg] <- colSums( p.aj.xi[,,gg] * item.patt.freq[,gg] / Igg )
						}
					}

# cat( "\n Step 2 (calc P(alpha|xi) \n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1					
					
#######################################################################
# STEP IIa: reduction of skill space					
#######################################################################

# This currently only works in case of a single group
	if (reduced.skillspace){
		eps <- 10^(-10)
#		p.aj.xi1 <- p.aj.xi
#		p.aj.xi1[ p.aj.xi1 < 0 ] <- 0
		ntheta <- colSums( outer( item.patt.freq , rep( 1 , L) )*p.aj.xi )
		lntheta <- matrix(log(ntheta+eps),ncol=1 )
		V <- diag( ntheta)
		Z1 <- t(Z) %*% V %*% Z
		diag(Z1) <- diag(Z1)+eps
		covbeta <- solve( Z1 )
		beta <- covbeta  %*% ( t(Z) %*% V %*% lntheta )
		pred.ntheta <- exp( Z %*% beta )
		# calculate attribute probability
		attr.prob <- ( pred.ntheta / sum(pred.ntheta ) )[,1]
			}

# cat( "\n Step 2a (reduced skillspace) \n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1				
			
################################################################################
# STEP III:                                                                    #
# calculate I_{lj} and R_{lj}                                                  #
# for a derivation see De La Torre (2008, Journal of Educational and           #
# Behavioral Statistics)                                                       #
# I_{lj} ... expected frequency of persons in attribute class l for item j     #
#               (in case of no missing data I_{lj} = I_l for all items j       #
# R_{lj} ... expected frequency of persons in attribute class l for item j     #
#               which correctly solve item j                                   #
################################################################################

               
	if (G == 1){ 	
		R.lj <- I.lj <- matrix( 0 , nrow=J , ncol=L )
		if ( some.missings ){
#			for (i in 1:J){ 
#				I.lj[i,] <- colSums(  item.patt.freq*resp.patt[,i] * p.aj.xi  )
#						}
			I.lj <- t( item.patt.freq*resp.patt	) %*% p.aj.xi
					} else {
#			I.lj <- matrix( colSums(  item.patt.freq * p.aj.xi  ) , nrow=J , ncol=L , byrow=T )
#			I.lj <- t( item.patt.freq ) %*% p.aj.xi
			I.lj <- matrix( t( item.patt.freq ) %*% p.aj.xi , nrow=J , 
							ncol=L , byrow=TRUE )
					}
					
					
#		for (i in 1:J){ 
#			R.lj[i,] <- colSums(  ipr[,i] * p.aj.xi  )
#			R.lj[i,] <-  t( ipr[,i] ) %*% p.aj.xi 
#					}
		R.lj <- t(ipr) %*% p.aj.xi				
		colnames(I.lj) <- colnames(R.lj) <- attr.patt.c
		rownames(I.lj) <- rownames(R.lj) <- colnames(data)

				}	# end one group

				
	if (G > 1){ 					 
		R.lj.gg <- I.lj.gg <- array( 0 , c( J, L , G ) )
		for (gg in 1:G){ 		
		if ( some.missings ){
#			for (i in 1:J){ 
#				I.lj.gg2[i,,gg] <- colSums(  item.patt.freq[,gg]*resp.patt[,i] * p.aj.xi[,,gg]  )
#						}								
				I.lj.gg[,,gg] <- t( item.patt.freq[,gg]*resp.patt	) %*% p.aj.xi[,,gg]	
					} else {
#			I.lj.gg2[,,gg] <- matrix( colSums(  item.patt.freq[,gg] * p.aj.xi[,,gg]  ) , 
#							nrow=J , ncol=L , byrow=T )
#			I.lj.gg[,,gg] <- t( item.patt.freq[,gg] ) %*% p.aj.xi[,,gg]
			I.lj.gg[,,gg] <- t( item.patt.freq[,gg]*resp.patt ) %*% p.aj.xi[,,gg]
					}
#		for (i in 1:J){ 
#			R.lj.gg[i,,gg] <- colSums(  item.patt.split[,i] * 
#					item.patt.freq[,gg]*resp.patt[,i] * p.aj.xi[,,gg]  )
#					}
# 		R.lj <- t(ipr) %*% p.aj.xi	
		    R.lj.gg[,,gg] <- t( item.patt.split  * item.patt.freq[,gg] * resp.patt ) %*% p.aj.xi[,,gg]
			colnames(I.lj.gg) <- colnames(R.lj.gg) <- attr.patt.c
			rownames(I.lj.gg) <- rownames(R.lj.gg) <- colnames(data)
						}
		# calculate I.lj and R.lj
		I.lj <- I.lj.gg[,,1]
		R.lj <- R.lj.gg[,,1]
		for (gg in 2:G){ 
			I.lj <- I.lj + I.lj.gg[,,gg]
			R.lj <- R.lj + R.lj.gg[,,gg]
						}
				}				

	a0 <- Sys.time()
# cat( "\n Step 3 (calculate expected counts) \n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1					
	
################################################################################
# STEP IV:                                                                     #
# M Step																	   # 
# GDINA Model																   #
################################################################################

	# calculation of expected counts
	R.ljM <- R.lj %*% aggr.patt.designmatrix
	I.ljM <- I.lj %*% aggr.patt.designmatrix


	eps <- 10^(-10)
	eps2 <- 10^(-10)

	delta.new <- NULL
	for (jj in 1:J){ 	# begin item
	#	jj <- 2 
		Ajjj <- Aj[[jj]]
		Mjjj <- Mj[[jj]][[1]]
#		Rlj.ast <- aggregate( R.lj[jj,] , list( aggr.attr.patt[[jj]]) , sum )
#		Ilj.ast <- aggregate( I.lj[jj,] , list( aggr.attr.patt[[jj]]) , sum )	
#		pjjj <- Rlj.ast[,2] / Ilj.ast[,2]		
		Rlj.ast <- R.ljM[ jj, Mj.index[jj,5]:Mj.index[jj,6] ]
		Ilj.ast <- I.ljM[ jj, Mj.index[jj,5]:Mj.index[jj,6] ]
		pjjj <- Rlj.ast / Ilj.ast
#		pjjj <- Rlj.ast[,2] / Ilj.ast[,2]
		if (linkfct == "logit" ){ 
				pjjj[ pjjj > 1-eps ] <- 1 - eps
				pjjj[ pjjj < eps ] <- eps
				pjjj <- qlogis( pjjj ) 
								}
		if (linkfct == "log" ){ 
				pjjj[ pjjj < eps ] <- eps
				pjjj <- log( pjjj ) 
								}
#		Wj <- diag( Ilj.ast[,2] )
		Wj <- diag( Ilj.ast )
	    if ( ( rule[jj] == "GDINA" )| ( method == "ULS" ) ){ 
				invM <- invM.list[[jj]] 
				delta.jj <- invM %*% t(Mjjj) %*% pjjj				
							} else { 
				invM <- solve( t(Mjjj) %*% Wj %*% Mjjj + diag( rep( eps2 , ncol(Mjjj) )) )
				delta.jj <- invM %*% t(Mjjj) %*% Wj %*% pjjj
								}
		djj <- delta.jj[,1]
		djj.change <- djj - delta[[jj]]
		if (linkfct == "identity" & iter > 3 ){ 
			step.change <- .20
 			djj.change <- ifelse( abs(djj.change) > step.change ,
									step.change*sign(djj.change) , djj.change )
			djj <- delta[[jj]] + djj.change
				if ( sum(djj) > 1 ){ 
					djj <- djj / sum( djj ) 
							}											
									}
		#######################################################################
		if (linkfct == "log" & iter > 10 ){ 
			if ( rule[jj] == "ACDM" ){
				if ( sum( djj ) > 0 ){
					djj <- djj - sum(djj )
									}
								}
								}				
		djj <- ifelse ( is.na(djj) , delta[[jj]] , djj )
		delta.new[[jj]] <- djj
					}		# end item
	#.............................................................					

	##########################################################################
	# estimation with a design matrix for delta parameters
	##########################################################################
	if ( ! is.null( delta.designmatrix ) ){ 
		u.delta.new <- unlist( delta.new )
		# calculate basis parameter of delta
		delta.basispar <- solve( t( delta.designmatrix) %*% delta.designmatrix ) %*% 
								t(delta.designmatrix) %*% u.delta.new
		if ( ! is.null( delta.basispar.lower )){						
			delta.basispar <- ifelse( delta.basispar < delta.basispar.lower , 
										delta.basispar.lower , delta.basispar )
											}
		if ( ! is.null( delta.basispar.upper )){						
			delta.basispar <- ifelse( delta.basispar > delta.basispar.upper , 
										delta.basispar.upper , delta.basispar )
									}
		delta.new1 <- ( delta.designmatrix %*% delta.basispar )[,1]
		for (jj in 1:J){
			delta.new[[jj]] <- delta.new1[ seq( Mj.index[jj,2] , Mj.index[jj,3] ) ]
						}
									}

									
# cat( "\n Step 4 (m step item parameters) \n" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1									
# stop("here")									
	#################################################
	
    # calculate the updated likelihood    
	p.xi.aj[ p.xi.aj > 1 ] <- 1-10^(-30)
	p.xi.aj[ p.xi.aj < 0 ] <- 10^(-30)	
	if (G==1){ 	
		l1 <- rowSums( p.xi.aj * outer( rep(1,IP), attr.prob )  ) + 10^(-30) 
		l1[ l1 < 0 ] <- 10^(-30)
			}
	if (G>1){
		l1 <- matrix( 0 , IP , G )
		for (gg in 1:G){ 
			l1[,gg] <- rowSums( p.xi.aj * outer( rep(1,IP), attr.prob[,gg] )  ) + 10^(-30) 
			l1[ l1[,gg] < 0 ,gg] <- 10^(-30)
					}
				}
	like.new <- sum( log( l1 ) * item.patt.freq ) 
    likediff <- abs( loglike - like.new )
	loglikeold <- loglike
    loglike <- like.new
    # maximum parameter change
    max.par.change <- max( abs( unlist( delta.new ) - unlist( delta ) ) )
	if ( linkfct %in% c("logit","log") ){
				max.par.change <- max( abs( plogis(unlist( delta.new )) -
										plogis( unlist( delta ) )) )
								}
	
    # define estimates which are updated in this iteration
    delta <- delta.new
    if (progress) {  
	if (progress.item){ 
			g1 <- unlist( lapply( delta , FUN = function(ll){ paste( round(ll,4) , collapse= " " ) } ))
			g1 <- matrix( paste( colnames(data) , g1 ) , ncol=1)
			print(g1)
			}
		cat( "Deviance = "  , round( -2*like.new , 5 ) )
			if (iter >1){ cat(" | Deviance change = " , round( 2*(like.new-loglikeold), 7) ) }
			cat("\n" )
		cat("Maximum parameter change:" , round( max.par.change, 6), "\n") 			
			}
    
    flush.console() # Output is flushing on the console
    iter <- iter + 1 # new iteration number                                    
	devchange <- abs( 2*(like.new-loglikeold) )
	
	}


	
################################################################################
# END OF THE ITERATION LOOP                                                    #
################################################################################


    # calculate posterior probability for each attribute pattern
	if (G==1){
	
		# set likelihood for skill classes with zero probability to zero
	 if ( ! is.null(zeroprob.skillclasses) ){
		p.xi.aj[ , zeroprob.skillclasses ] <- 0
								}
		pattern <- cbind( 
						freq = round(as.numeric(item.patt[,-1]),3),
						mle.est = attr.patt.c[ max.col( p.xi.aj ) ], 
						mle.post = rowMaxs( p.xi.aj ) / rowSums( p.xi.aj ), 
						map.est = attr.patt.c[ max.col( p.aj.xi ) ], 
						map.post = rowMaxs( p.aj.xi )
						)
				}
	if (G>1){
# work on this pattern
#		pattern <- cbind( 
#						freq = round(as.numeric(item.patt[,-1]),3),
#						mle.est = attr.patt.c[ max.col( p.xi.aj ) ], 
#						mle.post = rowMaxs( p.xi.aj ) / rowSums( p.xi.aj ), 
#						map.est = attr.patt.c[ max.col( p.aj.xi ) ], 
#						map.post = rowMaxs( p.aj.xi )
#						)	
		pattern <- NULL
			}
#print(pattern)


    # calculate posterior probabilities for all skills separately
	if (G==1){
		attr.postprob <- p.aj.xi %*% attr.patt
		colnames( attr.postprob ) <- paste("post.attr",1:K, sep="")
		pattern <- cbind( pattern,  attr.postprob )
			}
	
	#####################################################
	# itemwise standard error calculation
	
	varmat.delta <- varmat.palj <-  NULL
	se.delta <- NULL	

	
	delta.summary <- NULL

	if (G == 1){ 	
		PAJXI <-  p.aj.xi
				}
	if (G>1){			
		a1 <- outer( rep(1,nrow(attr.prob) ) , colSums( item.patt.freq ) ) / sum( item.patt.freq)
		attr.prob.tot <- rowSums( attr.prob * a1 )
		PAJXI <- outer( rep(1,IP), attr.prob.tot ) * p.xi.aj
		#	p.aj.xi <- p.aj.xi / rowSums( p.aj.xi )
		PAJXI <- PAJXI / rowSums(PAJXI)
			}

	# matrix form of item.patt.freq
	if (G==1){ item.patt.freq <- matrix( item.patt.freq , ncol=1 ) }
	freq.pattern <- rowSums( item.patt.freq )
	
	for (jj in 1:J){	
	# cat("........",jj,".,,,\n")
			#	jj <- 1		# Item jj
				Ajjj <- Aj[[jj]]
				Mjjj <- Mj[[jj]][[1]]
				Rlj.ast <- aggregate( R.lj[jj,] , list( aggr.attr.patt[[jj]]) , sum )
				Ilj.ast <- aggregate( I.lj[jj,] , list( aggr.attr.patt[[jj]]) , sum )
				pjjj <- Rlj.ast[,2] / Ilj.ast[,2]
				Mjj2 <- Mj[[jj]][[2]]
				apjj <- aggr.attr.patt[[jj]] 
				M1 <- max( apjj )
				p.ajast.xi <- matrix( 0 , nrow=IP , ncol = M1 )
				for (kk in 1:M1){
					pg1 <-  PAJXI[ , apjj == kk  ]					
					if ( is.vector(pg1)){ 
								p.ajast.xi[,kk] <- pg1 
									} else {
								p.ajast.xi[,kk] <- rowSums( pg1 ) 
										}
								}	
				Rlj.ast <- aggregate( R.lj[jj,] , list( aggr.attr.patt[[jj]]) , sum )
				Ilj.ast <- aggregate( I.lj[jj,] , list( aggr.attr.patt[[jj]]) , sum )
				pjjj <- Rlj.ast[,2] / Ilj.ast[,2]
				pjjjM <- outer( rep(1,IP) , pjjj )
				nM <- ncol(pjjjM) 
				x1 <- outer( item.patt.split[,jj] , rep(1,nM) )
				r1 <- outer( resp.patt[,jj] * item.patt.freq , rep(1,ncol(pjjjM) ) )
				# Formula (17) for calculating the standard error
				mat.jj <- p.ajast.xi * ( x1 - pjjjM) / ( pjjjM * ( 1 - pjjjM ) )	
				infomat.jj <- matrix( 0 , nM , nM )
				for (kk1 in 1:nM){
					for (kk2 in kk1:nM){ 
						# kk1 <- 1
						# kk2 <- 1
#							infomat.jj[kk2,kk1] <- infomat.jj[kk1,kk2] <-  
#											sum( mat.jj[,kk1] * mat.jj[,kk2]  )
						#@@ARb (2012-07-20) correction
						# frqeuency weights must be taken into account
						hh1 <- sum( mat.jj[,kk1] * mat.jj[,kk2] * freq.pattern * 
											resp.patt[,jj] * item.patt.split[,jj] )
						infomat.jj[kk2,kk1] <- infomat.jj[kk1,kk2] <-  hh1
										}
									}
				a1 <- NULL
#				try( a1 <- solve( infomat.jj ) )
				try( a1 <- solve( infomat.jj + diag( eps2 , ncol(infomat.jj) ) ) )
				if ( is.null(a1)){ 
						cat( "Item" , colnames(data)[jj] , "Singular covariance matrix\n")
						a1 <- NA*infomat.jj 
							}
				varmat.palj[[jj]] <- Ijj <- a1
				Wj <- diag( Ilj.ast[,2] )	
				if ( ( method == "ULS" ) ){ 
					Wjjj <- solve( t(Mjjj) %*% Mjjj ) %*% t(Mjjj)
									} else {
					Wjjj <- solve( t(Mjjj) %*% Wj %*% Mjjj ) %*% t(Mjjj) %*% Wj
											}
				if ( linkfct == "logit" ){
					pjjj.link <- 1 / ( pjjj * ( 1 - pjjj ) )
					pjjj.link <- diag( pjjj.link )
				    Wjjj <- Wjjj %*% pjjj.link
						}
				if ( linkfct == "log" ){
					pjjj.link <- 1 /  pjjj 
					pjjj.link <- diag( pjjj.link )
					Wjjj <- Wjjj %*% pjjj.link
						}
				varmat.delta[[jj]] <- Wjjj %*% Ijj %*% t(Wjjj)		
				se.jj <- sqrt( diag(varmat.delta[[jj]] )  ) 
								
				delta.summary.jj <-
					data.frame( "link" = linkfct , "item" = colnames(data)[jj] , 
								"itemno" = jj , 
								"type" = Mj[[jj]][2] , 
								"rule" = rule[jj] , 
								"est" = delta[[jj]] , 
								"se" = se.jj
								)
				colnames(delta.summary.jj)[4] <- "partype"					
				delta.summary <- rbind( delta.summary , delta.summary.jj )
				}
		

			
			
	delta.summary$partype.attr <- paste(delta.summary$partype)
	for (jj in 1:J){
		ind.jj <- which( delta.summary$itemno == jj )
		qjj <- which( q.matrix[ jj , ]	> 0 )
		pgjj <- pajj <- paste(delta.summary$partype.attr[ind.jj])
		cjj <- paste(colnames(q.matrix)[qjj])
		NN <- length(pajj)
		pajj <- gsub( "|" , "-" , pajj )
		pajj <- gsub( "=" , "-" , pajj )
		for (nn in 1:NN){
			st1 <- as.numeric(unlist( strsplit( paste(pajj[nn]) , "-" ) ))
			st1 <- st1[ ! is.na( st1 ) ]
			st1 <- st1[ st1 > 0 ]
			pgjj[nn] <- paste( cjj[ st1 ] , collapse="-" )
						}
		delta.summary$partype.attr[ind.jj] <- pgjj
					}

				
    # attribute pattern
	if (G==1){ 
		attr.prob <- matrix( attr.prob, ncol=1)
		colnames( attr.prob ) <- "class.prob"		
			}
	if (G>1){
		colnames( attr.prob ) <- paste( "class.prob.group" , 1:G , sep="")
				}
		rownames( attr.prob ) <- attr.patt.c

	
	if (G==1){   
		# pattern for separate skills
		skill.patt <- matrix(apply( matrix( rep(  attr.prob, K ), ncol=K) * attr.patt, 2, sum ),ncol=1)
#		rownames(skill.patt) <- paste("Skill_", colnames(q.matrix),sep="")
		rownames(skill.patt) <- colnames(q.matrix)
		colnames(skill.patt) <- "skill.prob" 
				}
	if (G>1){   
		skill.patt <- matrix( 0 , K , G )
		for (gg in 1:G){
		skill.patt[,gg] <- matrix(apply( matrix( rep(  attr.prob[,gg], K ), ncol=K) * 
									attr.patt , 2, sum ),ncol=1)
						}
		#		rownames(skill.patt) <- paste("Skill_", colnames(q.matrix),sep="")
		rownames(skill.patt) <- colnames(q.matrix)
		colnames(skill.patt) <- paste( "skill.prob.group"  , 1:G , sep="")
				}
				
				
		#####################################################################		
		# calculation of the AIC und BIC        
		bb <- 0
	#    if ( is.null( constraint.guess ) == F ){  bb <- bb + nrow(constraint.guess) }
	#    if ( is.null( constraint.slip ) == F ){  bb <- bb + nrow(constraint.slip) }
	
		Nipar <- length( unlist( delta) )
		if ( ! is.null( delta.designmatrix ) ){ 
			Nipar <- ncol(delta.designmatrix ) }
		
		Nskillpar <- G*ncolZ - length( zeroprob.skillclasses )		
		Npars <- Nipar  - bb + Nskillpar
		II <- sum( item.patt.freq )
		aic <- -2*loglike + 2 * Npars  
		bic <- -2*loglike + Npars*log(II)
		#         ic$CAIC <- dev + ( log(ic$n) + 1 )*ic$np
		caic <- -2*loglike + ( log(II) + 1 ) * Npars

	if (G==1){	
		rownames( p.aj.xi ) <- rownames( pattern ) # output rownames posterior probabilities    
		pattern <- data.frame(pattern) # convert pattern to numeric format
		for (vv in seq(1,ncol(pattern))[ -c(2,4) ] ){
						pattern[,vv ] <- as.numeric( paste( pattern[,vv] ) ) }
		
		# subject pattern
		item.patt.subj <- data.frame( "case" = 1:(nrow(data) ), 
									   "pattern" = item.patt.subj, 
                                       "pattern.index" = match( item.patt.subj, item.patt[,1] )
												)
											
		# attribute pattern (expected frequencies)
		attr.prob <- data.frame( attr.prob )
		attr.prob$class.expfreq <-  attr.prob[,1] * nrow(data) 
		
		#*****
		# modify output (ARb 2012-06-05)
		pattern <- pattern[ item.patt.subj$pattern.index , ]	
		pattern[,1] <- paste( item.patt.subj$pattern )
		colnames(pattern)[1] <- "pattern"
		p.aj.xi <- p.aj.xi[ item.patt.subj$pattern.index , ]
		rownames(p.aj.xi) <- pattern$pattern
		p.xi.aj <- p.xi.aj[ item.patt.subj$pattern.index , ]
		rownames(p.xi.aj) <- pattern$pattern
		#*****		
		
				}
	############################################
	# item fit [ items , theta , categories ] 
	# # n.ik [ 1:TP , 1:I , 1:(K+1) , 1:G ]
	n.ik <- array( 0 , dim=c(L , J , 2 , 1 ) )
	n.ik[  , , 2 , 1 ] <- t(R.lj)
	n.ik[  , , 1 , 1 ] <- t(I.lj-R.lj)
	pi.k <- array( 0 , dim=c(L,1) )
	
	if (G>1){	# item fit only in multiple group case
		g1 <- colSums( item.patt.freq )
		g1 <- g1 / sum(g1) 
		for (gg in 1:G){		
			pi.k[,1] <- pi.k[,1] + attr.prob[,gg] * g1[gg]
				}
			} 
		
	if (G==1){	# item fit only in one group case
		pi.k[,1] <- attr.prob$class.prob
			} 
		probs <- aperm( pjM , c(3,1,2) )
		itemfit.rmsea <- itemfit.rmsea( n.ik , pi.k , probs )	
		names(itemfit.rmsea) <- colnames(data)

    cat("---------------------------------------------------------------------------------\n")
    res <- list( coef = delta.summary , delta = delta , se.delta = se.delta , 
				"itemfit.rmsea" = itemfit.rmsea , 
				"mean.rmsea" = mean(itemfit.rmsea) ,	
				loglike = loglike, deviance = -2*loglike , G = G , N = colSums( as.matrix(item.patt.freq) ) , 
				AIC = aic, BIC = bic, CAIC = caic , Npars  = Npars , 
				Nipar=Nipar  , Nskillpar = Nskillpar ,
				Nskillclasses = L , 	
				varmat.delta = varmat.delta ,  varmat.palj = varmat.palj ,
                 posterior = p.aj.xi, "like" = p.xi.aj, "data" = data, "q.matrix" = q.matrix,
                 pattern = pattern , attribute.patt = attr.prob, skill.patt = skill.patt,
                 "subj.pattern" = item.patt.subj, "attribute.patt.splitted" = attr.patt, 
				 "pjk" = pjM , 
				 Mj = Mj , Aj = Aj , delta.designmatrix = delta.designmatrix , 
				 "reduced.skillspace" = reduced.skillspace , 
				 "Z.skillspace" = if(reduced.skillspace){ Z } else { NULL } , 
#				 "delta.index" = if( reduced.skillspace){ sum( abs( ntheta-pred.ntheta) )/ (2*sum(ntheta)) } ,
				 beta = beta , covbeta = covbeta , 
				 "display" = disp,
                 "item.patt.split" = item.patt.split, "item.patt.freq" = item.patt.freq,
                 "model.type" = r1 , iter = iter-1 #,
#				 "q.matrix" = q.matrix 
				 ) 
    class(res) <- "gdina"
    return(res)
}
##################################################################

##################################################################
# Summary of the GDINA model
summary.gdina <- function( object , rdigits = 4 , ... ){
	#-------------------------------------------------------
	# INPUT:
	# object	... result from GDINA analysis
	# rdigits 	... number of digits for rounding parameter estimates
	#-------------------------------------------------------
	# Parameter summary
    cat("---------------------------------------------------------------------------------------------------------- \n")
	d1 <- packageDescription("CDM")
	cat( paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="") , "\n" )		
    cat("Generalized DINA Model \n")
    if ( object$G > 1 ){ 
			cat("  Multiple Group Estmation with",object$G , "Groups \n") 
			cat("\n")
						}
	cat( "\nNumber of iterations =" , object$iter , "\n" )
    cat( "Deviance =" , round( object$deviance , 2 ) ) 
	cat( "  | Loglikelihood =" , round( - object$deviance / 2 , 2 ) ,	"\n" )
    cat( "Number of persons =" , object$N , "\n" )    
    cat( "Number of estimated parameters =" , object$Npars , "\n" )    
    cat( "Number of estimated item parameters =" , object$Nipar , "\n" )    	
    cat( "Number of estimated skill class parameters =" , object$Nskillpar )    		
	cat( " (" , object$Nskillclasses , "latent skill classes)\n")
    cat( "\nAIC = " , round( object$AIC , 2 ) , " ; penalty =" , round( object$AIC - object$deviance ,2 ) , "\n" )    
    cat( "BIC = " , round( object$BIC , 2 ) , " ; penalty =" , round( object$BIC - object$deviance ,2 ) , "\n" )  
    cat( "CAIC = " , round( object$CAIC , 2 ) ," ; penalty =" , round( object$CAIC - object$deviance ,2 ) , "\n\n" )  
#	if (object$reduced.skillspace ){ 
#	    cat("Goodness of fit for reduced skillspace\n")
#		cat( "Delta index =" , round( object$delta.index , 3 ) , "\n\n")
#						}
#    cat("Model fit\n")
#	g1 <- gdina.fit( object , print.output = TRUE )				
	###########################################################
	ds <- object$coef
	ind <- which( colnames(ds) %in% c("est", "se" ) ) 
#	if (G>0){ ind <- which( colnames(ds) %in% c("est" ) ) }
	ds[,ind] <- round( ds[,ind] , rdigits )
	cat("----------------------------------------------------------------------------\n")
	cat("\nItem Parameter Estimates \n\n")
	print(ds)
    	if ( ! is.null( object$delta.designmatrix ) ){ 
			cat("\nNote: Standard errors are not (yet) correctly implemented!\n")
											}	

	cat("\nRMSEA Item Fit\n")											
	print( round( object$itemfit.rmsea,3) )
											
  cat("\nMean of RMSEA item fit:" , 
     round( object$mean.rmsea ,3 ) , "\n")											
											
											
	cat("----------------------------------------------------------------------------\n")
	cat("\nSkill Probabilities \n\n")
	print(round(object$skill.patt ,rdigits) )
	if ( ( object$G == 1 ) & (ncol(object$q.matrix ) > 1 )){ 
		cat("----------------------------------------------------------------------------\n")
		cat("\nTetrachoric Correlations \n\n")
		gt1 <- skill.cor( object )
		print(round(gt1$cor.skills,3))
			}
#    if ( ncol(object$q.matrix ) > 1 ){ 
		cat("\n----------------------------------------------------------------------------\n")	
		cat("\nSkill Pattern Probabilities \n\n")
		xt <- round( object$attribute.patt[,1] , rdigits )
		names(xt) <- rownames( object$attribute.patt )
		print(xt)
#			}
		}
##########################################################################



#****************************************************************
# design matrices for GDINA model
.create.Aj <- function(nq){ 
    Aj <- NULL
    if (nq == 1){ Aj <- matrix( c(0,1) , ncol=1 ) }
    if (nq == 2){ 
        Aj <- matrix( c( 0 , 0 ,
                        1 , 0 ,
                        0 , 1 ,
                        1 , 1 ) , byrow=T , ncol=2 )
                    }
    if (nq == 3){ 
        Aj <- matrix( c( 0 , 0 , 0,
                        1 , 0 , 0 ,    0 , 1 , 0,     0 , 0 , 1 ,
                        1 , 1 , 0 ,    1 , 0 , 1 ,   0 , 1 , 1,
                        1 , 1 , 1 ) , byrow=T , ncol=3 )
                    }
    if (nq == 4){ 
        Aj <- matrix( c( 0 , 0 , 0, 0,
                        1,0,0,0 ,   0,1,0,0  , 0,0,1,0  , 0,0,0,1 ,
                        1,1,0,0,  1,0,1,0 ,   1,0,0,1,  0,1,1,0 , 0,1,0,1 , 0,0,1,1 ,
                        1,1,1,0 , 1,0,1,1,   1,1,0,1,   0,1,1,1 , 
                        1 , 1 , 1 ,1) , byrow=T , ncol=4 )
                    }
    if (nq == 5){ 
        Aj <- matrix( c( 0,0,0,0,0   , 
                1,0,0,0,0   , 
                0,1,0,0,0   , 
                0,0,1,0,0   , 
                0,0,0,1,0   , 
                0,0,0,1,0   , 
                1,1,0,0,0   , 
                1,0,1,0,0   , 
                1,0,0,1,0   , 
                1,0,0,0,1   , 
                0,1,1,0,0   , 
                0,1,0,1,0   , 
                0,1,0,0,1   , 
                0,0,1,1,0   , 
                0,0,1,0,1   , 
                0,0,0,1,1   , 
                1,1,1,0,0   , 
                1,1,0,1,0   , 
                1,1,0,0,1   , 
                1,0,1,1,0   , 
                1,0,1,0,1   , 
                1,0,0,1,1   , 
                0,1,1,1,0   , 
                0,1,1,0,1   , 
                0,1,0,1,1   , 
                0,0,1,1,1   , 
                1,1,1,1,0   , 
                1,1,1,0,1   , 
                1,1,0,1,1   , 
                1,0,1,1,1   , 
                0,1,1,1,1   , 
                1,1,1,1,1          ) , byrow=T , ncol=5 )
                    }                 
    if ( nq > 5){   stop( "Design matrices with more than 5 nonzero row entries are not allowed!\n") }
    return(Aj)
        }
#*****************************************************************


#***************************************************************
# design matrix Mj
.create.Mj <- function( Aj , rule = "GDINA" ){
        K <- ncol(Aj)
        Mj <- NULL
        Mj.lab <- NULL
		#***********************************************
        if (K==1){ 
            Mj <- matrix( c( 1,0,
                             1 ,1 ) , byrow=TRUE , ncol=2^1 )
            Mj.lab <- c( "0" , "1" )
                    } 
		#***********************************************
        if (K==2){ 
            Mj <- matrix( c( 1,0,0,0,
                             1,1,0,0,    1,0,1,0 ,
                             1 ,1,1,1 ) , byrow=TRUE , ncol=2^2 )
            Mj.lab <- c( "0" , "1" , "2" , "1-2" )
			if (rule == "DINA"){
				M <- 2^2
				selv <- c(1,M)
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[ selv]
								}
			if (rule == "DINO"){
				M <- 2^2
				selv <- c(1,M)
				Mj <- Mj[,selv]
				Mj[,2] <- 1 ; Mj[1,2] <- 0
				Mj.lab <- Mj.lab[ selv]
				Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
								}
			if (rule == "ACDM" | rule == "GDINA1" ){
				selv <- 1:3
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[ selv ]
								}
			# In case of K = 2, GDINA2 = GDINA					
                    } 
		#***********************************************
        if (K==3){ 
            Mj <- cbind( 1 , sapply( 1:3 , FUN = function(jj){ 1*(Aj[,jj]==1) } ) )            
               Mj.lab <- c( "0" , 1:3 )
            g2 <- combn( 3 , 2 )
                Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
                Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )
            g2 <- combn( 3 , 3 )
			g2 <- matrix( g2 , nrow=length(g2) , ncol=1 )
                Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
                Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )       
			if (rule == "DINA"){
				M <- 2^3
				selv <- c(1,M)
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv]
								}
			if (rule == "DINO"){
				M <- 2^3
				selv <- c(1,M)
				Mj <- Mj[,selv]
				Mj[,2] <- 1 ; Mj[1,2] <- 0
				Mj.lab <- Mj.lab[ selv]
				Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
								}																
								
			if (rule == "ACDM" | rule == "GDINA1"){
				selv <- 1:4
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv ]
								}						
			if (rule == "GDINA2"){
				selv <- 1:7
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv ]
								}					
                    } 
		#********************************************************************************************
        if (K==4){
            Mj <- cbind( 1 , sapply( 1:4 , FUN = function(jj){ 1*(Aj[,jj]==1) } ) )            
               Mj.lab <- c( "0" , 1:4 )
            for (kk in 2:4){ 
                g2 <- combn( 4 , kk )
				if (kk==4){
						g2 <- matrix( g2 , nrow=length(g2) , ncol=1 )
							}
                    Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
                    Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )
                            }
			if (rule == "DINA"){
				M <- 2^4
				selv <- c(1,M)
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv]
								}
			if (rule == "DINO"){
				M <- 2^4
				selv <- c(1,M)
				Mj <- Mj[,selv]
				Mj[,2] <- 1 ; Mj[1,2] <- 0
				Mj.lab <- Mj.lab[ selv]
				Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
								}																
								
			if (rule == "ACDM" | rule == "GDINA1"){
				selv <- 1:5
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv ]
								}
			if (rule == "GDINA2"){
				selv <- 1:( 1 + 4 + 6 )
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv ]
								}									
                   }
		#***************************************
        if (K==5){
            Mj <- cbind( 1 , sapply( 1:5 , FUN = function(jj){ 1*(Aj[,jj]==1) } ) )            
               Mj.lab <- c( "0" , 1:5 )
            for (kk in 2:5){ 
                g2 <- combn( 5 , kk )
				if (kk==5){
						g2 <- matrix( g2 , nrow=length(g2) , ncol=1 )
							}				
                    Mj <- cbind( Mj , sapply( seq( 1 , ncol(g2)) , FUN = function(jj){ rowProds2(Aj[, g2[,jj] ]) } ) )
                    Mj.lab <- c( Mj.lab , apply( g2 , 2 , FUN = function(ll){ paste( ll , collapse="-" ) } ) )
                            }
			if (rule == "DINA"){
				M <- 2^5
				selv <- c(1,M)
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv]
								}
			if (rule == "DINO"){
				M <- 2^5
				selv <- c(1,M)
				Mj <- Mj[,selv]
				Mj[,2] <- 1 ; Mj[1,2] <- 0
				Mj.lab <- Mj.lab[ selv]
				Mj.lab[2] <- gsub("-" , "|" , Mj.lab[2] )
								}																
								
			if (rule == "ACDM" | rule == "GDINA1"){
				selv <- 1:6
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv ]
								}
			if (rule == "GDINA2"){
				selv <- 1:( 1 + 5 + 10 )
				Mj <- Mj[,selv]
				Mj.lab <- Mj.lab[  selv ]
								}									
								
                   }
            res <- list( Mj , Mj.lab )
           return(res)
                     }
#***************************************************************