################################################################################
# Main function for parameter estimation in cognitive diagnosis models         #
################################################################################

din <-
function( data, q.matrix, conv.crit = 0.001, maxit = 100,
                    constraint.guess = NULL, constraint.slip = NULL,
                    guess.init = rep(.2, ncol(data) ), slip.init = guess.init,
                    weights = rep( 1, nrow( data ) ),  rule = "DINA", 
                    progress = TRUE ){
                    
# data: a required matrix of binary response data, whereas the items are in the columns 
#		and the response pattern in the rows. NA values are allowed.
#
# q.matrix: a required binary matrix describing which attributes are required, coded by 1,
#		and which attributes are not required, coded by 0, to master the items, whereas the
#		attributes are in the columns and the items in the rows.
#
# conv.crit: termination criterion of the iterations defined as the maximum change in parameter
#		estimates. Iteration ends if maximal parameter change is below this value.
#
# maxit: maximal number of iterations.
#
# constraint.guess: an optional matrix of fixed guessing parameters. The first column of this 
#		matrix indicates the items whose guessing parameters are fixed and the second column 
#		the values the guessing parameters are fixed to.
#
# constraint.slip: an optional matrix of fixed slipping parameters. The first column of this matrix 
#		indicates the items whose guessing parameters are fixed and the second column the values 
#		the guessing parameters are fixed to.
#
# guess.init: an optional initial vector of guessing parameters. Guessing parameters are bounded between
#		0 and 1.
#
# slip.init: an optional initial vector of guessing parameters. Slipping parameters are bounded between
#		0 and 1.
#
# weights: an optional vector of weights for the response pattern. Noninteger weights allow for different
#		sampling schemes.
#
# rule: an optional character string or vector of character strings specifying the model rule that is used. 
#		The character strings must be of "DINA" or "DINO". If a vector of character strings is specified, 
#		implying an itemwise condensation rule, the vector must be of length ncol(data). The default is the 
#		condensation rule "DINA" for all items.
#
# progress: an optional logical indicating whether the function should print the progress of iteration.


	cat("---------------------------------------------------------------------------------\n")

################################################################################
# check consistency of input (data, q.matrix, ...)                             #
################################################################################

	clean <- check.input(data, q.matrix, conv.crit, maxit, constraint.guess,
  		constraint.slip, guess.init, slip.init, weights, rule, progress)   

	if (is.character(clean)) return(clean)
    
	dat.items <- clean$data; q.matrix <- clean$q.matrix; conv.crit <- clean$conv.crit;
	maxit <- clean$maxit; constraint.guess <- clean$constraint.guess; 
	constraint.slip <- clean$constraint.slip; guess.init <- clean$guess.init;
	slip.init <- clean$slip.init; weights <- clean$weights; rule <- clean$rule;
	progress <- clean$progress    

################################################################################
# model specification: DINA, DINO or itemwise specification of DINA or DINO    #
################################################################################

	if ( length(rule) == 1){
	    if ( rule == "DINA" ){ r1 <- "DINA MODEL"  }
	    if ( rule == "DINO" ){ r1 <- "DINO MODEL" }
	    } else { r1 <- "Mixed DINA & DINO Model" }
                                       
################################################################################
# display on R console                                                         #
################################################################################

	disp <- r1      
	cat(disp,"\n")
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
            
################################################################################
# Initialization and missing data handling                                     #
################################################################################

	# initialize guessing and slipping parameters
	# without constraints, the default is set equal to .2 for all items
	guess <- guess.init ; slip <- slip.init
	
	# missing data is coded by 9
	resp <- 1 - is.na(dat.items)
	dat.items[ resp == 0 ] <- 9
	
	# standardize weights such that the sum of defined weights is equal to the number of rows in the data frame
	weights <- nrow(dat.items)*weights / sum(weights )

################################################################################
# calculate item response patterns                                             #
################################################################################

	# string with item response patterns
	item.patt.subj <- sapply( 1:I, FUN = function(ii){ paste( dat.items[ ii, ], collapse="" )  } )
        
	# calculate frequency of each item response pattern
	item.patt <- table( item.patt.subj )

	# sort item response pattern according to their absolute frequencies
	six <- sort( item.patt, index.return=F, decreasing=T)

	# define data frame 'item.patt' with item response pattern and its frequency (weight)
	item.patt <- cbind( "pattern" = rownames(six), "freq" = as.numeric(as.vector( six ) ) )

	# calculate weighted frequency for each item response pattern
	item.patt[,2] <- sapply( 1:( nrow(item.patt) ), FUN = function(kk){
    	                sum( weights * ( item.patt[ kk, 1] == item.patt.subj  ) )
                    	} )                  
	item.patt.freq <- as.numeric(item.patt[,2])


################################################################################ 
# generate all attribute patterns                                              #
################################################################################

	attr.patt <- matrix( rep( 0, K*L) , ncol=K)
	h1 <- 2
	for(ll in 1:(K-1) ){
	    lk <- combn( 1:K, ll ) 
	    lk
	    for ( jj in 1:( ncol(lk) ) ){ 
        	attr.patt[ h1, lk[,jj] ] <- 1
        	h1 <- h1 + 1
        	}
    	}
	attr.patt[ L, ] <- rep( 1, K )

	# combine all attributes in an attribute pattern as a string
	attr.patt.c <- apply( attr.patt, 1, FUN = function(ll){ paste(ll,collapse="" ) } )

################################################################################
# uniform prior distribution of all latent class patterns                      #
################################################################################

	attr.prob <- rep( 1/L, L )
            
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

	# constraints for guessing and slipping parameters
	if ( is.null( constraint.slip ) == FALSE ){  slip[ constraint.slip[,1] ] <- constraint.slip[,2] }
	if ( is.null( constraint.guess ) == FALSE ){  guess[ constraint.guess[,1] ] <- constraint.guess[,2] }
            
	
	iter <- 1 # Iteration number
	likediff <- 1 # Difference in likelihood estimates
	loglike <- 0 # init for log-Likelihood
	
	# init value for maximum parameter change in likelihood maximization
	max.par.change <- 1000
	
	# calculate for each item how many attributes are necessary for solving the items
	# according to the specified DINA or DINO rule                                  
	comp <- ( rowSums(q.matrix)  )*( rule=="DINA")   + 1* ( rule == "DINO" )               
        
        
################################################################################
# BEGIN OF THE ITERATION LOOP                                                  #
################################################################################
	
	while ( iter <= maxit & max.par.change > conv.crit ){

################################################################################
# STEP I:                                                                      #
# calculate P(X_i | alpha_l):                                                  # 
# probability of each item response pattern given an attribute pattern         #
################################################################################
                    
	p.xi.aj <- apply( attr.patt, 1, FUN = function(attr.patt.ll){
                
	# form attribute pattern (Lx1) in matrix (J x 1 )
	attr.patt.ll <- outer( rep(1,J), attr.patt.ll )
	
	# define indicator variable 'ind' which indicates if all necessary attributes for
	#	an item are solved
	ind <- 1*(rowSums(q.matrix * attr.patt.ll)  >= comp )   

	##### (A1) #######
	# calculate item response probability given the attributes according to DINA or DINO rule
	pj <- (1 - slip )*ind + guess * ( 1 - ind )
	# IP ... number of item response patterns
	pj <- outer( rep(1,IP), pj )
           
	##### (A2) #######           
	# calculate likelihood L(X_i | alpha_l )
	# Due to local independence of items, this is a product of item response functions
	rowProds( pj^( resp.patt * item.patt.split ) * ( 1 - pj )^( resp.patt * ( 1 - item.patt.split ) )  )
	} )
           
################################################################################
# STEP II:                                                                     #
# calculate P(  \alpha_l | X_i ):                                              #
# posterior probability of each attribute pattern given the item response pattern
################################################################################
                                           
	# posterior probabilities  P( \alpha_l | X_i ) 
	p.aj.xi <- outer( rep(1,IP), attr.prob ) * p.xi.aj 
	p.aj.xi <- p.aj.xi / rowSums( p.aj.xi )

	# calculate marginal probability P(\alpha_l) for attribute alpha_l
	attr.prob <- colSums( p.aj.xi * item.patt.freq / I )


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
                     
	I.lj <- sapply( 1:L, FUN = function(ll){ # skill pattern ll
	# I_{lj} = \sum_i [ N_i * P( \alpha_l | X_i ) ]
	# I_{lj} is the sum of the product of item reponse pattern and the conditional
	# probability of being in attribute class l given item response pattern
    colSums( item.patt.freq * resp.patt * p.aj.xi[,ll] )    
                            } )
	R.lj <- sapply( 1:L, FUN = function(ll){ # skill pattern ll
	# R_{lj} = \sum_i [ N_i * X_i * P( \alpha_l | X_i ) ]
	# ... expected frequency of a correct response to an item => multiplication with item.patt.split
    colSums( item.patt.split * resp.patt * p.aj.xi[,ll] * item.patt.freq )
                            } )
	colnames(I.lj) <- colnames(R.lj) <- attr.patt.c
	rownames(I.lj) <- rownames(R.lj) <- paste("Item",1:J,sep="")

################################################################################
# STEP IV:                                                                     #
# calculate R_j0, I_j0, I_j1, R_j1                                            #
# R_{j1} ... expected frequency of students which correctly solve item j and   #
#               possess all necessary attributes for this item                 #
# I_{j1} ... expected frequency of students which correctly solve the item     #
# I_{j0}, R_{j0} ... expected frequencies of students which incorrectly        #
#                      solve the item                                          #
################################################################################
                      
	I.j0 <- rowSums( ( t(( attr.patt %*% t(q.matrix) ) )  <  outer(  comp, rep(1,L) ) ) * I.lj ) 
	I.j1 <- rowSums( ( t(( attr.patt %*% t(q.matrix) ) )  >= outer(  comp, rep(1,L) ) ) * I.lj )             
	R.j0 <- rowSums( ( t(( attr.patt %*% t(q.matrix) ) )  <  outer(  comp, rep(1,L) ) ) * R.lj )
	R.j1 <- rowSums( ( t(( attr.patt %*% t(q.matrix) ) )  >= outer(  comp, rep(1,L) ))  * R.lj )

################################################################################
# STEP V:                                                                      #
# M-Step: update slipping and guessing parameters.                             #
# The guessing parameter 'guess.new' can be claculated as R.j0 / I.j0          #
#   -> correct solution for item j if not all necessary attributes are possessed
################################################################################

	I.j0[ I.j0 == 0] <- 0.05
	I.j1[ I.j1 == 0] <- 0.05           
	guess.new <- R.j0 / I.j0
	slip.new <- ( I.j1 - R.j1 ) / I.j1

	# constrained slipping and guessing parameter
	if ( is.null( constraint.slip ) == F ){  slip.new[ constraint.slip[,1] ] <- constraint.slip[,2] }
	if ( is.null( constraint.guess ) == F ){  guess.new[ constraint.guess[,1] ] <- constraint.guess[,2] }

	# calculate the updated likelihood                                               
	like.new <- sum( log( rowSums( p.xi.aj * outer( rep(1,IP), attr.prob )  ) + 10^(-300) ) * item.patt.freq )
	likediff <- abs( loglike - like.new )
	loglike <- like.new

	# maximum parameter change
	max.par.change <- max( abs( guess.new - guess ), abs( slip.new - slip ) )
            
	# define estimates which are updated in this iteration
	guess <- guess.new
	slip <- slip.new
	if (progress == T) {  cat( "Iter. ",iter, " :", 
	    substring( paste(Sys.time()), first=11 ), ", ", " loglike = ", 
    	round( like.new, 7), " / max. param. ch. : ",
	    round( max.par.change, 6), "\n", sep="") }
    
    flush.console() # Output is flushing on the console
    iter <- iter + 1 # new iteration number                                    
}

################################################################################
# END OF THE ITERATION LOOP                                                    #
################################################################################

	if(any(guess > 1 - slip)) 
  		warning(paste("Parameter identification problem in item",ifelse(length(which(guess > 1 - slip))>1,"s "," "),
  	paste(which(guess > 1 - slip), collapse=", "),". See Help-files to fix this problem.", sep=""))


	# calculate posterior probability for each attribute pattern
	pattern <- cbind( freq = round(as.numeric(item.patt[,-1]),3),
    	            mle.est = attr.patt.c[ max.col( p.xi.aj ) ], 
        	        mle.post = rowMaxs( p.xi.aj ) / rowSums( p.xi.aj ), 
            	    map.est = attr.patt.c[ max.col( p.aj.xi ) ], 
                	map.post = rowMaxs( p.aj.xi )
                 	)

	# calculate posterior probabilities for all skills separately
	attr.postprob <- p.aj.xi %*% attr.patt
	colnames( attr.postprob ) <- paste("post.attr",1:K, sep="")
	pattern <- cbind( pattern,  attr.postprob )

################################################################################
# estimation of the standard errors for slipping and guessing parameters       #
# guessing parameters (DINA and DINO)                                          #
# NOTE: In this calculation of standard errors, sample weights were not        #
#   taken into account. Use for example the bootstrap to do some inference.    #
################################################################################

	se.guess <- sapply( 1:J, FUN = function(jj){
            	indA0.jj <- rowSums( q.matrix[jj,] * attr.patt ) < comp[jj]
            	l1 <- rowSums( p.aj.xi * outer( resp.patt[,jj], rep(1,L) ) *
                    	( outer( item.patt.split[,jj ], rep(1,L) ) - guess[ jj ] ) * outer( rep(1,IP), indA0.jj )
                                	)
            	( sum( item.patt.freq * l1^2  ) / guess[jj]^2 / (1-guess[jj])^2 )^(-.5)
	                } )
	
	# constrained guessing parameter
	if ( is.null( constraint.guess ) == F ){  se.guess[ constraint.guess[,1] ] <- NA }
	guess <- data.frame( est = guess, se = se.guess                 )      
	
	# slipping parameter (DINA and DINO)
	se.slip <- sapply( 1:J, FUN = function(jj){
            	indA0.jj <- rowSums( q.matrix[jj,] * attr.patt ) >= comp[jj]
            	l1 <- rowSums( p.aj.xi * outer( resp.patt[,jj], rep(1,L) ) *
                    	( outer( item.patt.split[,jj ], rep(1,L) ) - 1 + slip[ jj ] ) * outer( rep(1,IP), indA0.jj )
                                	)
            	( sum( item.patt.freq * l1^2  ) / slip[jj]^2 / (1-slip[jj])^2 )^(-.5)
	                } )
	
	# constrained slipping parameter
	if ( is.null( constraint.slip ) == F ){  se.slip[ constraint.slip[,1] ] <- NA }        
	slip <- data.frame( est = slip, se = se.slip )
	
	# attribute pattern
	attr.prob <- matrix( attr.prob, ncol=1)
	colnames( attr.prob ) <- "class.prob"
	rownames( attr.prob ) <- attr.patt.c
	
	# pattern for seperate skills
	skill.patt <- matrix(apply( matrix( rep(  attr.prob, K ), ncol=K) * attr.patt, 2, sum ),ncol=1)
	rownames(skill.patt) <- paste("Skill_", colnames(q.matrix),sep="")
	colnames(skill.patt) <- "skill.prob" 
	
	# calculation of the AIC und BIC        
	bb <- 0
	if ( is.null( constraint.guess ) == F ){  bb <- bb + nrow(constraint.guess) }
	if ( is.null( constraint.slip ) == F ){  bb <- bb + nrow(constraint.slip) }
	aic <- -2*loglike + 2*( 2*J  - bb + ( L - 1)  )
	bic <- -2*loglike + ( 2*J - bb + ( L - 1 ) )*log(I)

	rownames( p.aj.xi ) <- rownames( pattern ) # output rownames posterior probabilities	
	pattern <- data.frame(pattern) # convert pattern to numeric format
	for (vv in seq(1,ncol(pattern))[ -c(2,4) ] ){
                    pattern[,vv ] <- as.numeric( paste( pattern[,vv] ) ) }
	
	# subject pattern
	item.patt.subj <- data.frame( "case" = 1:(nrow(data) ), 
    	                           "pattern" = item.patt.subj, 
        	                        "pattern.index" = match( item.patt.subj, rownames(pattern ) )
            	                            )
	
	# attribute pattern (expected frequencies)
	attr.prob <- data.frame( attr.prob )
	attr.prob$class.expfreq <-  attr.prob[,1] * nrow(data) 

	datfr <-  data.frame( round( cbind( guess , slip  ), 3 ) )
	colnames(datfr) <- c("guess", "se.guess", "slip", "se.slip" )
	rownames(datfr) <- colnames( dat.items )
	datfr <- data.frame( "type" = rule, datfr )
	
	cat("---------------------------------------------------------------------------------\n")
	res <- list( coef = datfr, guess = guess, slip = slip, loglike = loglike, AIC = aic, BIC = bic, 
       	         posterior = p.aj.xi, "like" = p.xi.aj, "data" = data, "q.matrix" = q.matrix,
           	     pattern = pattern , attribute.patt = attr.prob, skill.patt = skill.patt,
               	 "subj.pattern" = item.patt.subj, "attribute.patt.splitted" = attr.patt, "display" = disp,
				 "item.patt.split" = item.patt.split, "item.patt.freq" = item.patt.freq,
                 "model.type" = r1) 
	class(res) <- "din"
    return(res)
}
