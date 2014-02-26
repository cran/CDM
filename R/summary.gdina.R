

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
	cat( "Date of Analysis:" , paste( object$time$s2 ) , "\n" )
	cat("Computation Time:" , print(object$time$s2 - object$time$s1), "\n\n")	
    if (object$HOGDINA==-1){ 
		cat("Generalized DINA Model \n") } else {
		cat("Higher Order Generalized DINA Model \n") }
    if ( object$G > 1 ){ 
			cat("  Multiple Group Estmation with",object$G , "Groups \n") 
			# group statistics
			cat("\nGroup statistics\n")
			print( object$group.stat )				
			cat("\n")
						}
	cat( "\nNumber of iterations =" , object$iterused )
	cat( "\nIteration with minimal deviance =" , object$iter , "\n" )	
    cat( "Deviance =" , round( object$deviance , 2 ) ) 
	cat( "  | Loglikelihood =" , round( - object$deviance / 2 , 2 ) ,	"\n" )
    cat( "Number of persons =" , object$N , "\n" )    
	    cat( "Number of items =" , ncol(object$data) , "\n" )    
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
	r1 <- options()
	options(scipen=999)
	print(ds)
	options(scipen=r1$scipen)
    	if ( ! is.null( object$delta.designmatrix ) ){ 
			cat("\nNote: Standard errors are not (yet) correctly implemented!\n")
											}	

	cat("\nRMSEA Item Fit\n")											
	print( round( object$itemfit.rmsea,3) )
											
    cat("\nMean of RMSEA item fit:" , 
     round( object$mean.rmsea ,3 ) , "\n")											
	
	# RRUM model	
	if (object$rrum.model){
		cat("\n****\nRRUM Parametrization\n")
		print( round( object$rrum.params,3) , na  ="")
		cat("\n")
					}

	cat("----------------------------------------------------------------------------\n")
	cat("Model Implied Conditional Item Probabilities \n\n")
	obji <- object$probitem
	obji[,"prob"] <- round( obji$prob, 4 )	
	print(obji)					
	cat("----------------------------------------------------------------------------\n")
	cat("\nSkill Probabilities \n\n")
	print(round(object$skill.patt ,rdigits) )
	if ( ( object$G == 1 ) & (ncol(object$q.matrix ) > 1 ) & 
			max(object$NAttr ==1 ) ){ 
		cat("----------------------------------------------------------------------------\n")
		QM <- max(object$q.matrix)
		if (QM == 1){ 
			cat("\nTetrachoric Correlations \n\n")
			gt1 <- skill.cor( object )
				} else {
			cat("\nPolychoric Correlations \n\n")
			gt1 <- skill.polychor( object )				
				}
		print(round(gt1$cor.skills,3))
			}
	cat("\n----------------------------------------------------------------------------\n")	
	cat("\nSkill Pattern Probabilities \n\n")
	if ( object$G == 1 ){
		xt <- round( object$attribute.patt[,1] , rdigits )
		names(xt) <- rownames( object$attribute.patt )
			} else {
	xt <- round( object$attribute.patt , rdigits )
	rownames(xt) <- rownames( object$attribute.patt )
					}
	print(xt)

		if (object$HOGDINA>=0){
			cat("\n***************************\n")	
			cat("Higher Order GDINA Model ")
			cat("\n  Attribute Response Function Parameters \n\n")
			print( round( object$attr.rf,3) )
					}
		
		}
##########################################################################


#***************************************************************
# RRUM parametrization
.rrum.param <- function( delta.summary , q.matrix ){
	#---
	#  RRUM parametrization
	#  log( P(X=1) ) = b0 + b1*alpha1 + b2 * alpha2 
	#  RRUM:
	#  P(X=1) = pi * r1^( 1- alpha1) * r2^(1-alpha2)
	#  => log( P(X=1) ) = log[ pi * r1 * r2 * r1^(-alpha1) * r2^(-alpha2) ]
	#                   = log( pi ) + log(r1) + log(r2) + -log(r1)*alpha1 + -log(r2) * alpha2
	#  => b1 = -log(r1) and r1 = exp( -b1 )
	#  => log(pi) = b0 + b1 + b2 and pi = exp( b0 + b1 + b2 )
	I <- nrow(q.matrix)
	K <- ncol(q.matrix)
	rrum.params <- matrix( NA , I , K+1 )
	rownames(rrum.params) <- delta.summary[ delta.summary$partype == 0 , "item" ]
	colnames(rrum.params) <- c( "pi" , paste( "r_", colnames(q.matrix) , sep="") )
	for (ii in 1:I){
		# ii <- 2
		d.ii <- delta.summary[ delta.summary$itemno == ii , ]
		rrum.params[ii,"pi"] <- exp( sum( d.ii$est ) )
		rrum.params[ ii , which( q.matrix[ii,]==1) +1 ] <- exp( - d.ii$est[-1] )
				}
	return( rrum.params )
        }