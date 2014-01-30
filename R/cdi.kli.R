
#######################################################
# cognitive diagnostic indices
cdi.kli <- function( object ){
	# object must be of class din or gdina
	if ( ! ( class(object) %in% c("din","gdina") ) ){
		stop("This functions only supports objects of class din or gdina!")
					}
	items <- colnames( object$data )				
	q.matrix <- object$q.matrix
	pjk0 <- pjk <- object$pjk       # [ items , categories , skills ]
	skillclasses <- as.matrix( object$attribute.patt.splitted )
	# rearrange probabilities
	pjk <- aperm( pjk , c(1,3,2 ) )
	round( pjk[,,1] , 2 )
	dpjk <- dim(pjk)
	pjk <- matrix( pjk , nrow=dpjk[1] , ncol=dpjk[2]*dpjk[3] )
	round( pjk , 2 )

	eps <- 10^(-7) 	# prevent division by zero
	pjk <- ( pjk + eps ) / ( 1 + 2*eps )

	#*****
	# apply core Cpp function for calculation
#	res0 <-  cdm_kli_id( pjk , skillclasses )
	res0 <- .Call( "cdm_kli_id_C", 
					pjk , skillclasses , 
					PACKAGE = "CDM")	
	
	#****
	# arrange Kullback Leibler information
	kli <- array( res0$kli , dim=c( res0$TP , res0$TP , res0$I ) )
	kli <- aperm( kli , c(3,1,2) )

	# arrange output
	res <- list( "test_disc" =  sum(res0$glob_item) , "attr_disc" = colSums( res0$attr_item ) , 
				"glob_item_disc" = res0$glob_item , "attr_item_disc" = res0$attr_item ,
				"KLI" = kli , 
				"skillclasses" = res0$skillclasses , "hdist" = res0$hdist , "pjk" = pjk0 ,
				"q.matrix" = q.matrix )	
	# names
	names(res$attr_disc) <- colnames(res$attr_item_disc) <- colnames(q.matrix)
	dimnames(res$KLI)[[1]] <- items
	names(res$glob_item_disc) <- rownames(res$attr_item_disc) <- items	
	return(res)
	}
##############################################################