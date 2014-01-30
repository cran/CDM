###################################################
# initial estimate of item parameters delta
.mcdina.init.delta <- function( lc , lr ){
    I <- max( lc$item )
    CC <- max( lc$cats )
    delta_ideal <- delta <- array( 0 , dim=c(I , CC , CC ) )
    delta_ideal[ as.matrix( lc[ , c("item" , "cats" , "lr_index" ) ] ) ] <- 1
    eps <- 1E-10
    # define initial delta estimate
    for (ii in 1:I){
	    dii <- delta_ideal[ii,,]
		lcii <- lc[ lc$item == ii , ]
		Ncii <- nrow(lcii)
        for (cc in 1:CC){
            dii.cc <- dii[,cc]
            delta[ii,,cc] <- dii.cc * ( 0.8 / sum( dii.cc + eps) ) +
                                (1-dii.cc) * ( .2 / sum( ( 1-dii.cc) + eps ) )
			if (Ncii < CC ){ 
				delta[ii,seq(Ncii+1,CC),cc] <- 0
				delta[ii,,cc] <- delta[ii,,cc] / sum( delta[ii,,cc] )
							}
            if ( sum( dii.cc ) == 0 ){ delta[ii,,cc] <- 0 }
						
                        }
                    }
    res <- list( "delta" = delta , "delta_ideal" = delta_ideal )
    return(res)
        }
############################################################

##########################################
# preparation function for whole test
.mcdina.prep.test.latent.response <- 
function( q.matrix , K , TP , skillclasses ){
	I <- length( unique(q.matrix[,1]))
	lr <- NULL
	lc <- NULL
	itemstat <- NULL
    for (ii in 1:I){ 
	   res <- .mcdina.prep.item.latent.response( ii , q.matrix , 
				K , TP , skillclasses )
		lr <- rbind( lr , res$lr )
		lc <- rbind( lc , res$lc )
		itemstat <- rbind( itemstat , res$itemstat )
			}
	res <- list("lr"=lr , "lc"=lc , "itemstat"=itemstat)			
	return(res)
		}
###############################################		
		
##############################################
# compute preparation table for one item
.mcdina.prep.item.latent.response <- 
function( ii , q.matrix , K , TP , skillclasses ){
	q.ii <- q.matrix[ q.matrix[,1] == ii , ]
	classes <- rownames(skillclasses)
	# categories
	cats.ii <- q.ii[,2]
	CC <- length(cats.ii)
	# calculate relevant attributes
	qsum <- rowSums( q.ii[ , 1:K + 2  ] )
	index.max <- which( qsum == max(qsum) )
	# necessary attributes for item ii
	attr.ii <- which( q.ii[ index.max[1] , 1:K + 2] > 0 )
	q.ii.red <- q.ii[ , attr.ii + 2 , drop=FALSE]
	# calculate matrix with skill classes
	sk.ii1 <- sk.ii2 <- matrix( 0 , nrow=TP , ncol=CC)
	colnames(sk.ii1) <- colnames(sk.ii2) <- paste0("Cat" , cats.ii )
	rownames(sk.ii1) <- rownames(sk.ii2) <- rownames(skillclasses)
	for (cc in 1:CC){
		sk.ii2[ , cc] <- 1 * ( rowSums( skillclasses[ , attr.ii , drop=FALSE] != q.ii.red[rep(cc,TP) ,] ) == 0 )
		tmp1 <- skillclasses[ , attr.ii , drop=FALSE] %*% t( q.ii.red[cc,]  )
		sk.ii1[ , cc] <- 1 * ( tmp1 >=  sum( q.ii.red[cc ,] ) ) 
		sk.ii1[ , cc] <-  tmp1*sk.ii1[ , cc]
				}
	sk.ii1 <- 1 * ( sk.ii1 > 0 )
	v1.ii <- which( rowSums( sk.ii1 ) == 0 )
	i5 <- which( rowSums( q.ii.red ) == 0 )
	sk.ii1[ v1.ii , i5 ] <- 1
	ind.ii <- which( rowSums( sk.ii2 ) == 0 )
	sk.ii2[ind.ii , ] <- sk.ii1[ ind.ii , ]
	# define latent response groups
	lg <- "LR"
	for (cc in 1:CC){
		lg <- paste0( lg , ifelse( sk.ii2[,cc]==1 , cats.ii[cc] , "") )
				}
#	sk.ii3 <- cbind( sk.ii2 , lg )
	groups <- sort( unique(lg) )

	lr <- data.frame("item" = ii , "skillclass" = classes , 
		"skillclass_index" = 1:TP , "lr" = lg )
	lr$lr_index <- match( lr$lr , groups )
	# unique latent groups
	lg1 <- sapply( cats.ii , FUN = function(cc){ grep( cc , groups) } )
	lc <- data.frame("item"=ii , "cats"=cats.ii ,
				"lr"= groups[ lg1 ] )
	lc$max.cat <- 0
	lc$max.cat[ index.max ] <- 1
	lc$lr_index <- match( lc$lr , groups )
	lc$Q <- .matrixstring( q.ii[ , 1:K + 2  ] , "Q" )
	# item statistics
	itemstat <- data.frame("item" = ii , "N.cat" = CC ,
			"N.lr" = length(groups) )
	itemstat$N.attr <- length(attr.ii)
	res <- list("lr"=lr , "lc"=lc , "itemstat"=itemstat)			
	return(res)
		}
############################################################
# calculates a string pattern consisting of matrix entries
# matr <- skillclasses
# string <- "Q"
.matrixstring <- function( matr , string ){
	VV <- ncol(matr)
	l1 <- string
	for ( vv in 1:VV){
		l1 <- paste0( l1 , matr[,vv] )
				}
	return(l1)
		}
#################################################################